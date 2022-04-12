{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Categorifier.C.CTypes.Codegen.Render.Render
  ( RenderedFile (..),
    RenderedModule (..),
    renderModule,

    -- * For non-CxxTarget stuff like "Kitty.Codegen.Cxx.WrapKGenCFunction".
    renderModuleHeader,
    renderModuleSource,
    renderParamType,
    runCExpr,
    renderCTypeWithBackdoor,
    -- TODO(greg/guillaume): hide this! use function writer!
    passByReference,
  )
where

import Categorifier.C.CTypes.Codegen.Helpers (CStructOrUnion (..))
import Categorifier.C.CTypes.Codegen.Render.Typedefs (renderTypedef)
import Categorifier.C.CTypes.Codegen.Render.Utils
  ( indentWith,
    nolintNextLineReadabilityIdentifierNaming,
    toCommentText,
  )
import Categorifier.C.CTypes.Codegen.Sanitize (sanitizeText)
import Categorifier.C.CTypes.DSL.CxxAst
  ( BinOp (..),
    CExpr (..),
    CFunction (..),
    CTypeWithBackdoor (..),
    ConditionBlock (..),
    CxxModule (..),
    CxxOrC (..),
    CxxTarget (..),
    Define (..),
    ExternalType (..),
    Identifier (..),
    Include (..),
    Param (..),
    ParamType (..),
    SystemLib (..),
    TapeElement (..),
    TypeLevelFunctions (..),
    UnOp (..),
    UsingDecl (..),
    UsingDeclGroup (..),
  )
import Categorifier.C.CTypes.KTypeRep (ctypeNameFromKTypeRep)
import qualified Categorifier.C.CTypes.Render as R
import Categorifier.C.CTypes.Types
  ( CCon,
    CConF (..),
    CStructF (..),
    CTypeF (..),
    CUnionConF (..),
    CUnionF (..),
    CxxType (..),
    RfName,
    sanitizedRfName,
  )
import Categorifier.C.Recursion (hproject)
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import PyF (fmt)

moduleLanguage :: CxxTarget -> CxxOrC
moduleLanguage CTypes = C
moduleLanguage IsEqual = Cxx -- TODO(greg): could pretty easily split this in two
-- Imagine CxxAndCMixed type that split for you into 4 files.
moduleLanguage CxxTypes = Cxx
moduleLanguage CborEncode = Cxx
moduleLanguage CborDecode = Cxx
moduleLanguage DataLayout = Cxx
moduleLanguage EnumUtils = Cxx
moduleLanguage FromArrays = C -- xx
moduleLanguage PrintDifferences = Cxx
moduleLanguage ToArrays = C -- xx
moduleLanguage ToString = Cxx
moduleLanguage Dimensions = C

data RenderedFile = RenderedFile
  { rfName :: T.Text,
    rfContents :: T.Text
  }

data RenderedModule = RenderedModule
  { rmHeader :: RenderedFile,
    rmSource :: Maybe RenderedFile
  }

-- Make a header and an optional source.
renderModule :: CxxTarget -> CxxModule -> RenderedModule
renderModule target cxxModule =
  RenderedModule
    { rmHeader =
        RenderedFile
          { rfName = renderCxxTargetHeaderName target,
            rfContents = renderModuleHeader cxxOrC cxxModule
          },
      rmSource = case filter (not . cfStaticLinkage) (moduleFunctions cxxModule) of
        [] -> Nothing -- don't write source file if there are no non-static functions
        _ ->
          Just
            RenderedFile
              { rfName = renderCxxTargetSourceName target,
                rfContents = renderModuleSource cxxOrC cxxModule [IncludeModule target]
              }
    }
  where
    cxxOrC = moduleLanguage target

renderDefine :: Define -> T.Text
renderDefine (Define mcomment lhs rhs) = [fmt|{toCommentText mcomment}#define {lhs} {rhs}\n|]

renderMany :: (a -> T.Text) -> [a] -> T.Text
renderMany renderFun =
  let go (x : xs) = "\n" <> renderFun x <> go xs
      go [] = ""
   in go

-- | In C++, surround text with namespace kitty {}.
-- Do the same in C but guard the namespace inside "#ifdef __cplusplus".
withNamespaceKitty :: CxxOrC -> T.Text -> T.Text
withNamespaceKitty Cxx content =
  [fmt|\
namespace kitty {{
{indentWith "  " content}
}}  // namespace kitty
|]
withNamespaceKitty C content =
  [fmt|\
#ifdef __cplusplus
namespace kitty {{
#endif
{indentWith "  " content}
#ifdef __cplusplus
}}  // namespace kitty
#endif
|]

renderModuleSource :: CxxOrC -> CxxModule -> [Include] -> T.Text
renderModuleSource cxxOrC cxxModule extraIncludes =
  mconcat
    [ renderIncludes extraIncludes,
      withNamespaceKitty cxxOrC (renderMany (renderFunctionDef cxxOrC) functions)
    ]
  where
    CxxModule _includes _defines _typedefs _usingDecls functions _typeLevelFunctions = cxxModule

renderModuleHeader :: CxxOrC -> CxxModule -> T.Text
renderModuleHeader cxxOrC cxxModule =
  mconcat
    [ "#pragma once\n\n",
      renderIncludes includes,
      renderMany renderDefine defines,
      withNamespaceKitty cxxOrC $
        T.intercalate
          "\n"
          [ renderMany renderTypedef typedefs,
            renderMany renderUsingDeclGroup usingDecls,
            renderMany (renderFunctionDeclWithOverload cxxOrC) $
              filter (not . cfStaticLinkage) functions,
            renderMany renderTypeLevelFunctionGroup typeLevelFunctions
          ]
    ]
  where
    CxxModule includes defines typedefs usingDecls functions typeLevelFunctions = cxxModule

renderIncludes :: [Include] -> T.Text
renderIncludes [] = "// (No includes)\n\n"
renderIncludes includes = T.intercalate "\n" (renderInclude <$> includes) <> "\n\n"

renderInclude :: Include -> T.Text
renderInclude (IncludeModule target) =
  renderInclude
    . IncludeHeavisoftFile
    $ [fmt|avionics/flight_computer/kitty_protos/autogen/{renderCxxTargetHeaderName target}|]
renderInclude (IncludeHeavisoftFile path) = [fmt|#include "{path}"|]
renderInclude (IncludeSystemFile filename) = case M.lookup filename systemLibFilenameMap of
  Nothing -> [fmt|#include <{filename}>|]
  Just lib ->
    error
      [fmt|You specified `IncludeSystemFile "{filename}"`
when you should have used `IncludeSystemLib {show lib}`.
Doing it the second way adds C/C++ compatibility.
|]
renderInclude (IncludeSystemLib lib) =
  [fmt|\
#ifdef __cplusplus
#include <{systemLibCxxFile lib}>
#else
#include <{systemLibCFile lib}>
#endif
|]
renderInclude (IncludeThatNeedsDefines defines include) =
  foldMap renderDefine defines <> renderInclude include

systemLibCxxFile :: SystemLib -> T.Text
systemLibCxxFile Assert = "cassert"
systemLibCxxFile StdBool = "cstdbool"
systemLibCxxFile StdInt = "cstdint"
systemLibCxxFile Time = "ctime"
systemLibCxxFile Math = "cmath"
systemLibCxxFile Def = "cstddef"

systemLibCFile :: SystemLib -> T.Text
systemLibCFile Assert = "assert.h"
systemLibCFile StdBool = "stdbool.h"
systemLibCFile StdInt = "stdint.h"
systemLibCFile Time = "time.h"
systemLibCFile Math = "math.h"
systemLibCFile Def = "stddef.h"

systemLibFilenameMap :: M.Map T.Text SystemLib
systemLibFilenameMap =
  M.fromListWith const $
    concat [[(systemLibCFile lib, lib), (systemLibCxxFile lib, lib)] | lib <- enumFrom minBound]

renderCxxTargetHeaderName :: CxxTarget -> T.Text
renderCxxTargetHeaderName target = case moduleLanguage target of
  Cxx -> renderCxxTargetName target <> ".hpp"
  C -> renderCxxTargetName target <> ".h"

renderCxxTargetSourceName :: CxxTarget -> T.Text
renderCxxTargetSourceName target = case moduleLanguage target of
  Cxx -> renderCxxTargetName target <> ".cpp"
  C -> renderCxxTargetName target <> ".c"

renderCxxTargetName :: CxxTarget -> T.Text
renderCxxTargetName CTypes = "c_types"
renderCxxTargetName CxxTypes = "cxx_types"
renderCxxTargetName CborEncode = "cbor_encode"
renderCxxTargetName CborDecode = "cbor_decode"
renderCxxTargetName DataLayout = "data_layout"
renderCxxTargetName EnumUtils = "enum_utils"
renderCxxTargetName IsEqual = "is_equal"
renderCxxTargetName FromArrays = "from_arrays"
renderCxxTargetName PrintDifferences = "print_differences"
renderCxxTargetName ToArrays = "to_arrays"
renderCxxTargetName ToString = "to_string"
renderCxxTargetName Dimensions = "dimensions"

renderTypeLevelFunctionGroup :: TypeLevelFunctions -> T.Text
renderTypeLevelFunctionGroup tlf =
  [fmt|\
template<typename T>
struct {className} {{
  static constexpr {returnType} {functionName}();
}};

|]
    <> T.intercalate "\n" (renderTypeLevelFunctionInstance <$> tlfInstances tlf)
  where
    returnType = tlfReturnType tlf
    className = tlfClassName tlf
    functionName = tlfFunctionName tlf
    renderTypeLevelFunctionInstance :: (T.Text, T.Text) -> T.Text
    renderTypeLevelFunctionInstance (type', body) =
      [fmt|\
template <>
constexpr {returnType} {className}<{type'}>::{functionName}() {{
{indentWith "  " body}
}}
|]

renderFunctionPrototype :: CxxOrC -> Bool -> CFunction -> T.Text
renderFunctionPrototype cxxOrC isDecl fun = [fmt|{maybeStatic}{retType} {funName}({params})|]
  where
    maybeStatic :: T.Text
    maybeStatic
      | cfStaticLinkage fun = "static "
      | otherwise = ""
    Identifier funName = cfName fun
    params
      | null (cfParams fun) = ""
      | -- One line per type starting the line after the function name, 4-space continuation.
        otherwise =
          ("\n    " <>) $
            T.intercalate ",\n    " (renderParam cxxOrC isDecl <$> cfParams fun)
    retType = maybe "void" renderParamType (cfReturnType fun)

renderFunctionOverload :: CxxOrC -> Identifier -> CFunction -> T.Text
renderFunctionOverload cxxOrC (Identifier overloadName) fun
  | cfStaticLinkage fun = error "Function {funName} is both static and overloaded. This is illegal."
  | otherwise =
      [fmt|\
inline {retType} {overloadName}({params}) {{
  return {funName}({callParams});
}}
|]
  where
    Identifier funName = cfName fun
    params
      | null (cfParams fun) = ""
      | -- One line per type starting the line after the function name, 4-space continuation.
        otherwise =
          ("\n    " <>) $
            T.intercalate ",\n    " (renderParam cxxOrC False <$> cfParams fun)
    callParams = T.intercalate ", " (unIdentifier . pId <$> cfParams fun)
    retType = maybe "void" renderParamType (cfReturnType fun)

renderFunctionDecl :: CxxOrC -> CFunction -> T.Text
renderFunctionDecl cxxOrC fun =
  toCommentText (cfComment fun)
    <> ifDefCxxExternC cxxOrC
    <> nolintNextLineReadabilityIdentifierNaming
    <> "\n"
    <> renderFunctionPrototype cxxOrC True fun
    <> ";\n"

renderFunctionDeclWithOverload :: CxxOrC -> CFunction -> T.Text
renderFunctionDeclWithOverload cxxOrC fun
  | Just overloadName <- cfInlineOverloadName fun =
      [fmt|\
{decl}
{renderFunctionOverload cxxOrC overloadName fun}
|]
  | otherwise = decl
  where
    decl = renderFunctionDecl cxxOrC fun

ifDefCxxExternC :: CxxOrC -> T.Text
ifDefCxxExternC Cxx = ""
ifDefCxxExternC C =
  [fmt|\
#ifdef __cplusplus
extern "C"
#endif
|]

-- Check if a list of tape element only contains a bunch of x++ operators
-- This is used later in codegen to generate a for instead of a
-- while.
isOnlyPlusPlus :: [TapeElement] -> Maybe [CExpr]
isOnlyPlusPlus [] = Just []
isOnlyPlusPlus (t : xs) = case t of
  PlusPlus e -> (e :) <$> isOnlyPlusPlus xs
  _ -> Nothing

renderCTypeWithBackdoor :: CTypeWithBackdoor -> T.Text
renderCTypeWithBackdoor = \case
  NewCType c -> R.renderCType c
  CTypeBackdoor t -> t

renderFunctionDef :: CxxOrC -> CFunction -> T.Text
renderFunctionDef cxxOrC fun =
  [fmt|
{maybeComment}{nolintNextLineReadabilityIdentifierNaming}
{renderFunctionPrototype cxxOrC False fun} {{
{tapeElements}}}
|]
  where
    maybeComment = toCommentText (cfComment fun)
    tapeElements =
      T.intercalate "\n" . fmap (indentWith "  " . renderTapeElement) . F.toList $ cfTape fun

renderTapeElement :: TapeElement -> T.Text
renderTapeElement (PlusPlus e) =
  [fmt|++({runCExpr e});
|]
renderTapeElement (UnionCast typeFrom typeTo exprFrom identifierTo) =
  [fmt|
{renderCTypeWithBackdoor typeTo} {unIdentifier identifierTo};
{{
  union
  {{
     {renderCTypeWithBackdoor typeTo} fieldTo;
     {renderCTypeWithBackdoor typeFrom} fieldFrom;
  }} convertor;

  convertor.fieldFrom = {runCExpr exprFrom};
  {unIdentifier identifierTo} = convertor.fieldTo;
}}
|]
renderTapeElement (FunctionCall cfunction params _)
  | -- length check
    length params /= length (cfParams cfunction) =
      error $
        T.unpack
          [fmt|\
param length mismatch in function {show (cfName cfunction)}
length (cfParams cfunction): {length (cfParams cfunction)}
length of callExprs: {length params}

cfParams cfunction:
{T.intercalate "\n" (T.pack . show <$> cfParams cfunction)}

callExprs:
{T.intercalate "\n" (T.pack . show <$> params)}
|]
renderTapeElement (FunctionCall cfunction params mcomment) =
  let Identifier funName = cfName cfunction
      callExprs
        | null params = ""
        | -- One line per type starting the line after the function name, 4-space continuation.
          -- TODO(greg): remove code duplication
          otherwise =
            "\n    " <> T.intercalate ",\n    " (fmap runCExpr params)
   in [fmt|{toCommentText mcomment}{funName}({callExprs});
|]
renderTapeElement (TapeBackdoor x) = x
renderTapeElement (SwitchCase caseTypeName expr cases defaultCase) =
  let writeOneCase :: (T.Text, [TapeElement]) -> T.Text
      writeOneCase (k, tape) =
        let conTape = T.intercalate "\n" (renderTapeElement <$> tape)
         in [fmt|\
case {k}: {{
{indentWith "  " conTape}
  break;
}}
|]
      switchCases = T.intercalate "\n" (fmap writeOneCase cases)
      defaultCase' = T.intercalate "\n" (fmap renderTapeElement defaultCase)
      otherDefault = T.intercalate "\n" $ case caseTypeName of
        Nothing -> []
        Just tName ->
          fmap
            (\x -> [fmt|case {x}:|])
            [ R.renderEnumForceSignedLiteralWithTypeName tName,
              R.renderEnumNumFieldsLiteralWithTypeName tName
            ]
   in [fmt|\
switch ({runCExpr expr}) {{
{indentWith "  " switchCases}\
{indentWith "  " otherDefault}
  default:
{indentWith "  " defaultCase'}
}}
|]
renderTapeElement (CUnionConSwitch (TakeAddress e) cunion@(CUnion _ ccons _ _) conFun) =
  let switchCases = T.intercalate "\n" (writeSwitchCase <$> V.toList ccons)
      writeSwitchCase :: CCon Proxy -> T.Text
      writeSwitchCase ccon =
        let conExpr = AsUnionCon (TakeAddress e) ccon
            conTape = T.intercalate "\n" (renderTapeElement <$> conFun ccon conExpr)
         in [fmt|\
case {R.renderCUnionTagLiteral cunion ccon}: {{
{indentWith "  " conTape}
  break;
}}
|]
   in [fmt|\
switch ({runCExpr e}.{R.renderCUnionTagMember cunion}) {{
{indentWith "  " switchCases}\
  case {R.renderEnumForceSignedLiteralWithTypeName (R.renderCUnionTagType cunion)}:
  case {R.renderEnumNumFieldsLiteralWithTypeName (R.renderCUnionTagType cunion)}:
  default:
  assert(false);

}}
|]
renderTapeElement (CUnionConSwitch expr cunion@(CUnion _ ccons _ _) conFun) =
  let switchCases = T.intercalate "\n" (writeSwitchCase <$> V.toList ccons)
      writeSwitchCase :: CCon Proxy -> T.Text
      writeSwitchCase ccon =
        let conExpr = AsUnionCon expr ccon
            conTape = T.intercalate "\n" (renderTapeElement <$> conFun ccon conExpr)
         in [fmt|\
case {R.renderCUnionTagLiteral cunion ccon}: {{
{indentWith "  " conTape}
  break;
}}
|]
   in [fmt|
switch (({runCExpr expr})->{R.renderCUnionTagMember cunion}) {{
{indentWith "  " switchCases}\
  case {R.renderEnumForceSignedLiteralWithTypeName (R.renderCUnionTagType cunion)}:
  case {R.renderEnumNumFieldsLiteralWithTypeName (R.renderCUnionTagType cunion)}:
  default:
  assert(false);

}}
|]
-- This version turns a simplified ForLoop into a real forloop
-- That's because the c++ for operator only accept expressions
-- (not statement), but our DSL accept any statement in the step of
-- the for loop.
-- If we detect only expression (e.g. only ++), we can use a for, else
-- we use a while
-- Next case will generate a generic one using while
renderTapeElement
  (ForLoop [Declare t identifier exprInit] cond (isOnlyPlusPlus -> Just steps) body) =
    let renderedBody = T.intercalate "\n" (fmap renderTapeElement body)
        renderedSteps = T.intercalate ", " (fmap (\e -> [fmt|++{runCExpr e}|]) steps)
     in [fmt|\
for ({renderCTypeWithBackdoor t} {unIdentifier identifier} = {runCExpr exprInit}; \
{runCExpr cond}; {renderedSteps}) {{
{indentWith "  " renderedBody}}}
|]
renderTapeElement (ForLoop [] cond (isOnlyPlusPlus -> Just steps) body) =
  let renderedBody = T.intercalate "\n" (fmap renderTapeElement body)
      renderedSteps = T.intercalate ", " (fmap (\e -> [fmt|++{runCExpr e}|]) steps)
   in [fmt|\
for (; {runCExpr cond}; {renderedSteps}) {{
{indentWith "  " renderedBody}}}
|]
renderTapeElement (ForLoop start cond step body) =
  let renderedInit = T.intercalate "\n" (fmap renderTapeElement start)
      renderedStep = T.intercalate "\n" (fmap renderTapeElement step)
      renderedBody = T.intercalate "\n" (fmap renderTapeElement body)
   in [fmt|\
{renderedInit}
while({runCExpr cond})
{{
{indentWith "  " renderedBody}
{indentWith "  " renderedStep}
}}
|]
renderTapeElement (ForCxx t identifier expr body) =
  let renderedBody = T.intercalate "\n" (fmap renderTapeElement body)
   in [fmt|\
for (const {renderCTypeWithBackdoor t} &{unIdentifier identifier} : {runCExpr expr}) {{
{indentWith "  " renderedBody}}}
|]
renderTapeElement (Return expr) =
  [fmt|return {runCExpr expr};
|]
renderTapeElement ReturnVoid =
  [fmt|return;
|]
renderTapeElement (Comment' c) = T.unlines . fmap ("// " <>) $ T.lines c
renderTapeElement (ForceExpr expr) =
  [fmt|{runCExpr expr};
|]
renderTapeElement (Declare newt identifier expr) =
  let -- compact a cascade of if () else { if () else {}} to one If

      -- Operator precedence are taken
      -- from https://en.cppreference.com/w/cpp/language/operator_precedence
      -- We mostly use left to right binary operators, so precedence is done left to right
      -- TODO: in some cases, to match with the previous DSL, operator
      -- precedance is not taken into account and parenthesis are
      -- unconditionally used.

      -- TODO: AsUnionCon: always parenthesed, this can be improved
      -- Special case for AsUnionCon if the value is an address
      -- This exists just to help with the conversion from the old DSL

      -- TODO: show is used for escaping.

      -- Special case for function of more than 6 arguments
      -- Help conversion from the old DSL
      -- This also generates a multi line call which is more readable if
      -- there are a lot of arguments

      -- Special case for CStyleCast, where the operand is surrounded by parenthesis

      -- Special case when Or have a left or right operand which is an And
      -- This way, (x :|| a :&& b) will be rendered as (x :|| (a :&& b))

      -- We are in a Or operation, force parenthesis if the left or right operand is a And

      -- Special case when And have a binary operand as right or left child
      -- This way (a == b :&& c) will be rendered as ((a == b) :&& c)

      -- We are in a binary operation, force parenthesis if the left or right operand is a And

      -- Special case for array subscript, where the second operand is
      -- between squared braquet

      -- detect if the identifier is used in the function body

      -- All arguments are const references

      -- TODO: implement operator precedence for it

      -- In definitions, add __attribute__((unused)).

      -- In declarations, don't change anything.

      -- Arrays have special syntax. TODO(greg): arrays of arrays wrong here.

      -- anything mutable pass by pointer

      -- immutable things mostly still passed by pointer

      -- some immutable things passed by value

      -- same length for all reps?

   in [fmt|{renderCTypeWithBackdoor newt} {unIdentifier identifier} = {runCExpr expr};
|]
renderTapeElement (DeclareDefault newt identifier) =
  let
   in [fmt|{renderCTypeWithBackdoor newt} {unIdentifier identifier};
|]
renderTapeElement (DeclareCxxDefault newt identifier) =
  let
   in [fmt|{renderCTypeWithBackdoor newt} {unIdentifier identifier}{{}};
|]
renderTapeElement (Assign lhs rhs) =
  [fmt|{runCExpr lhs} = {runCExpr rhs};
|]
renderTapeElement (If cblocks (Just [If cblocks' elseBlock])) =
  renderTapeElement (If (cblocks <> cblocks') elseBlock)
renderTapeElement (If cblocks elseBlock) =
  T.intercalate " else " (fmap renderConditionBlock cblocks) <> case elseBlock of
    Nothing -> "\n"
    Just elements ->
      [fmt| else {{
{indentWith "  " renderedBody}}}
|]
      where
        renderedBody = T.intercalate "" (fmap renderTapeElement elements)

renderConditionBlock :: ConditionBlock -> T.Text
renderConditionBlock (ConditionBlock condition block) =
  [fmt|\
if ({runCExpr condition}) {{
{indentWith "  " renderedBody}}}|]
  where
    renderedBody = T.intercalate "" (fmap renderTapeElement block)

parens :: Bool -> T.Text -> T.Text
parens True t = [fmt|({t})|]
parens False t = t

runCExpr :: CExpr -> T.Text
runCExpr = runCExprPrec 20

-- | The maximum C++ precedance is 19, so 20 is more
maxCxxPrec :: Int
maxCxxPrec = 20

functionCallPrecedence :: Int
functionCallPrecedence = 2

unaryOperatorPrecedence :: UnOp -> Int
unaryOperatorPrecedence = \case
  Not' -> 3
  IsNan' -> 2
  IsInf' -> 2
  Dereference -> 3
  Address -> 3
  FieldByDot _ -> 2
  FieldByPtr _ -> 2
  CStyleCast _ -> 3

binaryOperatorPrecedence :: BinOp -> Int
binaryOperatorPrecedence = \case
  LessThan -> 9
  Equal -> 10
  NotEqual -> 10
  GreaterThan -> 9
  LessThanEqual -> 9
  GreaterThanEqual -> 9
  And -> 14
  Or -> 15
  Add -> 6
  Sub -> 6
  Mul -> 5
  Quot -> 5
  LeftShift -> 7
  Subscript -> 2

-- * Operator (binary/unary) formatting

-- | Format an unary operator over an expression already formatted
formatUnaryOperator :: UnOp -> T.Text -> T.Text
formatUnaryOperator = \case
  Not' -> formatPrefix "!"
  IsNan' -> \e -> [fmt|isnan({e})|]
  IsInf' -> \e -> [fmt|isinf({e})|]
  Dereference -> formatPrefix "*"
  Address -> formatPrefix "&"
  FieldByDot field -> formatField "." field
  FieldByPtr field -> formatField "->" field
  CStyleCast t -> \e -> [fmt|({R.renderCType t})({e})|]
  where
    formatPrefix :: T.Text -> T.Text -> T.Text
    formatPrefix o e = [fmt|{o}{e}|]
    formatField :: T.Text -> RfName -> T.Text -> T.Text
    formatField o field e = [fmt|{e}{o}{sanitizedRfName field}|]

-- | Format a binary operator over two expressions already formatted
formatBinaryOperator :: BinOp -> T.Text -> T.Text -> T.Text
formatBinaryOperator = \case
  LessThan -> leftRight "<"
  Equal -> leftRight "=="
  NotEqual -> leftRight "!="
  GreaterThan -> leftRight ">"
  LessThanEqual -> leftRight "<="
  GreaterThanEqual -> leftRight ">="
  And -> leftRight "&&"
  Or -> leftRight "||"
  Add -> leftRight "+"
  Sub -> leftRight "-"
  Mul -> leftRight "*"
  Quot -> leftRight "/"
  LeftShift -> leftRight "<<"
  Subscript -> \a b -> [fmt|{a}[{b}]|]
  where
    leftRight :: T.Text -> T.Text -> T.Text -> T.Text
    leftRight m a b = [fmt|{a} {m} {b}|]

hasLambda :: [CExpr] -> Bool
hasLambda (Lambda {} : _) = True
hasLambda _ = False

runCExprPrec :: Int -> CExpr -> T.Text
runCExprPrec _prec (Ident (Identifier name)) = name
runCExprPrec _prec (AsUnionCon (TakeAddress e) ccon) =
  [fmt|{runCExpr e}.{R.renderCUnionMemberName ccon}|]
runCExprPrec prec (AsUnionCon expr ccon) =
  [fmt|{parens (True) $ runCExprPrec prec expr}->{R.renderCUnionMemberName ccon}|]
runCExprPrec _prec (LiteralInt i) = [fmt|{i}|]
runCExprPrec _prec (LiteralBool b) = if b then "true" else "false"
runCExprPrec _prec (LiteralString i) = [fmt|{show i}|]
runCExprPrec _prec (LiteralChar c) = [fmt|{show c}|]
runCExprPrec prec (CallFunction name args) =
  let (startArgs, sep, indent, endArgs)
        | hasLambda args = ("\n" :: T.Text, ",\n", "  ", "\n" :: T.Text)
        | length args > 6 = ("\n    " :: T.Text, ",\n    ", "", "")
        | otherwise = ("", ", ", "", "")
      rArgs = T.intercalate sep (indentWith indent . runCExprPrec maxCxxPrec <$> args)
   in [fmt|{parens (prec < functionCallPrecedence)
         $ runCExprPrec prec name}({startArgs}{rArgs}{endArgs})|]
runCExprPrec prec (UnaryOp op e) =
  parens
    (prec < opPrec)
    (formatUnaryOperator op (runCExprPrec opPrecRecursive e))
  where
    opPrec = unaryOperatorPrecedence op

    opPrecRecursive = case op of
      CStyleCast _ -> maxCxxPrec
      _ -> opPrec
runCExprPrec prec (BinaryOp op e e') = parens (prec < opPrec) $ case op of
  Or -> formatBinaryOperator op (trampolineOr opPrec e) (trampolineOr opPrecRight e')
    where
      trampolineOr _opPrec expr@(BinaryOp And _ _) = parens True (runCExprPrec maxCxxPrec expr)
      trampolineOr opPrec' expr = runCExprPrec opPrec' expr
  And -> formatBinaryOperator op (trampolineAnd opPrec e) (trampolineAnd opPrecRight e')
    where
      trampolineAnd _opPrec expr@BinaryOp {} = parens True (runCExprPrec maxCxxPrec expr)
      trampolineAnd opPrec' expr = runCExprPrec opPrec' expr
  _ -> formatBinaryOperator op (runCExprPrec opPrec e) (runCExprPrec opPrecRight e')
  where
    opPrecRight = case op of
      Subscript -> maxCxxPrec
      _ -> opPrec
    opPrec = binaryOperatorPrecedence op
runCExprPrec _prec (FunctionExpr f) = [fmt|{unIdentifier $ cfName f}|]
runCExprPrec _prec (StaticCast t e) =
  parens False [fmt|static_cast<{renderCTypeWithBackdoor t}>({runCExprPrec maxCxxPrec e})|]
runCExprPrec _prec (Lambda captures args body) =
  [fmt|\
[{capture}]({arg}) {{
{indentWith "    " renderedBody}}}|]
  where
    renderedBody = T.intercalate "" (fmap renderTapeElement body)

    displayIdent (Identifier ident)
      | ident `T.isInfixOf` renderedBody = ident
      | otherwise = ""

    arg =
      T.intercalate
        ", "
        (fmap (\(t, ident) -> [fmt|const {R.renderCxxUnionConType t} &{displayIdent ident}|]) args)
    capture = "&" <> T.intercalate ", &" (fmap unIdentifier captures)
runCExprPrec _ (IfThenElse cond exprTrue exprFalse) =
  [fmt|(({runCExpr cond}) ? ({runCExpr exprTrue}) : ({runCExpr exprFalse}))|]

-- | TODO(greg/guillaume): I don't think this is right.
-- It does not correctly handle C arrays.
renderParamType :: ParamType -> T.Text
renderParamType (ParamCxxType cxxType) = R.renderCxxType cxxType
renderParamType (ParamCUnionCon unionCon) = R.renderCUnionConType unionCon
renderParamType (ParamCUnionTag cunion) = R.renderCUnionTagType cunion
renderParamType (ParamCxxUnionCon unionCon) = R.renderCxxUnionConType unionCon
renderParamType (ParamExternalType (ExternalType x)) = x

renderParam :: CxxOrC -> Bool -> Param -> T.Text
renderParam cxxOrC decl (Param paramType (Identifier name) unused mutable)
  | unused && not decl =
      paramDecl <> " __attribute__((unused))"
  | otherwise =
      paramDecl
  where
    paramDecl = case paramType of
      ParamCxxType (CxxTypeCType (CTypeArray _ elemType _))
        | mutable -> [fmt|{R.renderCType $ hproject elemType} * const {name}|]
        | otherwise -> [fmt|const {R.renderCType $ hproject elemType} * const {name}|]
      _
        | mutable ->
            renderParamType paramType <> " * const " <> name
        | passByReference paramType -> case cxxOrC of
            C -> "const " <> renderParamType paramType <> " * const " <> name
            Cxx -> "const " <> renderParamType paramType <> " &" <> name
        | otherwise ->
            "const " <> renderParamType paramType <> " " <> name

passByReference :: ParamType -> Bool
passByReference (ParamCxxType (CxxTypeCType (CTypePrim _))) = False
passByReference (ParamCxxType (CxxTypeCType (CTypeEnum _))) = False
passByReference (ParamCxxType (CxxTypeCType (CTypeStruct (CStruct _ CNullaryCon {})))) = True
passByReference (ParamCxxType (CxxTypeCType (CTypeStruct (CStruct _ CBitfieldCon {})))) = False
passByReference (ParamCUnionCon (CUnionCon _ CNullaryCon {})) = True
passByReference (ParamCUnionCon (CUnionCon _ CBitfieldCon {})) = False
passByReference ParamCUnionTag {} = False
passByReference _ = True

renderUsingDeclGroup :: UsingDeclGroup -> T.Text
renderUsingDeclGroup (UsingDeclGroup tyCon []) = [fmt|// All entries filtered for {show tyCon}|]
renderUsingDeclGroup (UsingDeclGroup tyCon (UsingDecl reps0 _ : others))
  | any differentLength others = [fmt|// {show tyCon} has inconsistent type apps lengths|]
  where
    n = length reps0
    differentLength (UsingDecl reps _) = length reps /= n
renderUsingDeclGroup (UsingDeclGroup tyCon (UsingDecl [] _ : _)) =
  [fmt|// {show tyCon} has no type apps, don't need a family|]
renderUsingDeclGroup (UsingDeclGroup tyCon entries@(UsingDecl apps0 _ : _)) =
  [fmt|
// ***************** family for {show tyCon} ***************
// base class
template{typenameList}
{nolintNextLineReadabilityIdentifierNaming}
struct wrap_{sanitizedTyCon} {{
}};

{T.intercalate "\n" (fmap renderUsingDecl entries)}

// Cute "using" wrapper
template{typenameList}
{nolintNextLineReadabilityIdentifierNaming}
using {sanitizedTyCon} = typename wrap_{sanitizedTyCon}{typevarList}::type;
|]
  where
    n = length apps0
    typeVars :: [T.Text]
    typeVars
      | n == 1 = ["T"]
      | otherwise = take n $ fmap (\k -> [fmt|T{k}|]) [(0 :: Int) ..]
    typenameList :: T.Text
    typenameList = spaceRightBrackets [fmt|<{T.intercalate ", " $ fmap ("typename " <>) typeVars}>|]
    typevarList :: T.Text
    typevarList = spaceRightBrackets [fmt|<{T.intercalate ", " typeVars}>|]
    sanitizedTyCon = sanitizeText (T.pack (show tyCon))
    renderUsingDecl :: UsingDecl -> T.Text
    renderUsingDecl (UsingDecl reps cstructOrUnion) =
      let sanitizedTypeReps = fmap ctypeNameFromKTypeRep reps
          sanitizedTypeRepList = spaceRightBrackets [fmt|<{T.intercalate ", " sanitizedTypeReps}>|]
          renderedType = case cstructOrUnion of
            CS x -> R.renderCStructType x
            CU x -> R.renderCUnionType x
       in [fmt|\
// UsingDecl for {renderedType}
template<>
{nolintNextLineReadabilityIdentifierNaming}
struct wrap_{sanitizedTyCon}{sanitizedTypeRepList} {{
  {nolintNextLineReadabilityIdentifierNaming}
  typedef {renderedType} type;
}};
|]

spaceRightBrackets :: T.Text -> T.Text
spaceRightBrackets x0
  | x1 == x0 = x1
  | otherwise = spaceRightBrackets x1
  where
    x1 = T.replace ">>" "> >" x0
