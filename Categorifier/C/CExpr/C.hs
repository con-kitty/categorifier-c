{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | C-code emission from CExpr graphs -- this module contains the low-level functionality
-- for turning AST fragments into pretty-printer fragments.
module Categorifier.C.CExpr.C
  ( -- * The assignment monad
    genAssignment,
    genOutputAssignment,
    AssignVar,
    AssignTypeError (..),
    GenError (..),
    GenOperationError (..),

    -- * Function generation
    genFunctionDeclaration,
    genFunctionDefinition,
    wrapWithExternC,
  )
where

import qualified Barbies
import Categorifier.C.CExpr.C.Assignment
  ( AssignState (..),
    AssignTypeError (..),
    AssignVar,
    prettyAssignVar,
  )
import Categorifier.C.CExpr.C.Operations
  ( genBoolBinOp,
    genBoolUnOp,
    genCast,
    genCastFP,
    genCmpOp,
    genDoubleBinOp,
    genDoubleUnOp,
    genFPTestOp,
    genFloatBinOp,
    genFloatUnOp,
    genInput,
    genIntBinOp,
    genIntSetBit,
    genIntTestOp,
    genIntUnOp,
    genKBranch,
    genOutput,
    genPrimitive,
    genRoundFP,
    genTypeName,
  )
import Categorifier.C.CExpr.C.Pretty
  ( assignStatement,
    cFunArgs,
    constAssignStatement,
    indexArray,
    newline,
    ternary,
  )
import Categorifier.C.CExpr.Types (CmpOp (..))
import Categorifier.C.CExpr.Types.Core
  ( CExprF (..),
    Callee (..),
    FunctionCall (FunctionCall),
    Select (Select),
  )
import Categorifier.C.Prim
  ( Arrays,
    IsPrimitive (primGADT, toPrim),
    PrimGADT (..),
    PrimType,
  )
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as Doc

encloseSepIndent :: Int -> Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseSepIndent _ l r _ [] = l <> r
encloseSepIndent ind l r app ds = l <> Doc.indent ind (Doc.vcat $ fmap (<> app) ds) <> r

{- Function calls -}

-- | Generate a C function call to another code-generated function.  All functions have the same
--   calling convention (one input array per primitive; one output array per primitive).  All we do
--   is:
--
-- 1. allocate all output arrays based on the known sizes
-- 2. open a new anonymous scope
-- 3. allocate and initialize const input arrays from previous values in the function scope
-- 4. call the function
-- 5. close the anonymous scope, removing all the input arrays
genFunctionCall ::
  forall ann. AssignVar -> FunctionCall (Const (Doc ann)) -> Doc ann
genFunctionCall vn (FunctionCall funName inputVars callee) =
  functionCallComment
    <> outAlloc -- Comment the function call variable
    <> "{" -- Allocate output arrays
    <> Doc.line
    <> Doc.indent -- Open a new scope to contain the input arrays
      2
      ( inAlloc
          <> Doc.pretty funName -- Allocate the inputs
          <> funArgs
          <> ";"
      )
    <> Doc.line
    <> "}" -- Call the function
    <> Doc.line
    <> functionCallEndComment -- Close the input scope
    -- Add a comment that the call is done.
  where
    inAlloc = Barbies.bfoldMapC @IsPrimitive (allocateInputArray vn) inputVars
    outAlloc = case callee of
      Generated graph -> Barbies.bfoldMapC @IsPrimitive (allocateOutputArray vn) graph
      Imported outspec _ -> Barbies.bfoldMapC @IsPrimitive (allocateOutputArray' vn) outspec
      HandWritten outspec _ _ _ -> Barbies.bfoldMapC @IsPrimitive (allocateOutputArray' vn) outspec
    funArgs =
      cFunArgs $
        Barbies.bfoldMapC @IsPrimitive (genArrayName vn "input") inputVars
          <> ( case callee of
                 Generated graph ->
                   Barbies.bfoldMapC @IsPrimitive (genArrayName vn "output") graph
                 Imported outspec _ ->
                   Barbies.bfoldMapC @IsPrimitive (genArrayName vn "output") outspec
                 HandWritten outspec _ _ _ ->
                   Barbies.bfoldMapC @IsPrimitive (genArrayName vn "output") outspec
             )
    functionCallComment =
      "/* Function call to '"
        <> Doc.pretty funName
        <> "()' is '"
        <> prettyAssignVar vn
        <> "' */"
        <> Doc.line
    functionCallEndComment =
      "/* End of function call to '"
        <> Doc.pretty funName
        <> "()' (i.e. '"
        <> prettyAssignVar vn
        <> "') */"
        <> Doc.line

    -- Generate the assignment statement for a 'Compose' of variables of this type.
    allocateInputArray ::
      forall a. IsPrimitive a => AssignVar -> Compose Vector (Const (Doc ann)) a -> Doc ann
    allocateInputArray varNum =
      ("const" <+>)
        . allocateArray @a
          varNum
          "input"
          (("[] =" <+>) . arrayBraces . toList . fmap getConst . getCompose)
    -- Generate the allocation statement for a 'Vector' whose length is the size of the output array
    -- of this type.
    allocateOutputArray :: forall a f. IsPrimitive a => AssignVar -> Compose Vector f a -> Doc ann
    allocateOutputArray varNum =
      allocateArray @a varNum "output" (Doc.brackets . Doc.pretty . Vector.length . getCompose)

    -- Generate the allocation statement from the output spec
    allocateOutputArray' :: forall a. IsPrimitive a => AssignVar -> Const Int a -> Doc ann
    allocateOutputArray' varNum =
      allocateArray @a varNum "output" (Doc.brackets . Doc.pretty . getConst)

    allocateArray ::
      forall a b.
      IsPrimitive a =>
      AssignVar ->
      Doc ann ->
      (b -> Doc ann) ->
      b ->
      Doc ann
    allocateArray varNum lbl cont contIn =
      let typeName = genTypeName @a
       in typeName <+> funCallArrayName lbl varNum typeName <> cont contIn <> newline
    -- Just get the name of the array, used to compute function call arguments.  Can be
    -- 'Barbies.bfoldMap'ed on any 'Arrays'; it only uses the field type @a@ to get the type name.
    genArrayName :: forall a v. IsPrimitive a => AssignVar -> Doc ann -> v a -> [Doc ann]
    genArrayName varNum lbl _ =
      let typeName = genTypeName @a
       in pure $ funCallArrayName lbl varNum typeName
    arrayBraces = Doc.group . Doc.encloseSep "{" "}" ", "

funCallArrayName ::
  -- | Array label ("output" or "input")
  Doc ann ->
  -- | Variable for this set of arrays
  AssignVar ->
  -- | Type name
  Doc ann ->
  -- | Name of the array
  Doc ann
funCallArrayName lbl varNum ty = lbl <> "_" <> prettyAssignVar varNum <> "_" <> ty

genFunctionOutputRef :: forall a ann. IsPrimitive a => Int -> AssignVar -> Doc ann
genFunctionOutputRef idx varNum =
  funCallArrayName "output" varNum (genTypeName @a) <> Doc.brackets (Doc.pretty idx)

{- Select -}

anonStructMemberName :: forall a ann. IsPrimitive a => Doc ann
anonStructMemberName = genTypeName @a <> "s"

arraysToStructMembers :: forall ann. Arrays (Const Int) -> Doc ann
arraysToStructMembers = Barbies.bfoldMapC @IsPrimitive go
  where
    go :: forall a. IsPrimitive a => Const Int a -> Doc ann
    go (Const n) =
      let typeName = genTypeName @a
       in typeName <+> anonStructMemberName @a <> Doc.brackets (Doc.pretty n) <> newline

-- | The generation strategy here is due to Greg:
--
--    1. declare an anonymous struct matching the type over which we want to select
--    2. initialize inline an array of @const@ input structures corresponding to the selection
--       choices
--    3. use a ternary operator to map out-of-bounds indices to the first input
--    4. initialize inline a single @const@ value of this structure for the output, and assign to it
--       by value the indexed element of the input array.
--
-- This keeps everything totally @const@ and allows a similar result extraction strategy to function
-- calls, where every resulting scalar can be described by a static access into the output struct.
genSelect ::
  forall ann. AssignVar -> Select (Const AssignState) -> AssignState -> Doc ann
genSelect vn' input@(Select argSpec choices) index =
  -- const struct { <fields> }
  anonStructPrelude
    <+>
    -- inputs[] = { <initialization> }
    inputsName
    <> "[]"
    <+> "="
    <+> inputInits
    <>
    -- , outputs = idx < count ? inputs[idx] : inputs[0];
    ","
    <+> outputsName
    <+> "="
    <+> selection
    <> newline
  where
    vn = prettyAssignVar vn'
    indexVar = prettyRef index
    Select _ choiceVars = Barbies.bmap (first prettyRef) input
    selection = indexOutputs vn
    inputInits = bracketList . toList $ fmap initializeArrays choiceVars
    inputsName = mkInputsName vn
    outputsName = mkOutputsName vn
    initializeArray ::
      forall a. IsPrimitive a => Compose Vector (Const (Doc ann)) a -> Const (Doc ann) a
    initializeArray (Compose v) =
      Const $
        "."
          <> anonStructMemberName @a
          <+> "="
          <+> (bracketList . fmap getConst $ toList v)
    initializeArrays :: Arrays (Compose Vector (Const (Doc ann))) -> Doc ann
    initializeArrays =
      bracketList . Barbies.bfoldMap (pure . getConst)
        . Barbies.bmapC @IsPrimitive initializeArray
    indexOutputs :: Doc ann -> Doc ann
    indexOutputs thisVar =
      let guardExpr = genCmpOp CmpLT indexVar . Doc.pretty $ length choices
       in ternary
            guardExpr
            (indexArray (mkInputsName thisVar) indexVar)
            (indexArray (mkInputsName thisVar) "0")
    anonStructPrelude = "const struct" <+> Doc.enclose "{" "}" (arraysToStructMembers argSpec)
    mkInputsName varName = "inputs_" <> varName
    mkOutputsName varName = "outputs_" <> varName
    bracketList = Doc.group . Doc.encloseSep "{" "}" ", "

selectMemberName ::
  forall a ann.
  IsPrimitive a =>
  -- | Variable for this struct
  AssignVar ->
  -- | Name of the array
  Doc ann
selectMemberName varNum = "outputs_" <> prettyAssignVar varNum <> "." <> anonStructMemberName @a

genSelectOutputRef :: forall a ann. IsPrimitive a => Int -> AssignVar -> Doc ann
genSelectOutputRef idx varNum =
  indexArray (selectMemberName @a varNum) $ Doc.pretty idx

{- Top-level expression generation -}

data GenError
  = AssignError (AssignTypeError Identity)
  | GenOpError GenOperationError
  | MissedGenAssignment (CExprF Identity AssignState)
  | PrimOpGivenAggregateType (CExprF Identity AssignState)

data GenOperationError = GenOperationError
  { _genOperationErrorExpected :: [PrimType],
    _genOperationErrorGot :: PrimType,
    _genOperationErrorOperation :: CExprF Identity AssignState
  }

prettyRef :: AssignState -> Doc ann
prettyRef = prettyAssignVar . assignVar

genAssignStatement :: forall a ann. IsPrimitive a => AssignVar -> Doc ann -> Doc ann
genAssignStatement var = constAssignStatement (genTypeName @a) (prettyAssignVar var)

genAssignment :: forall a ann. AssignVar -> CExprF (Const AssignState) a -> Doc ann
genAssignment var = \case
  FunctionCallF funcall -> genFunctionCall var (Barbies.bmap (first prettyRef) funcall)
  SelectF selection (Const index) -> genSelect var selection index
  LitF a -> go @a $ genPrimitive (toPrim (Identity a))
  IntUnOpF op (pretty -> a) -> go @a $ genIntUnOp @a op a
  IntBinOpF op (pretty -> a) (pretty -> b) -> go @a $ genIntBinOp @a op a b
  IntTestOpF op (pretty -> bitIdx) (intVal :: Const AssignState b) ->
    go @a $ genIntTestOp @b op bitIdx (pretty intVal)
  IntSetBitF op (pretty -> intVal) (pretty -> bitIdx) (pretty -> bitVal) ->
    go @a $ genIntSetBit @a op intVal bitIdx bitVal
  RoundF _ (val :: Const AssignState b) -> go @a $ genRoundFP @b (pretty val)
  CastF (from :: Const AssignState b) ->
    -- 'b' is "from" type; 'a' is "to" type.
    let fromType = primGADT @b
     in go @a $ case fromType of
          GInt8 -> genCast @a (pretty from)
          GInt16 -> genCast @a (pretty from)
          GInt32 -> genCast @a (pretty from)
          GInt64 -> genCast @a (pretty from)
          GWord8 -> genCast @a (pretty from)
          GWord16 -> genCast @a (pretty from)
          GWord32 -> genCast @a (pretty from)
          GWord64 -> genCast @a (pretty from)
          GFloat -> genCastFP @a (pretty from)
          GDouble -> genCastFP @a (pretty from)
  FPUnOpF op a -> go @a $ case primGADT @a of
    GFloat -> genFloatUnOp op (pretty a)
    GDouble -> genDoubleUnOp op (pretty a)
  FPBinOpF op a b -> go @a $ case primGADT @a of
    GFloat -> genFloatBinOp op (pretty a) (pretty b)
    GDouble -> genDoubleBinOp op (pretty a) (pretty b)
  FPTestOpF op (pretty -> a) -> go @a $ genFPTestOp op a
  BoolUnOpF op (pretty -> a) -> go @a $ genBoolUnOp op a
  BoolBinOpF op (pretty -> a) (pretty -> b) -> go @a $ genBoolBinOp op a b
  CmpOpF op (pretty -> a) (pretty -> b) -> go @a $ genCmpOp op a b
  BranchF (pretty -> b) (pretty -> t) (pretty -> f) -> go @a $ genKBranch b t f
  InputF i -> go @a $ genInput @a i
  FunctionCallOutputF out i -> go @a . genFunctionOutputRef @a i . assignVar $ getConst out
  SelectOutputF out i -> go @a . genSelectOutputRef @a i . assignVar $ getConst out
  where
    go :: forall b. IsPrimitive b => Doc ann -> Doc ann
    go = genAssignStatement @b var

    pretty = prettyRef . getConst

genOutputAssignment :: forall proxy a ann. IsPrimitive a => proxy a -> AssignVar -> Int -> Doc ann
genOutputAssignment _ var idx = assignStatement (genOutput @a idx) $ prettyAssignVar var

wrapWithExternC :: Doc ann -> Doc ann
wrapWithExternC decls =
  Doc.concatWith
    (\x y -> x <> Doc.hardline <> y)
    [ "#ifdef __cplusplus",
      "extern \"C\" {",
      "#endif  // __cplusplus",
      decls,
      "#ifdef __cplusplus",
      "}  // extern \"C\"",
      "#endif  // __cplusplus"
    ]

genFunctionSignature ::
  forall ann.
  -- | Input spec
  Arrays (Const Int) ->
  -- | Output spec
  Arrays (Const Int) ->
  -- | Function name
  Doc ann ->
  -- | Function signature
  Doc ann
genFunctionSignature inputSpec outputSpec functionName =
  noLintFunctionName <> Doc.hardline <> "void" <+> functionName
    <+> Doc.group
      (cFunArgs $ inputArgs <> outputArgs)
  where
    inputArgs = ("const" <+>) <$> Barbies.bfoldMapC @IsPrimitive (mkArg "input") inputSpec
    outputArgs = Barbies.bfoldMapC @IsPrimitive (mkArg "output") outputSpec
    mkArg :: forall a. IsPrimitive a => Doc ann -> Const Int a -> [Doc ann]
    mkArg label (Const n) =
      let typeName = genTypeName @a
          -- We have no guarantees that if someone calls `kFunctionCall`, every value will get used.
          -- If an input is used, this attribute has no effect.  So the easiest thing to do to avoid
          -- warnings when compiling the generated C code is to mark every input potentially unused
          -- when we generate it!
          --
          -- This is a temporary solution; see https://github.com/con-kitty/categorifier-c/issues/23
          -- for more information.
          unused = "__attribute__((unused))"
       in pure $ typeName <+> label <> "_" <> typeName <> Doc.brackets (Doc.pretty n) <+> unused
    -- See https://github.com/con-kitty/categorifier-c/issues/22
    noLintFunctionName = "// NOLINTNEXTLINE(readability-identifier-naming)"

genFunctionDeclaration :: Arrays (Const Int) -> Arrays (Const Int) -> Doc ann -> Doc ann
genFunctionDeclaration inspec outspec functionName =
  genFunctionSignature inspec outspec functionName <> newline

genFunctionDefinition :: Arrays (Const Int) -> Arrays (Const Int) -> Doc ann -> Doc ann -> Doc ann
genFunctionDefinition inspec outspec functionName functionBody =
  genFunctionSignature inspec outspec functionName <+> cBlock [functionBody]
  where
    cBlock body = encloseSepIndent 2 ("{" <> Doc.line) (Doc.line <> "}") "" body <> Doc.line
