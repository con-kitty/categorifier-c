{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Categorifier.C.CTypes.DSL.CxxAst
  ( CDecl (..),
    Define (..),
    Comment (..),
    Identifier (..),
    Include (..),
    SystemLib (..),
    TapeElement (.., SingleIf),
    CExpr
      ( ..,
        (:<),
        (:>),
        (:<=),
        (:>=),
        (:!=),
        (:==),
        (:&&),
        (:||),
        (:/),
        (:.),
        (:*),
        (:+),
        (:-),
        (:<<),
        (:->),
        (:++),
        IsNan,
        IsInf,
        Not,
        TakeAddress,
        StdEndl,
        StdHex
      ),
    pattern StdIosBaseFmtFlags,

    -- * Union
    unionTagLiteral,

    -- * array index
    (!),

    -- * Casts
    CTypeWithBackdoor (..),
    staticCast,
    oldCCast,

    -- * bool literal
    true,
    false,

    -- * Pointers
    dereference,
    --
    ConditionBlock (..),
    BinOp (..),
    UnOp (..),
    Typedef (..),
    UsingDecl (..),
    UsingDeclGroup (..),

    -- * Function calls
    (#!),
    StdFunc (..),

    -- * Functions
    CFunction (..),
    Param (..),
    ParamType (..),
    ExternalType (..),

    -- * Type-level
    TypeLevelFunctions (..),

    -- * Modules
    CxxOrC (..),
    CxxModule (..),
    CxxTarget (..),
    natExpr,

    -- * Internals
    ToCTypeWithBackdoor (..),
  )
where

import Categorifier.C.CTypes.Codegen.Helpers (CStructOrUnion (..))
import Categorifier.C.CTypes.KTypeRep (KTypeRep)
import qualified Categorifier.C.CTypes.Render as R
import Categorifier.C.CTypes.Types
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as LB
import Data.Typeable (TyCon)
import PyF (fmt)

newtype Comment = Comment {unComment :: T.Text}

-- | @'Define' maybeComment lhs rhs@ turns into:
-- > // optional comment
-- > #define lhs rhs
data Define = Define (Maybe Comment) T.Text T.Text

data SystemLib
  = Assert
  | StdBool
  | StdInt
  | Time
  | Math
  | Def
  deriving (Eq, Ord, Show, Enum, Bounded)

data Include
  = IncludeThatNeedsDefines [Define] Include
  | IncludeModule CxxTarget
  | IncludeSystemLib SystemLib
  | IncludeSystemFile T.Text
  | IncludeHeavisoftFile T.Text

-- | A module is something that creates exactly one function group which will
-- turn into either a header or a source and header.
data CxxTarget
  = CTypes
  | CxxTypes
  | CborEncode -- TODO(greg): use Enumerate to do Cbor DecodeOrEncode
  | CborDecode
  | DataLayout
  | EnumUtils
  | FromArrays -- TODO(greg): use Enumerate to do PolyVec ToOrFromArrays
  | PrintDifferences
  | ToArrays
  | IsEqual
  | ToString -- TODO(greg): rename this Render or something
  | Dimensions
  deriving (Bounded, Enum)

data CxxOrC = Cxx | C

data UsingDecl = UsingDecl [KTypeRep] CStructOrUnion deriving (Eq, Ord)

data UsingDeclGroup = UsingDeclGroup TyCon [UsingDecl]

-- | A list of #defines, typedefs, and functions to be generated.
-- A source and header will be generated from this group.
-- Includes and defines will be added to the header file.
-- The source file will include the header file and nothing else.
-- It's not a real module, we just model it so for codegen and dependency tracking.
data CxxModule = CxxModule
  { moduleIncludes :: [Include],
    moduleDefines :: [Define],
    moduleTypedefs :: [Typedef],
    moduleUsingDecls :: [UsingDeclGroup],
    moduleFunctions :: [CFunction],
    moduleTypeLevelFunctions :: [TypeLevelFunctions]
  }

-- | Template struct specializations.
-- Currently used for constexpr int32_t EnumLength<>::Length().
data TypeLevelFunctions = TypeLevelFunctions
  { tlfReturnType :: T.Text,
    tlfClassName :: T.Text,
    tlfFunctionName :: T.Text,
    tlfInstances :: [(T.Text, T.Text)]
  }

newtype Identifier = Identifier {unIdentifier :: T.Text} deriving (Show)

newtype ExternalType = ExternalType T.Text deriving (Show)

data ParamType
  = ParamCxxType (CxxType Proxy)
  | ParamCxxUnionCon (CxxUnionCon Proxy)
  | -- | enum associated with a union
    ParamCUnionTag (CUnion Proxy)
  | ParamCUnionCon (CUnionCon Proxy)
  | -- | JSON::Value, CborEncoder, CborValue, etc
    ParamExternalType ExternalType
  deriving (Show)

data Param = Param
  { pType :: ParamType,
    pId :: Identifier,
    pUnused :: Bool,
    pMutable :: Bool
  }
  deriving (Show)

-- A C function.
data CFunction = UnsafeCFunction
  { cfName :: Identifier,
    -- | Optionally generate a trivial wrapper, used for overloading functions.
    -- This increases C++ compilation times quite a bit, so only use when you can't do it
    -- any other way.
    -- TODO(greg): Remove this mechanism once we autogenerate Comms/netgraph C++ code.
    cfInlineOverloadName :: Maybe Identifier,
    cfReturnType :: Maybe ParamType,
    cfParams :: [Param],
    cfTape :: [TapeElement],
    cfComment :: Maybe Comment,
    cfStaticLinkage :: Bool
  }

instance Show CFunction where
  show _ = "<<A CFunction>>"

data Typedef
  = TypedefEnum (CEnum Proxy)
  | TypedefCxxUnion (CxxUnion Proxy)
  | TypedefCUnion (CUnion Proxy)
  | TypedefCxxStruct (CxxStruct Proxy)
  | TypedefCStruct (CStruct Proxy)

data CDecl
  = CDeclTypedef Typedef
  | CDeclFunctionDecl CFunction
  | CDeclFunctionDef CFunction
  | CDeclDefine Define
  | CDeclBackdoor LB.Builder

-- For expression, a few TODOs:
-- - it should be typed @CExpr t@
-- - it should integrate a @Mutable@ parameter
-- - Perhaps expression can be a typeclass. This way it can be
--   extensible, and some basic haskell type can be used directly as
--   literal, such as Char, string literal, integers, using the ToCxxType typeclass
-- - I'd like CExpr to implement some usefull typeclass such as Num,
--   but it won't be compatible with type safety

-- | Represents an expression
data CExpr
  = -- | a variable
    Ident Identifier
  | -- | Cast to a type. C++ style
    StaticCast CTypeWithBackdoor CExpr
  | -- | foo->as_bar -- TODO(greg): this needs stronger typing
    -- ^ TODO(guibou) This will/can be removed
    AsUnionCon CExpr (CCon Proxy)
  | -- | A binary operation between two expression
    BinaryOp BinOp CExpr CExpr
  | -- | Unary expression
    UnaryOp UnOp CExpr
  | -- | Call a function using an expression as name
    CallFunction CExpr [CExpr]
  | -- | Transform a CFunction to a CExpr
    FunctionExpr CFunction
  | -- | An integer literal. TODO: use 'Num' instance
    LiteralInt Integer
  | -- | An boolean literal
    LiteralBool Bool
  | -- | A String literal.
    LiteralString T.Text
  | -- | A literal Char
    LiteralChar Char
  | -- | A Lambda
    Lambda [Identifier] [(CxxUnionCon Proxy, Identifier)] [TapeElement]
  | -- | IfThenElse predicate ifTrue ifFalse
    IfThenElse CExpr CExpr CExpr
  deriving (Show)

{-
-- Todo(guillaume) use KEq / KOrd and friends
-}

-- | A set of binary operations. Exposed through patterns
data BinOp
  = LessThan
  | Equal
  | NotEqual
  | GreaterThan
  | LessThanEqual
  | GreaterThanEqual
  | And
  | Or
  | Add
  | Sub
  | Mul
  | Quot
  | LeftShift
  | Subscript
  deriving (Show)

true :: CExpr
true = LiteralBool True

false :: CExpr
false = LiteralBool False

-- | A set of unary operations. Exposed through patterns
data UnOp
  = Not'
  | Dereference
  | Address
  | -- | foo.bar
    FieldByDot RfName
  | -- | foo->bar
    FieldByPtr RfName
  | -- | Cast to a type.
    CStyleCast (CType Proxy)
  | IsNan'
  | IsInf'
  deriving (Show)

-- Pattern synonyms

-- * Operators

pattern (:<) :: CExpr -> CExpr -> CExpr

pattern (:>) :: CExpr -> CExpr -> CExpr

pattern (:<=) :: CExpr -> CExpr -> CExpr

pattern (:>=) :: CExpr -> CExpr -> CExpr

pattern (:!=) :: CExpr -> CExpr -> CExpr

pattern (:==) :: CExpr -> CExpr -> CExpr

pattern (:&&) :: CExpr -> CExpr -> CExpr

pattern (:||) :: CExpr -> CExpr -> CExpr

pattern (:.) :: CExpr -> RfName -> CExpr

pattern (:->) :: CExpr -> RfName -> CExpr

pattern (:/) :: CExpr -> CExpr -> CExpr

pattern (:+) :: CExpr -> CExpr -> CExpr

pattern (:++) :: CExpr -> CExpr -> CExpr

pattern (:*) :: CExpr -> CExpr -> CExpr

pattern (:-) :: CExpr -> CExpr -> CExpr

pattern (:<<) :: CExpr -> CExpr -> CExpr

pattern Not :: CExpr -> CExpr

pattern IsNan :: CExpr -> CExpr

pattern IsInf :: CExpr -> CExpr

pattern TakeAddress :: CExpr -> CExpr

pattern a :< b = BinaryOp LessThan a b

pattern a :> b = BinaryOp GreaterThan a b

pattern a :<= b = BinaryOp LessThanEqual a b

pattern a :>= b = BinaryOp GreaterThanEqual a b

pattern a :== b = BinaryOp Equal a b

pattern a :!= b = BinaryOp NotEqual a b

pattern a :&& b = BinaryOp And a b

pattern a :|| b = BinaryOp Or a b

pattern a :. b = UnaryOp (FieldByDot b) a

pattern a :-> b = UnaryOp (FieldByPtr b) a

pattern a :/ b = BinaryOp Quot a b

pattern a :* b = BinaryOp Mul a b

pattern a :+ b = BinaryOp Add a b

pattern a :- b = BinaryOp Sub a b

pattern a :++ b = BinaryOp Add a b

pattern a :<< b = BinaryOp LeftShift a b

pattern Not x = UnaryOp Not' x

pattern IsNan x = UnaryOp IsNan' x

pattern IsInf x = UnaryOp IsInf' x

pattern TakeAddress x = UnaryOp Address x

-- | Array indexing. TODO: check the symbol
(!) :: CExpr -> CExpr -> CExpr
(!) = BinaryOp Subscript

-- | Call a function, infix version. TODO: check the symbol
(#!) :: CallFunc f => f -> [CExpr] -> CExpr
(#!) = callFunc

-- * Fixity

-- same fixity as haskell equivalent

-- infixl

infixl 4 :<=

infixl 4 :<

infixl 4 :==

infixl 4 :>=

infixl 4 :>

infixl 4 :!=

-- string concatenation in haskell is infixr
-- however I decided to keep the C++ semantic
infixl 5 :++

infixl 6 :-

infixl 6 :+

infixl 7 :*

infixl 7 :/

-- infixr

infixr 2 :||

infixr 3 :&&

-- introduced fixity

-- should bind more tightly than anything
-- e.g a + b :. c ! 10 #! [] --> a + ((((b :. c) ! 10)) #! [])
infixl 8 :.

infixl 8 :->

infixl 8 !

infixl 8 #!

-- Not really used often, but it should at least bind less than used operators
infixl 1 :<<

-- | A 'TapeElement' is equivalent to a C block
data TapeElement
  = FunctionCall CFunction [CExpr] (Maybe Comment)
  | -- TODO: there is too many switch case, and a few of them contains lambda
    -- We can remove all of them but 'SwitchCase'
    CUnionConSwitch CExpr (CUnion Proxy) (CCon Proxy -> CExpr -> [TapeElement])
  | -- | Generalized Switch case.
    -- SwitchCase a b c d:
    --   a: optional type name of the enum being switched on.
    --      If this is 'Just' then it will generate @forceSigned@ and @numFields@ before the default
    --      case.
    --   b: switch(b)
    --   c: [(e, f)], list of case e: code f
    --   d: default
    SwitchCase (Maybe T.Text) CExpr [(T.Text, [TapeElement])] [TapeElement]
  | -- | Generalized ForLoop
    ForLoop [TapeElement] CExpr [TapeElement] [TapeElement]
  | -- | A C++ foreach
    ForCxx CTypeWithBackdoor Identifier CExpr [TapeElement]
  | -- | do whatever you want
    TapeBackdoor T.Text
  | -- | An condition with optional elseif / else block
    If [ConditionBlock] (Maybe [TapeElement]) -- TODO: use a NonEmpty?
  | -- | Return an expression
    Return CExpr
  | -- | Return nothing
    ReturnVoid
  | -- | Assign a value
    Assign CExpr CExpr
  | -- | Declare a new variable with a value
    Declare CTypeWithBackdoor Identifier CExpr
  | -- | Declare a new variable with default assignment (C++ semantic)
    DeclareDefault CTypeWithBackdoor Identifier
  | -- | Declare a new variable with empty list initializer assignment (C++ semantic)
    DeclareCxxDefault CTypeWithBackdoor Identifier
  | -- | A comment which will appear in the generated code
    Comment' T.Text
  | -- | The weird ++x used in C++, mostly used for iterators. Not exposed to user
    PlusPlus CExpr
  | -- | Force a CExpression to be evaluated.
    -- TODO: implement the difference between pure and impure functions
    ForceExpr CExpr
  | -- | Union cast.
    UnionCast CTypeWithBackdoor CTypeWithBackdoor CExpr Identifier

instance Show TapeElement where
  show _ = "<<A TapeElement>>"

-- | Simplify the If generic expression for singleton ifs
pattern SingleIf :: CExpr -> [TapeElement] -> TapeElement
pattern SingleIf condition expr = If [ConditionBlock condition expr] Nothing

-- | Contains a condition and an associated block
-- | Used in If statements
data ConditionBlock = ConditionBlock CExpr [TapeElement]

-- * Builtins

-- | Standard C++ builtins
data StdFunc
  = -- | @std::isnan@
    StdIsNaN
  | -- | @std::get\<k\>@
    StdGet Int
  | -- | @std::to_string@
    StdToString
  | -- | @std::setfill@
    StdSetfill
  | -- | @std::setw@
    StdSetw
  | -- | @assert@
    StdAssert
  deriving (Show)

-- | @std::endl@
pattern StdEndl :: CExpr
pattern StdEndl = Ident (Identifier "std::endl")

-- | @std::hex@
pattern StdHex :: CExpr
pattern StdHex = Ident (Identifier "std::hex")

-- | @std::ios_base::fmtflags@
pattern StdIosBaseFmtFlags :: CTypeWithBackdoor
pattern StdIosBaseFmtFlags = CTypeBackdoor "std::ios_base::fmtflags"

-- | This was introduced to provide CTypeBackdoor
-- TODO: remove that in the future
data CTypeWithBackdoor
  = -- | Represents a 'CType'
    NewCType (CType Proxy)
  | -- | AnyType
    CTypeBackdoor T.Text
  deriving (Show)

stdFuncToIdentifier :: StdFunc -> T.Text
stdFuncToIdentifier = \case
  StdIsNaN -> "std::isnan"
  StdGet k -> [fmt|std::get<{k}>|]
  StdToString -> [fmt|std::to_string|]
  StdSetfill -> [fmt|std::setfill|]
  StdSetw -> [fmt|std::setw|]
  StdAssert -> [fmt|assert|]

-- * Function calls

-- | Call a function
class CallFunc t where
  -- TODO: type safe api without a list
  callFunc :: t -> [CExpr] -> CExpr

-- | Implementation for CFunction
instance CallFunc CFunction where
  callFunc f l = CallFunction (FunctionExpr f) l

-- | Implementation for C++ function
instance CallFunc StdFunc where
  callFunc stdFun l = CallFunction (Ident (Identifier (stdFuncToIdentifier stdFun))) l

-- | Implementation for any Identifier (unsafe, it may no be a function)
instance CallFunc Identifier where
  callFunc i l = CallFunction (Ident i) l

-- | Implementation for any CExpr (unsafe, it may no be a function)
instance CallFunc CExpr where
  callFunc e l = CallFunction e l

natExpr :: CNat -> CExpr
natExpr (CNatInt i) = LiteralInt (fromIntegral i)
natExpr (CNatType i _) = LiteralInt (fromIntegral i)

-- | Use a literal string as CExpr
instance IsString CExpr where
  fromString s = LiteralString (T.pack s)

-- | Dereference a pointer
dereference :: CExpr -> CExpr
dereference = UnaryOp Dereference

-- | A static cast.
staticCast :: ToCTypeWithBackdoor t => t -> CExpr -> CExpr
staticCast = StaticCast . toCTypeWithBackdoor

oldCCast :: CType Proxy -> CExpr -> CExpr
oldCCast t e = UnaryOp (CStyleCast t) e

-- | Tag literal
unionTagLiteral :: CUnion Proxy -> CCon Proxy -> CExpr
unionTagLiteral cunion = Ident . Identifier . R.renderCUnionTagLiteral cunion

-- | Convert a type to CTypeWithBackdoor
class ToCTypeWithBackdoor t where
  toCTypeWithBackdoor :: t -> CTypeWithBackdoor

instance ToCTypeWithBackdoor (CType Proxy) where
  toCTypeWithBackdoor = NewCType

instance ToCTypeWithBackdoor CTypeWithBackdoor where
  toCTypeWithBackdoor = id
