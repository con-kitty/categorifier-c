{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- For `KEq CExpr ()`, `KEq CExpr (a, b)`, `KOrd CExpr ()` and `KOrd CExpr (a, b)`.
-- TODO: these instances are orphans because of the fundeps in `KEq` and `KOrd`.
-- We should probably remove the fundeps so that (1) they are no longer orphans,
-- and (2) it's possible to add `KEq C ()` etc.
{-# OPTIONS_GHC -Wno-orphans #-}
-- This has redundant contstraints because of SBV stuff on the `KFromIntegral` class.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Statically-typed expression functors.
module Kitty.CExpr.Types.Core
  ( Callee (..),
    FunctionCall (..),
    Select (..),
    PrimTypeLens,
    CExprF (..),
    CExpr,
    CExprHaskellFunction,
  )
where

import qualified Barbies
import qualified Barbies.Constraints as Barbies
import Control.Arrow ((&&&))
import Control.Lens (set, view, _Wrapped)
import Data.Bits (FiniteBits (..))
import Data.Foldable (foldl', toList)
import Data.Function (on)
import Data.Functor.Classes (Show1 (..), showsPrec1)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (Pair))
import Data.Hashable (Hashable (..))
import Data.Hashable.Lifted (Hashable1 (..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Endo (..))
import Data.Proxy (Proxy (..))
import qualified Data.Semigroup
import Data.Text (Text)
import Data.Typeable (typeRep)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Kitty.CExpr.Cat.TargetOb (TargetOb)
import Kitty.CExpr.Types (CExprFunctionCall, CExprSelectExpression, CExprTypeLens)
import Kitty.CExpr.Types.Operations
  ( BoolBinOp (..),
    BoolUnOp (..),
    CmpOp (..),
    FPBinOp (..),
    FPTestOp (..),
    FPUnOp (..),
    IntBinOp (..),
    IntSetBit (..),
    IntTestOp (..),
    IntUnOp (..),
    Round (..),
  )
import Kitty.CTypes.CGeneric.Class (CGeneric)
import qualified Kitty.CTypes.CGeneric.Class as CG
import Kitty.Higher
  ( HFoldableC (..),
    HFunctorC (..),
    HTraversableC (..),
    hfoldMapCDefault,
    hmapCDefault,
  )
import Kitty.KTypes.BooleanLogic (KAnd (..))
import Kitty.KTypes.Conditional (KSelect (..), KTernary (..))
import Kitty.KTypes.Equality (KEq (..))
import Kitty.KTypes.FromIntegral (KFromIntegral (..))
import Kitty.KTypes.IEEE (KConvertFloat (..), KIsInfinite (..), KIsNaN (..))
import Kitty.KTypes.KBits (KBits (..))
import Kitty.KTypes.KDivisible (KDivisible (..))
import Kitty.KTypes.KLiteral (KLiteral (..))
import Kitty.KTypes.Round (KRound (..), safeRound)
import Kitty.KTypes.SwitchCase (KIf (..))
import Kitty.KTypes.TotalOrder (KOrd (..))
import Kitty.PolyVec (PolyVec)
import Kitty.Prim
  ( Arrays,
    IsPrimitive (..),
    PrimAny,
    PrimFractional,
    PrimGADT (..),
    PrimIntegral,
    PrimLens (..),
    PrimNum,
  )
import Kitty.Recursion (HFix, hembed)

data Callee a
  = -- | Generate a C function with the given body
    Generated a
  | -- | An existing C function, with the given output spec, imported from the given header file
    Imported (Arrays (Const Int)) Text
  | -- | Manually write a C function with the given output spec
    HandWritten
      (Arrays (Const Int))
      -- ^ Output spec
      [Text]
      -- ^ Header files needed
      (Maybe Text)
      -- ^ Function declaration, if needed
      Text
      -- ^ Function definition

deriving instance Show a => Show (Callee a)

-- | This describes a function call to an external function with the same calling convention (one
-- input and one output array per primitive type).
--
-- The main reason to make this a separate type is so that we can write its 'Traversable' instance
-- by hand and continue using automatic deriving for the main AST functor.
data FunctionCall r = FunctionCall
  { -- | Function name
    functionCallName :: !Text,
    -- | Inputs from the calling function
    functionCallInputs :: Arrays (Compose Vector r),
    -- | outputs of the child
    -- function (the ASTs)
    functionCallBody :: Callee (Arrays (Compose Vector CExpr))
  }
  deriving (Generic)

deriving instance (Show1 r, Show1 CExpr) => Show (FunctionCall r)

-- This is slightly hacky, but I don't see how it could cause us problems in this situation.  It
-- could probably be replaced by the real deal.
instance Hashable1 r => Eq (FunctionCall r) where
  (==) = (==) `on` (functionCallName &&& hashWithSalt 2222)

bfoldlC' ::
  forall c x f b.
  (Barbies.TraversableB x, Barbies.ConstraintsB x, Barbies.AllB c x) =>
  (forall a. c a => b -> f a -> b) ->
  b ->
  x f ->
  b
bfoldlC' f z t = bfoldrC f' id t z
  where
    f' :: forall a. c a => f a -> (b -> b) -> b -> b
    f' x acc z0 = acc $! f z0 x

    bfoldrC :: forall d. (forall a. c a => f a -> d -> d) -> d -> x f -> d
    bfoldrC fr zr tr = appEndo (Barbies.bfoldMapC @c (Endo . fr) tr) zr

hashVectorWithSalt ::
  forall s a.
  (Hashable1 s, IsPrimitive a) =>
  Int ->
  Compose Vector s a ->
  Int
hashVectorWithSalt salt = Vector.foldl' (liftHashWithSalt hashWithSalt) salt . getCompose

instance Hashable1 r => Hashable (FunctionCall r) where
  hashWithSalt s (FunctionCall name inputs body) =
    s
      `hashWithSalt` name
      `hashWithSalt` bfoldlC' @IsPrimitive hashVectorWithSalt 22 inputs
      `hashWithSalt` ( case body of
                         Generated graph -> bfoldlC' @IsPrimitive hashVectorWithSalt 222 graph
                         Imported spec t -> s `hashWithSalt` spec `hashWithSalt` t
                         HandWritten spec hdrs decl defn ->
                           s `hashWithSalt` spec
                             `hashWithSalt` hdrs
                             `hashWithSalt` decl
                             `hashWithSalt` defn
                     )

instance Barbies.FunctorB FunctionCall where
  bmap f = runIdentity . Barbies.btraverse (Identity . f)

instance Barbies.TraversableB FunctionCall where
  btraverse f (FunctionCall name inputs body) =
    FunctionCall name
      <$> Barbies.btraverse (fmap Compose . traverse f . getCompose) inputs
      <*> pure body

instance Barbies.ConstraintsB FunctionCall where
  type AllB c FunctionCall = Barbies.AllB c Arrays

  baddDicts ::
    forall c r.
    Barbies.AllB c Arrays =>
    FunctionCall r ->
    FunctionCall (Barbies.Dict c `Product` r)
  baddDicts call =
    call
      { functionCallInputs =
          Barbies.bmap f . Barbies.baddDicts $ functionCallInputs call
      }
    where
      f ::
        forall a.
        Product (Barbies.Dict c) (Compose Vector r) a ->
        Compose Vector (Product (Barbies.Dict c) r) a
      f (Pair dict vec) = Compose . fmap (Pair dict) . getCompose $ vec

-- | This describes a @select@ (indexing among choices) statement.
data Select r = Select
  { -- | The shape of each output argument
    selectArgSpec :: Arrays (Const Int),
    -- | A list of the choices.  In the case of an out-of-bounds index, the first choice will be
    -- selected; the need to have this default choice leads to the 'NonEmpty' requirement.
    selectOptions :: NonEmpty (Arrays (Compose Vector r))
  }
  deriving (Eq, Ord, Show, Generic)

instance Barbies.FunctorB Select where
  bmap f = runIdentity . Barbies.btraverse (Identity . f)

instance Barbies.TraversableB Select where
  btraverse f (Select spec opts) =
    Select spec <$> traverse (Barbies.btraverse (fmap Compose . traverse f . getCompose)) opts

instance Barbies.ConstraintsB Select where
  type AllB c Select = Barbies.AllB c Arrays
  baddDicts (Select spec opts) = Select spec $ fmap (Barbies.bmap go . Barbies.baddDicts) opts
    where
      go (Pair dict (Compose vf)) = Compose $ fmap (Pair dict) vf

instance Hashable1 r => Hashable (Select r) where
  hashWithSalt s (Select spec opts) =
    s
      `hashWithSalt` spec
      `hashWithSalt` foldl' (bfoldlC' @IsPrimitive hashVectorWithSalt) 22 opts

-- | Definining this as a type class, rather than a type synonym, so that it can be
-- partially applied, e.g., used in @Barbies.bmapC@.
class (PrimAny a, CExprTypeLens a) => PrimTypeLens a

instance (PrimAny a, CExprTypeLens a) => PrimTypeLens a

-- | This is the strongly-typed version of the expression functor describing the C programs we know
-- how to generate.  In documentation, we often refer to a type @`CExprF` r a@ as having "AST
-- functor @r@" and "base type @a@".  The base type of a @CExpr@ expression corresponds to the C
-- type that the statement implementing it will have in the generated code.
--
-- == Uses
--
-- The fixed point of this expression functor (in its first argument @r@) yields the AST structure
-- used by the frontend to describe the flight control algorithm.  The graph reification stage of
-- this backend transforms the AST to values of type @`CExprF` (`Const` `Int`) a@, so that it forms
-- a flat graph structure with integer references.
--
-- == Uniqueness and multiplicity of constructors
--
-- It may seem like `IntTestOpF` could be just another `IntBinOp`, `FPTestOpF` could be an `FPUnOp`,
-- and so on.  These operations are distinguished by their argument types and results.  Each
-- constructor has a unique combination of argument types, result types and embedded constraints.
-- We want to keep these cases separate so that we preserve type-safety by construction at every
-- step of the code-generation algorithm.  This leads to more constructors and more complex code in
-- a few places, but it provides guarantees about the overall correctness of the code generation
-- process.
--
-- == Embedded constraints
--
-- Each constructor places some constraints on the return type if it's a variable:
--
--    * Most operations deal only with types which are primitive C / machine types and thus place an
--      `IsPrimitive` constraint on the base type variable.
--
--    * Constructors which hide the base type of one of their arguments, such as `IntTestOpF`,
--      `RoundF`, `CastF`, `FPTestOpF` or `CmpOpF`, require an additional constraint
--      `CExprTypeLens` on the hidden base type variable to allow it to be pulled out of a product
--      of a closed set of base types using a lens.  This is required because the precise type
--      cannot be recovered directly by pattern-matching on the constructor.  We can still work with
--      these terms in a type-safe way by requiring this polymorphic indexing capability.  Depending
--      on the operation at hand, we may index into a `Kitty.CExpr.Types.CExprTypeProduct` (a
--      product of all permitted base types) or a `Data.Prim.Base.Arrays` (a product of all
--      primitive base types, excluding "aggregate types", about which more below).
--
--    * Constructors typically also require the plain Haskell constraint corresponding to the logic
--      they implement (for example, `IntUnOpF` requires `Integral`, `IntTestOpF` requires
--      `FiniteBits`, `FPUnOpF` requires `RealFrac` and so on).  This occasionally comes in useful
--      during C source generation and furthermore preserves the information needed for direct
--      interpretation of the AST in Haskell.
--
-- Several constructors simply produce a `CExprF` of fixed base type: `IntTestOpF`, `FPTestOpF`,
-- `BoolBinOpF`, `BoolUnOpF`, `FunctionCallF` and `SelectF`.
--
-- == Aggregate base types
--
-- Internal function calls deal with the "aggregate type" `CExprFunctionCall`, which in practice
-- means a value of type @`Data.Prim.Base.Arrays` (`Compose` `Vector` r)@, where @r a@ is an AST
-- fragment of /primitive/ type @a@.  AST fragments of type @r `CExprFunctionCall`@ cannot be passed
-- into or out of internal function calls.  You can verify this by inspecting the constraints on all
-- the constructors; the only way you can refer to a `FunctionCallF` (of aggregate type) is to point
-- to it inside a `FunctionCallOutputF` (of primitive type), corresponding to one primitive value
-- returned from the function.
--
-- Select (switch-case) operations work the same way as function calls, with their own aggregate
-- type `CExprSelectExpression`.
data CExprF :: (Type -> Type) -> Type -> Type where
  LitF :: IsPrimitive a => a -> CExprF r a
  IntUnOpF :: PrimIntegral a => IntUnOp -> r a -> CExprF r a
  IntBinOpF :: PrimIntegral a => IntBinOp -> r a -> r a -> CExprF r a
  IntTestOpF ::
    (PrimTypeLens a, PrimIntegral a, FiniteBits a) =>
    IntTestOp ->
    -- | Bit index
    r Word8 ->
    -- | Input value
    r a ->
    CExprF r Bool
  IntSetBitF ::
    (PrimIntegral a, FiniteBits a) =>
    IntSetBit ->
    -- | Input value
    r a ->
    -- | Bit index
    r Word8 ->
    -- | Boolean value to which to set the indexed bit
    r Bool ->
    CExprF r a
  RoundF ::
    (PrimTypeLens b, PrimFractional b, PrimFractional a) =>
    Round ->
    r b ->
    CExprF r a
  CastF ::
    ( PrimTypeLens b,
      PrimNum b,
      PrimNum a
    ) =>
    r b ->
    CExprF r a
  FPBinOpF :: PrimFractional a => FPBinOp -> r a -> r a -> CExprF r a
  FPUnOpF :: PrimFractional a => FPUnOp -> r a -> CExprF r a
  FPTestOpF :: (PrimTypeLens a, PrimFractional a) => FPTestOp -> r a -> CExprF r Bool
  BoolBinOpF :: BoolBinOp -> r Bool -> r Bool -> CExprF r Bool
  BoolUnOpF :: BoolUnOp -> r Bool -> CExprF r Bool
  -- NB: this constructor is responsible for considerable pain, because its top-level term erases
  -- the type info about 'a', and everything becomes more difficult to handle because we don't
  -- necessarily know 'a'.  The pain is mitigated by requiring 'IsPrimitive'.
  CmpOpF ::
    (PrimTypeLens a, Ord a) =>
    CmpOp ->
    r a ->
    r a ->
    CExprF r Bool
  BranchF :: IsPrimitive a => r Bool -> r a -> r a -> CExprF r a
  InputF :: IsPrimitive a => Int -> CExprF r a
  FunctionCallF :: FunctionCall r -> CExprF r CExprFunctionCall
  FunctionCallOutputF ::
    PrimTypeLens a =>
    -- | Function call we want to get output from
    r CExprFunctionCall ->
    -- | Index into the primitive array of type @a@ in that function call's outputs
    Int ->
    CExprF r a
  SelectF ::
    Select r ->
    -- | Choice of branch within the select expression (index into the array of possible result
    -- structures in the generated C code)
    r Word8 ->
    CExprF r CExprSelectExpression
  SelectOutputF ::
    PrimTypeLens a =>
    -- | Select expression we want to get output from
    r CExprSelectExpression ->
    -- | Index into the primitive array of type @a@ in that select expression's outputs
    Int ->
    CExprF r a

deriving instance
  (Show1 r, Show a, forall b. Show b => Show (r b), Show (FunctionCall r)) =>
  Show (CExprF r a)

instance Show1 r => Show1 (CExprF r) where
  liftShowsPrec sp sl prec = \case
    LitF a ->
      ("LitF " <>) . showsPrec prec a
    IntUnOpF op a ->
      ("IntUnOpF " <>) . showsPrec prec op . space . showsPrec1 prec a
    IntBinOpF op a b ->
      ("IntBinOpF " <>) . showsPrec prec op . space . liftShowList sp sl [a, b]
    IntTestOpF op idx a ->
      ("IntTestOpF " <>) . showsPrec prec op . space . showsPrec1 prec idx
        . space
        . showsPrec1 prec a
    IntSetBitF op a idx val ->
      ("IntSetBitF " <>) . showsPrec prec op . space . showsPrec1 prec a
        . space
        . showsPrec1 prec idx
        . space
        . showsPrec1 prec val
    RoundF op a ->
      ("RoundF " <>) . showsPrec prec op . space . showsPrec1 prec a
    CastF a ->
      ("CastF " <>) . showsPrec1 prec a
    FPUnOpF op a ->
      ("FPUnOpF " <>) . showsPrec prec op . space . showsPrec1 prec a
    FPBinOpF op a b ->
      ("FPBinOpF " <>) . showsPrec prec op . space . liftShowList sp sl [a, b]
    FPTestOpF op a ->
      ("FPTestOpF " <>) . showsPrec prec op . space . showsPrec1 prec a
    BoolUnOpF op a ->
      ("BoolUnOpF " <>) . showsPrec prec op . space . showsPrec1 prec a
    BoolBinOpF op a b ->
      ("BoolBinOpF " <>) . showsPrec prec op . space . liftShowList sp sl [a, b]
    CmpOpF op a b ->
      ("CmpOpF " <>) . showsPrec prec op . space . showsPrec1 prec a
        . space
        . showsPrec1 prec b
    BranchF b t f ->
      ("BranchF " <>) . showsPrec1 prec b . space . showsPrec1 prec t
        . space
        . showsPrec1 prec f
    InputF a ->
      ("InputF " <>) . showsPrec prec a
    FunctionCallF call ->
      ("FunctionCallF " <>) . showsPrec prec call
    FunctionCallOutputF call idx ->
      ("FunctionCallOutputF " <>) . showsPrec1 prec call . space . showsPrec prec idx
    SelectF select whichBranch ->
      ("SelectF " <>) . showsPrec prec select . space . showsPrec1 prec whichBranch
    SelectOutputF sel idx ->
      ("SelectOutputF " <>) . showsPrec1 prec sel . space . showsPrec prec idx
    where
      space = (" " <>)

instance HTraversableC CExprTypeLens CExprF where
  htraverseC f = \case
    LitF prim -> pure $ LitF prim
    IntUnOpF op a -> IntUnOpF op <$> f a
    IntBinOpF op a b -> IntBinOpF op <$> f a <*> f b
    IntTestOpF op idx arg -> IntTestOpF op <$> f idx <*> f arg
    IntSetBitF op arg idx val -> IntSetBitF op <$> f arg <*> f idx <*> f val
    RoundF op arg -> RoundF op <$> f arg
    CastF arg -> CastF <$> f arg
    FPBinOpF op a b -> FPBinOpF op <$> f a <*> f b
    FPUnOpF op a -> FPUnOpF op <$> f a
    FPTestOpF op a -> FPTestOpF op <$> f a
    BoolBinOpF op a b -> BoolBinOpF op <$> f a <*> f b
    BoolUnOpF op a -> BoolUnOpF op <$> f a
    CmpOpF op a b -> CmpOpF op <$> f a <*> f b
    BranchF predicate yes no -> BranchF <$> f predicate <*> f yes <*> f no
    InputF idx -> pure $ InputF idx
    FunctionCallF funcall -> FunctionCallF <$> Barbies.btraverseC @CExprTypeLens f funcall
    FunctionCallOutputF funcall idx -> flip FunctionCallOutputF idx <$> f funcall
    SelectF select whichBranch ->
      SelectF <$> Barbies.btraverseC @CExprTypeLens f select <*> f whichBranch
    SelectOutputF sel idx -> flip SelectOutputF idx <$> f sel

instance HFoldableC CExprTypeLens CExprF where
  hfoldMapC = hfoldMapCDefault @CExprTypeLens

instance HFunctorC CExprTypeLens CExprF where
  hmapC = hmapCDefault @CExprTypeLens

instance Hashable1 r => Hashable1 (CExprF r) where
  liftHashWithSalt hashPrim s =
    (s `hashOp`) . \case
      LitF prim -> 0 `hashPrim` prim
      IntUnOpF op a -> 1 `hashOp` op `liftedHash` a
      IntBinOpF op a b -> 2 `hashOp` op `liftedHash` a `liftedHash` b
      IntTestOpF op idx arg -> 3 `hashOp` op `liftPrimHash` idx `liftPrimHash` arg
      IntSetBitF op arg idx val ->
        4 `hashOp` op `liftedHash` arg `liftPrimHash` idx `liftPrimHash` val
      RoundF op arg -> 5 `hashOp` op `liftPrimHash` arg
      CastF arg -> 6 `liftPrimHash` arg
      FPBinOpF op a b -> 7 `hashOp` op `liftedHash` a `liftedHash` b
      FPUnOpF op a -> 8 `hashOp` op `liftedHash` a
      FPTestOpF op a -> 9 `hashOp` op `liftPrimHash` a
      BoolBinOpF op a b -> 11 `hashOp` op `liftedHash` a `liftedHash` b
      BoolUnOpF op a -> 12 `hashOp` op `liftedHash` a
      CmpOpF op a b -> 13 `hashOp` op `liftPrimHash` a `liftPrimHash` b
      BranchF p t f -> 14 `liftPrimHash` p `liftedHash` t `liftedHash` f
      InputF idx -> 15 `hashWithSalt` idx
      FunctionCallF call -> 16 `hashOp` call
      FunctionCallOutputF cfcr idx -> liftHashWithSalt hashWithSalt 17 cfcr `hashWithSalt` idx
      SelectF sel whichBranch -> 18 `hashOp` sel `liftPrimHash` whichBranch
      SelectOutputF select idx -> liftHashWithSalt hashWithSalt 19 select `hashWithSalt` idx
    where
      infixl 4 `hashOp`
      hashOp :: forall b. Hashable b => Int -> b -> Int
      hashOp = hashWithSalt

      infixl 3 `liftedHash`
      liftedHash = liftHashWithSalt hashPrim

      infixl 3 `liftPrimHash`
      liftPrimHash :: forall a. IsPrimitive a => Int -> r a -> Int
      liftPrimHash = liftHashWithSalt (hashWithSalt @a)

type CExpr = HFix CExprF

-- | The type of a Haskell function between sets of CExpr expressions.  The calling convention for
-- all code-generated C functions is a set of input arrays, one per primitive, and a set of output
-- arrays, also one per primitive.  These correspond respectively to the 'Arrays' input to and
-- output from the Haskell function.
type CExprHaskellFunction m =
  Arrays (Compose Vector CExpr) -> m (Arrays (Compose Vector CExpr))

instance {-# OVERLAPPABLE #-} (KLiteral CExpr a, PrimIntegral a) => Num (CExpr a) where
  x + y = hembed $ IntBinOpF Add x y
  x - y = hembed $ IntBinOpF Sub x y
  x * y = hembed $ IntBinOpF Mul x y
  negate = hembed . IntUnOpF Negate
  abs = hembed . IntUnOpF Abs
  signum = hembed . IntUnOpF Signum
  fromInteger = kliteral . fromInteger

zero :: forall a. (IsPrimitive a) => CExpr a
zero = hembed (LitF 0)

one :: forall a. (IsPrimitive a) => CExpr a
one = hembed (LitF 1)

fpSignum :: forall a. (PrimAny a, Num (CExpr a), CExprTypeLens a) => CExpr a -> CExpr a
fpSignum x =
  -- This algorithm avoids checking for @== 0@, because all comparisons with @NaN@ will be false.
  hembed . BranchF (cmp CmpGT x 0) 1 . hembed . BranchF (cmp CmpLT x 0) (-1) $ x
  where
    cmp o a b = hembed (CmpOpF o a b)

instance Num (CExpr Double) where
  x + y = hembed $ FPBinOpF FPAdd x y
  x - y = hembed $ FPBinOpF FPSub x y
  x * y = hembed $ FPBinOpF FPMul x y
  negate = hembed . FPUnOpF FPNegate
  abs = hembed . FPUnOpF FPAbs
  signum = fpSignum
  fromInteger = kliteral . fromInteger

instance Num (CExpr Float) where
  x + y = hembed $ FPBinOpF FPAdd x y
  x - y = hembed $ FPBinOpF FPSub x y
  x * y = hembed $ FPBinOpF FPMul x y
  negate = hembed . FPUnOpF FPNegate
  abs = hembed . FPUnOpF FPAbs
  signum = fpSignum
  fromInteger = kliteral . fromInteger

type instance TargetOb Bool = CExpr Bool

type instance TargetOb Int8 = CExpr Int8

type instance TargetOb Int16 = CExpr Int16

type instance TargetOb Int32 = CExpr Int32

type instance TargetOb Int64 = CExpr Int64

type instance TargetOb Word8 = CExpr Word8

type instance TargetOb Word16 = CExpr Word16

type instance TargetOb Word32 = CExpr Word32

type instance TargetOb Word64 = CExpr Word64

type instance TargetOb Float = CExpr Float

type instance TargetOb Double = CExpr Double

type instance TargetOb Data.Semigroup.All = CExpr Bool

type instance TargetOb Data.Semigroup.Any = CExpr Bool

type instance TargetOb CExprFunctionCall = FunctionCall CExpr

type instance TargetOb CExprSelectExpression = (Select CExpr, CExpr Word8)

instance KConvertFloat CExpr where
  kFloatToDouble = hembed . CastF

  kDoubleToFloat = hembed . CastF

instance KLiteral CExpr Bool where
  kliteral = hembed . LitF

instance KLiteral CExpr Int8 where
  kliteral = hembed . LitF

instance KLiteral CExpr Int16 where
  kliteral = hembed . LitF

instance KLiteral CExpr Int32 where
  kliteral = hembed . LitF

instance KLiteral CExpr Int64 where
  kliteral = hembed . LitF

instance KLiteral CExpr Word8 where
  kliteral = hembed . LitF

instance KLiteral CExpr Word16 where
  kliteral = hembed . LitF

instance KLiteral CExpr Word32 where
  kliteral = hembed . LitF

instance KLiteral CExpr Word64 where
  kliteral = hembed . LitF

instance KLiteral CExpr Float where
  kliteral = hembed . LitF

instance KLiteral CExpr Double where
  kliteral = hembed . LitF

instance Fractional (CExpr Double) where
  x / y = hembed $ FPBinOpF FPDiv x y

  fromRational = kliteral . fromRational

instance Fractional (CExpr Float) where
  x / y = hembed $ FPBinOpF FPDiv x y

  fromRational = kliteral . fromRational

instance
  (KLiteral CExpr a, Fractional (CExpr a), Floating a, PrimFractional a) =>
  Floating (CExpr a)
  where
  pi = kliteral pi

  exp = hembed . FPUnOpF FPExp

  log = hembed . FPUnOpF FPLog

  sqrt = hembed . FPUnOpF FPSqrt

  x ** y = hembed $ FPBinOpF FPPow x y

  sin = hembed . FPUnOpF FPSin

  cos = hembed . FPUnOpF FPCos

  tan = hembed . FPUnOpF FPTan

  asin = hembed . FPUnOpF FPASin

  acos = hembed . FPUnOpF FPACos

  atan = hembed . FPUnOpF FPATan

  sinh = hembed . FPUnOpF FPSinh

  cosh = hembed . FPUnOpF FPCosh

  tanh = hembed . FPUnOpF FPTanh

  logBase base arg = log arg / log base

  asinh = hyperbolicFail

  acosh = hyperbolicFail

  atanh = hyperbolicFail

hyperbolicFail :: c
hyperbolicFail =
  error . unlines $
    [ "Hyperbolic inverse functions are not supported by CExpr at the moment,",
      "since their implementation in GHC differs so significantly from",
      "that in `libm` and is so badly behaved that they shouldn't be used",
      "in an abstract way (i.e. through the `Floating` class)."
    ]

instance KAnd CExpr where
  x .&& y = hembed $ BoolBinOpF BAnd x y
  x .|| y = hembed $ BoolBinOpF BOr x y
  kNot = hembed . BoolUnOpF BNot

safeIdx :: forall a. (PrimIntegral a, FiniteBits a) => Proxy a -> Int -> CExpr Word8
safeIdx proxyA idx
  | idx < 0 = error "I can only test bits with a non-negative index!"
  | idx > maxIndex =
    error $
      "The CExpr code generation system can only index up to "
        <> show numBits
        <> " bits (index "
        <> show maxIndex
        <> ") here.  Do you really have "
        <> show ((2 :: Integer) ^ idx)
        <> " bits (index "
        <> show idx
        <> ") in your "
        <> show (typeRep proxyA)
        <> "?"
  | otherwise = hembed $ LitF (fromIntegral idx :: Word8)
  where
    numBits = finiteBitSize (0 :: a)
    maxIndex = numBits - 1

instance (PrimIntegral a, FiniteBits a, CExprTypeLens a, Num (CExpr a)) => KBits CExpr a where
  testBit value idx = hembed $ IntTestOpF TestBit (safeIdx (Proxy @a) idx) value
  setBitTo value idx = hembed . IntSetBitF SetBit value (safeIdx (Proxy @a) idx)
  zeroBits = 0

kDivMod ::
  forall a.
  (PrimIntegral a, CExprTypeLens a) =>
  -- | What to do if the second argument is 0
  (CExpr a -> CExpr a) ->
  -- | What to do if the signum correction is needed
  (CExpr a -> CExpr a) ->
  -- | What to do otherwise
  (CExpr a -> CExpr a) ->
  -- | @x@
  CExpr a ->
  -- | @y@
  CExpr a ->
  -- | @x `div` y@ or @x `mod` y@
  CExpr a
kDivMod ifZero signumCorrection simpleCase x y =
  hembed $ BranchF testZero (ifZero zero) ifNotZero
  where
    testZero = hembed $ CmpOpF CmpEq zero y
    ifNotZero = hembed $ BranchF needSignumCorrection (signumCorrection r) (simpleCase r)
    needSignumCorrection = hembed $ CmpOpF CmpEq signumR negatedSignumY
    signumR = hembed $ IntUnOpF Signum r
    negatedSignumY = hembed . IntUnOpF Negate . hembed $ IntUnOpF Signum y
    r = hembed $ IntBinOpF Rem x y

instance (CExprTypeLens a, PrimIntegral a) => KDivisible (CExpr a) where
  kDiv x y = kDivMod id (const . hembed . IntBinOpF Sub q $ one) (const q) x y
    where
      q = hembed (IntBinOpF Quot x y)
  kMod x y = kDivMod (const x) (\r -> hembed (IntBinOpF Add r y)) id x y

instance (PrimTypeLens a, Ord a) => KEq CExpr (CExpr a) where
  x .== y = hembed $ CmpOpF CmpEq x y
  x ./= y = hembed $ CmpOpF CmpNeq x y

instance KEq CExpr () where
  _ .== _ = hembed $ LitF True
  _ ./= _ = hembed $ LitF False

instance (KEq CExpr a, KEq CExpr b) => KEq CExpr (a, b) where
  (a, b) .== (a', b') = hembed $ BoolBinOpF BAnd (a .== a') (b .== b')
  (a, b) ./= (a', b') = hembed $ BoolBinOpF BOr (a ./= a') (b ./= b')

instance KOrd CExpr () where
  _ .< _ = hembed $ LitF False
  _ .<= _ = hembed $ LitF True
  _ .> _ = hembed $ LitF False
  _ .>= _ = hembed $ LitF True
  kMin a _ = a
  kMax a _ = a

instance
  (KOrd CExpr a, KOrd CExpr b, PolyVec CExpr a, PolyVec CExpr b) =>
  KOrd CExpr (a, b)
  where
  (a, b) .< (a', b') =
    hembed $
      BoolBinOpF
        BOr
        (a .< a')
        (hembed $ BoolBinOpF BAnd (a .== a') (b .< b'))
  x .<= y = hembed $ BoolBinOpF BOr (x .== y) (x .< y)
  x .> y = hembed $ BoolUnOpF BNot (x .<= y)
  x .>= y = hembed $ BoolUnOpF BNot (x .< y)
  kMin x y = kIfThenElse (x .< y) x y
  kMax x y = kMin y x

instance (PrimFractional a, CExprTypeLens a) => KIsInfinite CExpr a where
  kIsInfinite = hembed . FPTestOpF FPIsInfinite

instance (PrimFractional a, CExprTypeLens a) => KIsNaN CExpr a where
  kIsNaN = hembed . FPTestOpF FPIsNaN

instance PrimNum b => KFromIntegral CExpr b where
  kFromIntegral :: forall a. PrimIntegral a => CExpr a -> CExpr b
  kFromIntegral a = case primGADT @a of
    GInt8 -> hembed $ CastF a
    GInt16 -> hembed $ CastF a
    GInt32 -> hembed $ CastF a
    GInt64 -> hembed $ CastF a
    GWord8 -> hembed $ CastF a
    GWord16 -> hembed $ CastF a
    GWord32 -> hembed $ CastF a
    GWord64 -> hembed $ CastF a

instance KIf CExpr

chooseExtremum :: PrimTypeLens a => CmpOp -> CExpr a -> CExpr a -> CExpr a
chooseExtremum op x y = hembed $ BranchF (hembed (CmpOpF op x y)) x y

instance {-# OVERLAPPABLE #-} (PrimTypeLens a, Ord a) => KOrd CExpr (CExpr a) where
  x .< y = hembed $ CmpOpF CmpLT x y
  x .<= y = hembed $ CmpOpF CmpLE x y
  x .> y = hembed $ CmpOpF CmpGT x y
  x .>= y = hembed $ CmpOpF CmpGE x y
  kMax = chooseExtremum CmpGE
  kMin = chooseExtremum CmpLE

instance KOrd CExpr (CExpr Double) where
  x .< y = hembed $ CmpOpF CmpLT x y
  x .<= y = hembed $ CmpOpF CmpLE x y
  x .> y = hembed $ CmpOpF CmpGT x y
  x .>= y = hembed $ CmpOpF CmpGE x y
  kMax x y = hembed $ FPBinOpF FPMax x y
  kMin x y = hembed $ FPBinOpF FPMin x y

instance KOrd CExpr (CExpr Float) where
  x .< y = hembed $ CmpOpF CmpLT x y
  x .<= y = hembed $ CmpOpF CmpLE x y
  x .> y = hembed $ CmpOpF CmpGT x y
  x .>= y = hembed $ CmpOpF CmpGE x y
  kMax x y = hembed $ FPBinOpF FPMax x y
  kMin x y = hembed $ FPBinOpF FPMin x y

instance
  (Bounded a, KLiteral CExpr a, PrimIntegral a) =>
  KRound CExpr a
  where
  kRoundDouble = safeRound roundDoubleToIntegral roundDoubleToIntegral roundInt64ToIntegral
    where
      roundDoubleToIntegral :: forall c. PrimIntegral c => CExpr Double -> CExpr c
      roundDoubleToIntegral = hembed . CastF . hembed . RoundF @_ @Double RoundToIntegral
      roundInt64ToIntegral = hembed . CastF

  kRoundFloat = safeRound roundFloatToIntegral roundFloatToIntegral roundInt64ToIntegral
    where
      roundFloatToIntegral :: forall c. PrimIntegral c => CExpr Float -> CExpr c
      roundFloatToIntegral = hembed . CastF . hembed . RoundF @_ @Float RoundToIntegral
      roundInt64ToIntegral = hembed . CastF

realSelectList ::
  NonEmpty (Arrays (Compose Vector CExpr)) -> CExpr Word8 -> Arrays (Compose Vector CExpr)
realSelectList choices@(c :| _) index = Barbies.bmapC @PrimTypeLens fillArray argSpec
  where
    argSpec = Barbies.bmap (Const . Vector.length . getCompose) c
    result = hembed $ SelectF (Select argSpec choices) index
    fillArray :: forall a. PrimTypeLens a => Const Int a -> Compose Vector CExpr a
    fillArray (Const n) = Compose . Vector.generate n $ hembed . SelectOutputF result

instance KSelect CExpr where
  selectList :: forall a. IsPrimitive a => [[CExpr a]] -> CExpr Word8 -> [CExpr a]
  selectList [] _ = error "You must provide at least one option!"
  selectList (a : as) idx =
    toList . getCompose . view prims_ $ realSelectList (fmap arr_a $ a :| as) idx
    where
      arr_a :: [CExpr a] -> Arrays (Compose Vector CExpr)
      arr_a xs = set (prims_ . _Wrapped) (Vector.fromList xs) $ Barbies.bpure (Compose mempty)

  unsafeBoolToZeroOrOne x = hembed $ BranchF x (hembed (LitF 1)) (hembed (LitF 0))

instance IsPrimitive a => KTernary CExpr (CExpr a) where
  kTernary predicate true false = hembed $ BranchF predicate true false

instance CGeneric (CExpr Bool) where
  type
    Rep (CExpr Bool) =
      CG.PrimRep "CExprBool" "Kitty.KTypes.CExpr" "cexpr-types" (CExpr Bool)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (CExpr Int8) where
  type
    Rep (CExpr Int8) =
      CG.PrimRep "CExprInt8" "Kitty.KTypes.CExpr" "cexpr-types" (CExpr Int8)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (CExpr Int16) where
  type
    Rep (CExpr Int16) =
      CG.PrimRep "CExprInt16" "Kitty.KTypes.CExpr" "cexpr-types" (CExpr Int16)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (CExpr Int32) where
  type
    Rep (CExpr Int32) =
      CG.PrimRep "CExprInt32" "Kitty.KTypes.CExpr" "cexpr-types" (CExpr Int32)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (CExpr Int64) where
  type
    Rep (CExpr Int64) =
      CG.PrimRep "CExprInt64" "Kitty.KTypes.CExpr" "cexpr-types" (CExpr Int64)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (CExpr Word8) where
  type
    Rep (CExpr Word8) =
      CG.PrimRep "CExprWord8" "Kitty.KTypes.CExpr" "cexpr-types" (CExpr Word8)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (CExpr Word16) where
  type
    Rep (CExpr Word16) =
      CG.PrimRep "CExprWord16" "Kitty.KTypes.CExpr" "cexpr-types" (CExpr Word16)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (CExpr Word32) where
  type
    Rep (CExpr Word32) =
      CG.PrimRep "CExprWord32" "Kitty.KTypes.CExpr" "cexpr-types" (CExpr Word32)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (CExpr Word64) where
  type
    Rep (CExpr Word64) =
      CG.PrimRep "CExprWord64" "Kitty.KTypes.CExpr" "cexpr-types" (CExpr Word64)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (CExpr Float) where
  type
    Rep (CExpr Float) =
      CG.PrimRep "CExprFloat" "Kitty.KTypes.CExpr" "cexpr-types" (CExpr Float)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (CExpr Double) where
  type
    Rep (CExpr Double) =
      CG.PrimRep "CExprDouble" "Kitty.KTypes.CExpr" "cexpr-types" (CExpr Double)
  from = CG.primFrom
  to = CG.primTo
