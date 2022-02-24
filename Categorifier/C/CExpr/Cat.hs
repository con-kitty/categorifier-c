{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- For @HasRep (a, b)@ and @HasRep (Either a b)@
{-# OPTIONS_GHC -Wno-orphans #-}
-- There are redundant constraints like @Ok Cat a@ and @Ok2 Cat a b@.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Categorifier.C.CExpr.Cat
  ( Cat (..),
    ToTargetOb (..),
    castK,
    cat,
    lowerCat,
    (.$),
    applyCat,
    curryCat,
    uncurryCat,
    (&&&$),
    exlCat,
    exrCat,
    apply2Cat,
    compose2Cat,
  )
where

import Categorifier.C.CExpr.Cat.TargetOb (TargetOb, TargetObTC1, TargetObW (..))
import Categorifier.C.CExpr.FunctionCall ()
import Categorifier.C.CExpr.Types (CExprTypeLens)
import Categorifier.C.CExpr.Types.Core (CExpr, CExprF (..))
import qualified Categorifier.C.CExpr.Types.Operations as Op
import Categorifier.C.CTypes.CGeneric.Plugin (Vectorizable (..))
import Categorifier.C.KTypes.BooleanLogic (KAnd (..))
import Categorifier.C.KTypes.Equality (KEq (..))
import Categorifier.C.KTypes.Function
  ( Callee (..),
    IsFunCall,
    KVariadicDevecAndApply,
    KVariadicVectorizeFunctionInputs,
    kForeignFunctionCall,
    kFunctionCall,
  )
import Categorifier.C.KTypes.KDivisible (kDiv, kMod)
import qualified Categorifier.C.KTypes.SwitchCase as SwitchCase
import Categorifier.C.KTypes.TotalOrder (KOrd (..))
import Categorifier.C.PolyVec (PolyVec)
import Categorifier.C.Prim (PrimAny, PrimFractional, PrimGADT (..), PrimIntegral, PrimNum, primGADT)
import Categorifier.C.Recursion (hembed)
import Categorifier.Category
  ( ForeignFunCallCat (..),
    ReferenceCat (..),
    RepCat (..),
    UnsafeCoerceCat (..),
  )
import Categorifier.Client (HasRep (..))
import Categorifier.Common.IO.Exception (Exception, impureThrow)
import Categorifier.ConCatExtensions
  ( ApplicativeCat (..),
    BindableCat (..),
    FixedCat (..),
    FloatingCat' (..),
    FloatingPointClassifyCat (..),
    FloatingPointConvertCat (..),
    IntegralCat' (..),
    LaxMonoidalFunctorCat (..),
    MonadCat (..),
    NumCat' (..),
    PowICat (..),
    RealToFracCat (..),
    SemigroupCat (..),
    TranscendentalCat (..),
    TraversableCat' (..),
    defaultTrace,
  )
import qualified Categorifier.UnconCat as UnconCat
import ConCat.Category
  ( AssociativePCat (..),
    BoolCat (..),
    BottomCat (..),
    BraidedPCat (..),
    Category (..),
    ClosedCat (..),
    ConstCat (..),
    CoproductCat (..),
    DistribCat (..),
    EqCat (..),
    FloatingCat (..),
    FractionalCat (..),
    FromIntegralCat (..),
    FunctorCat (..),
    IfCat (..),
    IntegralCat (..),
    MinMaxCat (..),
    MinMaxFunctorCat (..),
    MonoidalPCat (..),
    MonoidalSCat (..),
    NumCat (..),
    OkFunctor (..),
    OrdCat (..),
    PointedCat (..),
    ProductCat (..),
    RepresentableCat (..),
    Strong (..),
    TerminalCat (..),
    TracedCat (..),
    TraversableCat (..),
    (&&&),
  )
import Control.Applicative (liftA2)
import Control.Monad ((<=<))
import Data.Bifunctor (bimap)
import Data.Coerce (Coercible, coerce)
import Data.Either.Extra (fromEither)
import Data.Either.Validation (Validation)
import Data.Foldable (toList)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Rep (Representable)
import qualified Data.Functor.Rep as Representable
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (..))
import qualified Data.Semigroup
import Data.Text (Text)
import Data.Tuple.Extra (dupe)
import Data.Typeable (Typeable)
import qualified Data.Typeable as Typeable
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics ((:*:) (..), (:+:) (..))
import qualified GHC.Generics as G
import Prelude hiding (and, const, curry, id, not, pred, uncurry, (.))

-- so that it is legal to import Unsafe.Coerce
{-# ANN module "HLint: ignore Avoid restricted module" #-}

-- | Arrow type for code generation. The frontend takes, for instance,
-- @ControlInputs C -> ControlOutputs C@, and converts it into
-- @ControlInputs C `Cat` ControlOutputs C@.
--
-- TODO: make @k1@ and @k2@ the same kind (SW-3538).
newtype Cat (a :: k1) (b :: k2) = Cat {runCat :: TargetObW a -> TargetObW b}

-- | Construct `Cat` from `TargetOb`.
cat :: TargetOb (a -> b) -> Cat a b
cat f = Cat $ TargetObW . f . unTargetObW

lowerCat :: Cat a b -> TargetOb a -> TargetOb b
lowerCat (Cat f) = unTargetObW . f . TargetObW

-- * TargetOb1

-- | The reason to have 'TargetOb1Aux' is because
--
-- @class (forall a. TargetOb (f a) ~ f (TargetOb a)) => TargetOb1 f@
--
-- is not valid haskell (@~@ cannot be used with QuantifiedConstraints).
class (TargetOb (f a) ~ f (TargetOb a)) => TargetOb1Aux f a

instance (TargetOb (f a) ~ f (TargetOb a)) => TargetOb1Aux f a

class (forall a. TargetOb1Aux f a) => TargetOb1 f

instance (forall a. TargetOb1Aux f a) => TargetOb1 f

-- | The constraint cannot be 'TargetOb1 f', because from 'TargetOb1 f', GHC is unable
-- to deduce 'TargetOb1Aux f a'.
fromTargetOb1 :: TargetOb1Aux f a => Proxy a -> TargetOb (f a) -> f (TargetOb a)
fromTargetOb1 Proxy = id

toTargetOb1 :: TargetOb1Aux f a => Proxy a -> f (TargetOb a) -> TargetOb (f a)
toTargetOb1 Proxy = id

------------------------------------------------------------------------------

instance
  {-# OVERLAPPABLE #-}
  ( KVariadicDevecAndApply isFunCall CExpr (TargetOb a -> TargetOb b),
    KVariadicVectorizeFunctionInputs isFunCall CExpr (TargetOb a -> TargetOb b),
    isFunCall ~ IsFunCall (TargetOb a -> TargetOb b)
  ) =>
  ReferenceCat Cat (a :: Type) (b :: Type)
  where
  indirection name = cat . kFunctionCall (Proxy @CExpr) name . lowerCat

-- | This is for the @n \`isFreeIn\` fn@ case of @kFunctionCall@, such as
-- @\n -> kFunctionCall (f n) a@.
instance
  ( KVariadicDevecAndApply isFunCall CExpr (TargetOb a -> TargetOb b),
    KVariadicVectorizeFunctionInputs isFunCall CExpr (TargetOb a -> TargetOb b),
    isFunCall ~ IsFunCall (TargetOb a -> TargetOb b)
  ) =>
  -- TODO (SW-4070): instead of `(x, a)`, we should use a custom pair type that is only
  -- used by the plugin, such as `CategorifierCInternalPair x a`.
  ReferenceCat Cat (x, a) (b :: Type)
  where
  indirection name f =
    cat . uncurry $
      kFunctionCall (Proxy @CExpr) name . curry (lowerCat f)

instance
  ( KVariadicVectorizeFunctionInputs isFunCall CExpr (TargetOb a -> TargetOb b),
    isFunCall ~ IsFunCall (TargetOb a -> TargetOb b)
  ) =>
  ForeignFunCallCat Cat (Text, Callee) a b
  where
  ffcall = (cat .) . uncurry (kForeignFunctionCall (Proxy @CExpr))

-- * "ConCat.Category" & "Categorifier.ConCatExtensions" instances

instance Category Cat where
  id = UnconCat.id
  (.) = (UnconCat..)

instance ProductCat Cat where
  exl = UnconCat.exl
  exr = UnconCat.exr
  dup = UnconCat.dup

instance CoproductCat Cat where
  inl = UnconCat.inl
  inr = UnconCat.inr
  jam = UnconCat.jam

instance MonoidalPCat Cat where
  (***) = (UnconCat.***)

instance MonoidalSCat Cat where
  f +++ g = cat $ bimap (lowerCat f) (lowerCat g)

instance ClosedCat Cat where
  apply = UnconCat.apply
  curry = UnconCat.curry

instance AssociativePCat Cat

instance DistribCat Cat where
  distl = cat $ \(a, b) -> bimap (a,) (a,) b

instance OkFunctor Cat h where
  okFunctor = okFunctor @Cat

instance TerminalCat Cat where
  it = const ()

instance BraidedPCat Cat

instance {-# OVERLAPPABLE #-} ToTargetOb a => ConstCat Cat a where
  const = Cat . const . toTargetOb

instance {-# OVERLAPPABLE #-} (Functor f, TargetOb1 f) => FunctorCat Cat f where
  fmapC :: forall a b. a `Cat` b -> f a `Cat` f b
  fmapC f = cat $ toTargetOb1 @f (Proxy @b) . fmap (lowerCat f) . fromTargetOb1 (Proxy @a)

  unzipC = fmapC exl &&& fmapC exr

instance FunctorCat Cat (Const a) where
  fmapC _ = cat $ \(Const a) -> Const a
  unzipC = fmapC exl &&& fmapC exr

instance {-# OVERLAPPABLE #-} (Applicative f, TargetOb1 f) => LaxMonoidalFunctorCat Cat f where
  liftA2K :: forall a b c. (a, b) `Cat` c -> (f a, f b) `Cat` f c
  liftA2K f =
    cat $
      toTargetOb1 @f (Proxy @c) . (uncurry . liftA2 . curry $ lowerCat f)
        . (fromTargetOb1 (Proxy @a) *** fromTargetOb1 (Proxy @b))

instance {-# OVERLAPPABLE #-} (Applicative f, TargetOb1 f) => ApplicativeCat Cat f where
  apK :: forall a b. (f (a -> b), f a) `Cat` f b
  apK =
    cat $
      toTargetOb1 @f (Proxy @b)
        . uncurry (<*>)
        . (fromTargetOb1 (Proxy @(a -> b)) *** fromTargetOb1 (Proxy @a))

instance {-# OVERLAPPABLE #-} (Monad m, TargetOb1 m) => MonadCat Cat m where
  joinK :: forall a. m (m a) `Cat` m a
  joinK =
    cat $ toTargetOb1 @m (Proxy @a) . (fromTargetOb1 (Proxy @a) <=< fromTargetOb1 (Proxy @(m a)))

instance {-# OVERLAPPABLE #-} (Monad m, TargetOb1 m) => BindableCat Cat m where
  bindK :: forall a b. (m a, a -> m b) `Cat` m b
  bindK =
    cat $
      toTargetOb1 @m (Proxy @b)
        . uncurry (>>=)
        . (fromTargetOb1 (Proxy @a) *** (fromTargetOb1 (Proxy @b) .))

instance (Traversable t, TargetOb1 t, Applicative f, TargetOb1 f) => TraversableCat Cat t f where
  sequenceAC :: forall a. t (f a) `Cat` f (t a)
  sequenceAC =
    cat $
      toTargetOb1 @f (Proxy @(t a))
        . fmap (toTargetOb1 (Proxy @a))
        . sequenceA
        . fmap (fromTargetOb1 (Proxy @a))
        . fromTargetOb1 @t (Proxy @(f a))

instance (Traversable t, TargetOb1 t, Applicative f, TargetOb1 f) => TraversableCat' Cat t f where
  traverseK :: forall a b. a `Cat` f b -> t a `Cat` f (t b)
  traverseK f =
    cat $
      toTargetOb1 @f (Proxy @(t b)) . fmap (toTargetOb1 (Proxy @b))
        . traverse (fromTargetOb1 (Proxy @b) . lowerCat f)
        . fromTargetOb1 @t (Proxy @a)

instance {-# OVERLAPPABLE #-} (Functor f, TargetOb1 f) => Strong Cat f where
  strength :: forall a b. (a, f b) `Cat` f (a, b)
  strength =
    cat $ toTargetOb1 @f (Proxy @(a, b)) . uncurry fmap . ((,) *** fromTargetOb1 (Proxy @b))

instance Strong Cat (Const a) where
  strength = cat $ \(_, Const a) -> Const a

instance {-# OVERLAPPABLE #-} Semigroup (TargetOb a) => SemigroupCat Cat a where
  appendK = cat $ uncurry (<>)

instance SemigroupCat Cat Data.Semigroup.All where
  appendK = cat $ uncurry (.&&)

instance SemigroupCat Cat Data.Semigroup.Any where
  appendK = cat $ uncurry (.||)

instance TracedCat Cat where
  trace = defaultTrace

instance FixedCat Cat where
  fixK :: forall a x. Cat (a, x) x -> Cat a x
  fixK f = cat . fixK $ lowerCat f

instance
  (TargetOb1 f, Representable f, TargetOb (Representable.Rep f) ~ Representable.Rep f) =>
  RepresentableCat Cat f
  where
  tabulateC :: forall a. (Representable.Rep f -> a) `Cat` f a
  tabulateC = cat $ toTargetOb1 @f (Proxy @a) . Representable.tabulate

  indexC :: forall a. f a `Cat` (Representable.Rep f -> a)
  indexC = cat $ Representable.index . fromTargetOb1 @f (Proxy @a)

instance {-# OVERLAPPABLE #-} (TargetOb1 f, Applicative f) => PointedCat Cat f a where
  pointC = cat $ toTargetOb1 @f (Proxy @a) . pure

instance (HasRep a, r ~ Rep a, Vectorizable (TargetOb r) (TargetOb a)) => RepCat Cat a r where
  reprC = cat unmakeVectorizable
  abstC = cat makeVectorizable

instance {-# OVERLAPPABLE #-} KEq CExpr (TargetOb a) => EqCat Cat a where
  equal = cat $ uncurry (.==)
  notEqual = cat $ uncurry (./=)

-- This is for `EqCat (Foo (C Double))` where `newtype Foo a = Foo a`.
instance
  {-# OVERLAPPABLE #-}
  (HasRep (TargetOb (f a)), KEq CExpr (Rep (TargetOb (f a)))) =>
  EqCat Cat (f a)
  where
  equal = cat $ \(a, b) -> repr a .== repr b
  notEqual = cat $ \(a, b) -> repr a .== repr b

-- |
--
--  __NB__: The @`Ord` a@ constraint here is redundant, but forced by the `OrdCat` class definition.
instance
  {-# OVERLAPPABLE #-}
  ( Ord a,
    EqCat Cat a,
    HasRep (TargetOb a),
    KOrd CExpr (Rep (TargetOb a))
  ) =>
  OrdCat Cat a
  where
  lessThan = cat $ \(a, b) -> repr a .< repr b
  greaterThan = cat $ \(a, b) -> repr a .> repr b
  lessThanOrEqual = cat $ \(a, b) -> repr a .<= repr b
  greaterThanOrEqual = cat $ \(a, b) -> repr a .>= repr b

lessThanPrim :: (PrimAny a, CExprTypeLens a, TargetOb a ~ CExpr a) => (a, a) `Cat` Bool
lessThanPrim = binop (CmpOpF Op.CmpLT)

greaterThanPrim :: (PrimAny a, CExprTypeLens a, TargetOb a ~ CExpr a) => (a, a) `Cat` Bool
greaterThanPrim = binop (CmpOpF Op.CmpGT)

lessThanOrEqualPrim :: (PrimAny a, CExprTypeLens a, TargetOb a ~ CExpr a) => (a, a) `Cat` Bool
lessThanOrEqualPrim = binop (CmpOpF Op.CmpLE)

greaterThanOrEqualPrim :: (PrimAny a, CExprTypeLens a, TargetOb a ~ CExpr a) => (a, a) `Cat` Bool
greaterThanOrEqualPrim = binop (CmpOpF Op.CmpGE)

instance OrdCat Cat Bool where
  lessThan = lessThanPrim
  greaterThan = greaterThanPrim
  lessThanOrEqual = lessThanOrEqualPrim
  greaterThanOrEqual = greaterThanOrEqualPrim

instance OrdCat Cat Double where
  lessThan = lessThanPrim
  greaterThan = greaterThanPrim
  lessThanOrEqual = lessThanOrEqualPrim
  greaterThanOrEqual = greaterThanOrEqualPrim

instance OrdCat Cat Float where
  lessThan = lessThanPrim
  greaterThan = greaterThanPrim
  lessThanOrEqual = lessThanOrEqualPrim
  greaterThanOrEqual = greaterThanOrEqualPrim

instance OrdCat Cat Int8 where
  lessThan = lessThanPrim
  greaterThan = greaterThanPrim
  lessThanOrEqual = lessThanOrEqualPrim
  greaterThanOrEqual = greaterThanOrEqualPrim

instance OrdCat Cat Int16 where
  lessThan = lessThanPrim
  greaterThan = greaterThanPrim
  lessThanOrEqual = lessThanOrEqualPrim
  greaterThanOrEqual = greaterThanOrEqualPrim

instance OrdCat Cat Int32 where
  lessThan = lessThanPrim
  greaterThan = greaterThanPrim
  lessThanOrEqual = lessThanOrEqualPrim
  greaterThanOrEqual = greaterThanOrEqualPrim

instance OrdCat Cat Int64 where
  lessThan = lessThanPrim
  greaterThan = greaterThanPrim
  lessThanOrEqual = lessThanOrEqualPrim
  greaterThanOrEqual = greaterThanOrEqualPrim

instance OrdCat Cat Word8 where
  lessThan = lessThanPrim
  greaterThan = greaterThanPrim
  lessThanOrEqual = lessThanOrEqualPrim
  greaterThanOrEqual = greaterThanOrEqualPrim

instance OrdCat Cat Word16 where
  lessThan = lessThanPrim
  greaterThan = greaterThanPrim
  lessThanOrEqual = lessThanOrEqualPrim
  greaterThanOrEqual = greaterThanOrEqualPrim

instance OrdCat Cat Word32 where
  lessThan = lessThanPrim
  greaterThan = greaterThanPrim
  lessThanOrEqual = lessThanOrEqualPrim
  greaterThanOrEqual = greaterThanOrEqualPrim

instance OrdCat Cat Word64 where
  lessThan = lessThanPrim
  greaterThan = greaterThanPrim
  lessThanOrEqual = lessThanOrEqualPrim
  greaterThanOrEqual = greaterThanOrEqualPrim

instance BoolCat Cat where
  notC = unop (BoolUnOpF Op.BNot)
  andC = binop (BoolBinOpF Op.BAnd)
  orC = binop (BoolBinOpF Op.BOr)
  xorC = impureThrow XorCNotSupported

instance (Num (TargetOb a), Typeable a) => NumCat Cat a where
  negateC = cat negate
  addC = cat $ uncurry (+)
  subC = cat $ uncurry (-)
  mulC = cat $ uncurry (*)
  powIC = cat $ uncurry (^)

instance Num (TargetOb a) => PowICat Cat (a :: Type) where
  powIK i = cat (^ i)

instance Num (TargetOb a) => NumCat' Cat a where
  absK = cat abs
  signumK = cat signum

instance Floating (TargetOb a) => FloatingCat' Cat a where
  powK = cat $ uncurry (**)

instance
  {-# OVERLAPPABLE #-}
  ( PrimIntegral a,
    PrimNum b,
    CExprTypeLens a,
    CExprTypeLens b,
    TargetOb a ~ CExpr a,
    TargetOb b ~ CExpr b
  ) =>
  FromIntegralCat Cat a b
  where
  fromIntegralC = castK

instance
  {-# OVERLAPPABLE #-}
  (PrimNum a, CExprTypeLens a, ToTargetOb a) =>
  FromIntegralCat Cat Integer a
  where
  fromIntegralC = Cat $ toTargetOb . fromIntegralC . unTargetObW

instance
  {-# OVERLAPPABLE #-}
  ( PrimNum a,
    PrimFractional b,
    CExprTypeLens a,
    CExprTypeLens b,
    TargetOb a ~ CExpr a,
    TargetOb b ~ CExpr b
  ) =>
  RealToFracCat Cat a b
  where
  realToFracK = castK

instance FloatingPointConvertCat Cat where
  floatToDoubleK = castK
  doubleToFloatK = castK

instance
  (PrimFractional a, CExprTypeLens a, Num (TargetOb a), TargetOb a ~ CExpr a) =>
  FloatingPointClassifyCat Cat a
  where
  isNegativeZeroK =
    unop
      ( \x ->
          BoolBinOpF
            Op.BAnd
            (hembed $ FPTestOpF Op.FPSignBit x)
            ( hembed $
                CmpOpF
                  Op.CmpEq
                  (hembed $ LitF 0)
                  x
            )
      )
  isInfiniteK = unop (FPTestOpF Op.FPIsInfinite)
  isFiniteK = unop (BoolUnOpF Op.BNot . hembed . FPTestOpF Op.FPIsInfinite)
  isNaNK = unop (FPTestOpF Op.FPIsNaN)
  isDenormalK = impureThrow . IsDenormalKNotSupported . Typeable.typeRep $ Proxy @a

instance Floating (TargetOb a) => FloatingCat Cat a where
  expC = cat exp
  logC = cat log
  cosC = cat cos
  sinC = cat sin
  sqrtC = cat sqrt

instance
  {-# OVERLAPPABLE #-}
  (PrimIntegral a, CExprTypeLens a, TargetOb a ~ CExpr a) =>
  IntegralCat Cat a
  where
  divC = cat $ uncurry kDiv
  modC = cat $ uncurry kMod

instance
  {-# OVERLAPPABLE #-}
  (PrimIntegral a, CExprTypeLens a, TargetOb a ~ CExpr a) =>
  IntegralCat' Cat a
  where
  quotK = binop (IntBinOpF Op.Quot)
  remK = binop (IntBinOpF Op.Rem)

instance Fractional (TargetOb a) => FractionalCat Cat a where
  divideC = cat $ uncurry (/)
  recipC = cat recip

instance (Floating (TargetOb a), Typeable a) => TranscendentalCat Cat a where
  tanK = cat tan
  asinK = cat asin
  acosK = cat acos
  atanK = cat atan
  sinhK = cat sinh
  coshK = cat cosh
  tanhK = cat tanh
  asinhK = impureThrow . ASinhNotSupported . Typeable.typeRep $ Proxy @a
  acoshK = impureThrow . ACoshNotSupported . Typeable.typeRep $ Proxy @a
  atanhK = impureThrow . ATanhNotSupported . Typeable.typeRep $ Proxy @a

instance {-# OVERLAPPABLE #-} (PrimNum a, CExprTypeLens a) => MinMaxCat Cat a where
  minC = case primGADT @a of
    GInt8 -> minmax lessThan
    GInt16 -> minmax lessThan
    GInt32 -> minmax lessThan
    GInt64 -> minmax lessThan
    GWord8 -> minmax lessThan
    GWord16 -> minmax lessThan
    GWord32 -> minmax lessThan
    GWord64 -> minmax lessThan
    GFloat -> binop (FPBinOpF Op.FPMin)
    GDouble -> binop (FPBinOpF Op.FPMin)
  maxC = case primGADT @a of
    GInt8 -> minmax greaterThan
    GInt16 -> minmax greaterThan
    GInt32 -> minmax greaterThan
    GInt64 -> minmax greaterThan
    GWord8 -> minmax greaterThan
    GWord16 -> minmax greaterThan
    GWord32 -> minmax greaterThan
    GWord64 -> minmax greaterThan
    GFloat -> binop (FPBinOpF Op.FPMax)
    GDouble -> binop (FPBinOpF Op.FPMax)

minmax :: forall a. IfCat Cat a => Cat (a, a) Bool -> Cat (a, a) a
minmax cmp = Cat $ runCat ifC . uncurry zipTargetObW . (runCat cmp &&& id)

instance
  (Functor f, Foldable f, MinMaxCat Cat a, TargetOb (f a) ~ f (TargetOb a)) =>
  MinMaxFunctorCat Cat f a
  where
  minimumC = cat $ \xs -> case toList xs of
    [] -> error "minimumC: empty structure"
    y0 : ys -> foldr (curry $ lowerCat @_ @a minC) y0 ys
  maximumC = cat $ \xs -> case toList xs of
    [] -> error "maximumC: empty structure"
    y0 : ys -> foldr (curry $ lowerCat @_ @a maxC) y0 ys

instance IfCat Cat Bool where
  ifC = ifKPrim

instance IfCat Cat Int8 where
  ifC = ifKPrim

instance IfCat Cat Int16 where
  ifC = ifKPrim

instance IfCat Cat Int32 where
  ifC = ifKPrim

instance IfCat Cat Int64 where
  ifC = ifKPrim

instance IfCat Cat Word8 where
  ifC = ifKPrim

instance IfCat Cat Word16 where
  ifC = ifKPrim

instance IfCat Cat Word32 where
  ifC = ifKPrim

instance IfCat Cat Word64 where
  ifC = ifKPrim

instance IfCat Cat Float where
  ifC = ifKPrim

instance IfCat Cat Double where
  ifC = ifKPrim

instance {-# OVERLAPPABLE #-} PolyVec CExpr (TargetOb a) => IfCat Cat a where
  ifC = cat $ \(b, (t, f)) -> SwitchCase.kIfThenElse b t f

ifKPrim :: forall a. (PrimAny a, CExprTypeLens a, TargetOb a ~ CExpr a) => Cat (Bool, (a, a)) a
ifKPrim = cat $ \(cond, (t, f)) -> hembed $ BranchF cond t f

-- | There _should_ be a `Coercible (TargetOb a) (TargetOb b)` constraint, which would allow us to
--   use `coerce` rather than `unsafeCoerce`. But this leads to errors in categorification due to
--   data constructors not being in scope.
instance UnsafeCoerceCat Cat (a :: Type) (b :: Type) where
  unsafeCoerceK = cat unsafeCoerceK

instance Exception (TargetOb a) => BottomCat Cat (a :: Type) (b :: Type) where
  bottomC = cat impureThrow

------------------------------------------------------------------------------

-- * "UnconCat" instances

instance UnconCat.Category Cat where
  id = Cat id
  Cat f . Cat g = Cat (f . g)

instance UnconCat.AssociativePCat Cat

instance UnconCat.ClosedCat Cat where
  apply = cat $ uncurry ($)
  curry = cat . curry . lowerCat

instance UnconCat.CoproductCat Cat where
  inl = cat Left
  inr = cat Right
  jam = cat fromEither

instance UnconCat.MonoidalPCat Cat where
  f *** g = cat $ bimap (lowerCat f) (lowerCat g)

instance UnconCat.ProductCat Cat where
  exl = cat fst
  exr = cat snd
  dup = Cat $ uncurry zipTargetObW . dupe

------------------------------------------------------------------------------

-- * Helper functions

unop ::
  forall a b.
  (TargetOb a ~ CExpr a, TargetOb b ~ CExpr b) =>
  (CExpr a -> CExprF CExpr b) ->
  Cat a b
unop op = cat (hembed . op)

binop ::
  forall a b c.
  (TargetOb a ~ CExpr a, TargetOb b ~ CExpr b, TargetOb c ~ CExpr c) =>
  (CExpr a -> CExpr b -> CExprF CExpr c) ->
  Cat (a, b) c
binop op = cat (hembed . uncurry op)

castK ::
  forall a b.
  ( PrimNum a,
    PrimNum b,
    CExprTypeLens a,
    CExprTypeLens b,
    TargetOb a ~ CExpr a,
    TargetOb b ~ CExpr b
  ) =>
  Cat a b
castK = cat $ hembed . CastF @a @b

zipTargetObW :: TargetObW a -> TargetObW b -> TargetObW (a, b)
zipTargetObW (TargetObW x) (TargetObW y) = TargetObW (x, y)

------------------------------------------------------------------------------

-- * ToTargetOb

class ToTargetOb a where
  toTargetOb :: a -> TargetObW a

instance
  {-# OVERLAPPABLE #-}
  ( HasRep a,
    HasRep (TargetOb a),
    Rep (TargetOb a) ~ TargetOb (Rep a),
    ToTargetOb (Rep a)
  ) =>
  ToTargetOb a
  where
  toTargetOb = runCat (cat abst :: Cat (Rep a) a) . toTargetOb . repr

instance ToTargetOb () where
  toTargetOb = TargetObW

instance ToTargetOb Char where
  toTargetOb = TargetObW

instance ToTargetOb Int where
  toTargetOb = TargetObW

instance ToTargetOb Integer where
  toTargetOb = TargetObW

instance ToTargetOb Bool where
  toTargetOb = TargetObW . hembed . LitF

instance ToTargetOb Int8 where
  toTargetOb = TargetObW . hembed . LitF

instance ToTargetOb Int16 where
  toTargetOb = TargetObW . hembed . LitF

instance ToTargetOb Int32 where
  toTargetOb = TargetObW . hembed . LitF

instance ToTargetOb Int64 where
  toTargetOb = TargetObW . hembed . LitF

instance ToTargetOb Word8 where
  toTargetOb = TargetObW . hembed . LitF

instance ToTargetOb Word16 where
  toTargetOb = TargetObW . hembed . LitF

instance ToTargetOb Word32 where
  toTargetOb = TargetObW . hembed . LitF

instance ToTargetOb Word64 where
  toTargetOb = TargetObW . hembed . LitF

instance ToTargetOb Float where
  toTargetOb = TargetObW . hembed . LitF

instance ToTargetOb Double where
  toTargetOb = TargetObW . hembed . LitF

instance ToTargetOb Data.Semigroup.All where
  toTargetOb = TargetObW . hembed . LitF . Data.Semigroup.getAll

instance ToTargetOb Data.Semigroup.Any where
  toTargetOb = TargetObW . hembed . LitF . Data.Semigroup.getAny

instance (ToTargetOb a, ToTargetOb b) => ToTargetOb (a, b) where
  toTargetOb =
    TargetObW . bimap (unTargetObW . toTargetOb) (unTargetObW . toTargetOb)

instance (ToTargetOb a, ToTargetOb b) => ToTargetOb (Either a b) where
  toTargetOb =
    TargetObW . bimap (unTargetObW . toTargetOb) (unTargetObW . toTargetOb)

instance (ToTargetOb a, ToTargetOb b) => ToTargetOb (Validation a b) where
  toTargetOb =
    TargetObW . bimap (unTargetObW . toTargetOb) (unTargetObW . toTargetOb)

instance ToTargetOb a => ToTargetOb (() -> a) where
  toTargetOb f = TargetObW . const . unTargetObW . toTargetOb $ f ()

instance ToTargetOb a => ToTargetOb [a] where
  toTargetOb = TargetObW . fmap (unTargetObW . toTargetOb)

instance ToTargetOb a => ToTargetOb (Identity a) where
  toTargetOb (Identity a) = TargetObW . Identity . unTargetObW $ toTargetOb a

instance ToTargetOb a => ToTargetOb (NonEmpty a) where
  toTargetOb = TargetObW . fmap (unTargetObW . toTargetOb)

instance
  (ToTargetOb (f p), Coercible (TargetOb (f p)) (TargetOb (G.M1 i c f p))) =>
  ToTargetOb (G.M1 i c f p)
  where
  toTargetOb (G.M1 a) = TargetObW . coerce . unTargetObW $ toTargetOb a

instance ToTargetOb c => ToTargetOb (G.K1 i c p) where
  toTargetOb (G.K1 a) = TargetObW . coerce . unTargetObW $ toTargetOb a

instance
  ( TargetOb (f p) ~ TargetObTC1 f p,
    TargetOb (g p) ~ TargetObTC1 g p,
    ToTargetOb (f (p :: Type)),
    ToTargetOb (g p)
  ) =>
  ToTargetOb ((f :*: g) p)
  where
  toTargetOb (a :*: b) = runCat (cat abst) $ toTargetOb (a, b)

instance
  ( TargetOb (f p) ~ TargetObTC1 f p,
    TargetOb (g p) ~ TargetObTC1 g p,
    ToTargetOb (f (p :: Type)),
    ToTargetOb (g p)
  ) =>
  ToTargetOb ((f :+: g) p)
  where
  toTargetOb =
    runCat (cat abst) . toTargetOb . \case
      L1 a -> Left a
      R1 b -> Right b

------------------------------------------------------------------------------

-- * HasRep instances

-- The following instances are created in order to satisfy the @Rep (TargetOb b) ~ TargetOb (Rep b)@
-- constraint. They are not supposed to be used by `categorifyLambda` to handle non-standard
-- data types, because they would obviously result in infinite loops.

instance HasRep (a, b) where
  type Rep (a, b) = (a, b)

  abst = id
  {-# INLINE abst #-}

  repr = id
  {-# INLINE repr #-}

instance HasRep (Either a b) where
  type Rep (Either a b) = Either a b

  abst = id
  {-# INLINE abst #-}

  repr = id
  {-# INLINE repr #-}

instance HasRep () where
  type Rep () = ()

  abst = id
  {-# INLINE abst #-}

  repr = id
  {-# INLINE repr #-}

------------------------------------------------------------------------------

-- * Error type

data NotSupported
  = IsDenormalKNotSupported Typeable.TypeRep
  | XorCNotSupported
  | ASinhNotSupported Typeable.TypeRep
  | ACoshNotSupported Typeable.TypeRep
  | ATanhNotSupported Typeable.TypeRep
  deriving (Show)

instance Exception NotSupported

------------------------------------------------------------------------------

-- * Category operations specialized for `Cat`

(.$) ::
  forall (b :: Type) (c :: Type) (a :: Type).
  Cat b c ->
  Cat a b ->
  Cat a c
(.$) = (UnconCat..)

applyCat :: forall a b. Cat (a -> b, a) b
applyCat = UnconCat.apply

curryCat ::
  forall (a :: Type) (b :: Type) (c :: Type).
  Cat (a, b) c ->
  Cat a (b -> c)
curryCat = UnconCat.curry

uncurryCat ::
  forall (a :: Type) (b :: Type) (c :: Type).
  Cat a (b -> c) ->
  Cat (a, b) c
uncurryCat = UnconCat.uncurry

(&&&$) ::
  forall (a :: Type) (b :: Type) (c :: Type).
  Cat a b ->
  Cat a c ->
  Cat a (b, c)
(&&&$) = (UnconCat.&&&)

exlCat :: forall a b. Cat (a, b) a
exlCat = UnconCat.exl

exrCat :: forall a b. Cat (a, b) b
exrCat = UnconCat.exr

apply2Cat :: forall (x :: Type) a b. Cat x (a -> b) -> Cat x a -> Cat x b
apply2Cat = UnconCat.apply2

compose2Cat :: forall (x :: Type) b c a. Cat x (b -> c) -> Cat x (a -> b) -> Cat x (a -> c)
compose2Cat = UnconCat.compose2
