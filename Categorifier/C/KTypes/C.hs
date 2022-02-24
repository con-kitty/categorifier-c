{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- redundant constraints allow us to make `coerceC` safer than the underlying `unsafeCoerceK`
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Categorifier.C.KTypes.C
  ( C (..),
    fromKEnum,
    unCBool,
    (!!?),
    IndexOutOfBounds,

    -- * __TODO__: move this downstream
    toCxxTypeViaC,
  )
where

import Accessors (Lookup (..))
import qualified Barbies
-- NB: This is an internal GHC API. It hasn't changed for years at this point, but it's possible it
-- could go away and need to be replaced by direct C casting.

import Categorifier.C.CExpr.Cat (Cat (..), ToTargetOb (..), castK, cat)
import Categorifier.C.CExpr.Cat.TargetOb (TargetOb, TargetObTC1, TargetObW (..))
import Categorifier.C.CExpr.Types (CExprTypeLens)
import Categorifier.C.CExpr.Types.Core (CExpr)
import Categorifier.C.CTypes.CGeneric.Class (CGeneric)
import qualified Categorifier.C.CTypes.CGeneric.Class as CG
import Categorifier.C.CTypes.GArrays (FromArraysError, GArrays (..))
import Categorifier.C.CTypes.Instances (PrimitivesToCxxType)
import Categorifier.C.CTypes.ToCxxType (CoercibleT, ToCxxType (..))
import Categorifier.C.CTypes.Types (CxxType, SupportsKBits)
import Categorifier.C.KTypes.BooleanLogic (KAnd (..))
import Categorifier.C.KTypes.Conditional (KSelect (..), KTernary (..))
import Categorifier.C.KTypes.Equality (KEq (..))
import Categorifier.C.KTypes.FromIntegral (KFromIntegral (..))
import Categorifier.C.KTypes.Function (KForeignFunctionCall (..), KFunCall (..))
import Categorifier.C.KTypes.IEEE (KConvertFloat (..), KIsInfinite (..), KIsNaN (..), fpZeroComparison)
import Categorifier.C.KTypes.KBits (KBits (..))
import Categorifier.C.KTypes.KDivisible (KDivisible (..))
import Categorifier.C.KTypes.KEnum (KEnum (..), toKEnum)
import Categorifier.C.KTypes.KLiteral (KLiteral (..))
import qualified Categorifier.C.KTypes.Libm as Libm
import Categorifier.C.KTypes.Round (KRound (..), safeRound)
import qualified Categorifier.C.KTypes.SwitchCase as SwitchCase
import Categorifier.C.KTypes.TotalOrder (KOrd (..))
import Categorifier.C.Prim (Arrays, IsPrimitive, PrimFractional, PrimIntegral, PrimNum)
import Categorifier.Category (NativeCat (..), UnsafeCoerceCat (..))
import Categorifier.Client (deriveHasRep)
import Categorifier.Common.IO.Exception (Exception, impureThrow)
import Categorifier.ConCatExtensions (IntegralCat' (..), RealToFracCat (..))
import qualified Codec.Serialise as CBOR
import ConCat.Category
  ( EqCat (..),
    FromIntegralCat (..),
    IfCat (..),
    IntegralCat (..),
    MinMaxCat (..),
    OrdCat (..),
  )
import qualified ConCat.Category
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData, NFData1 (..))
import Control.Lens (Lens')
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (bimap, first)
import qualified Data.Bits as Bits
import Data.Coerce (Coercible, coerce)
import Data.Functor.Classes (Eq1 (..), Show1 (..))
import Data.Functor.Compose (Compose (..))
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1 (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (fromMaybe)
import Data.Order (Preorder ((<~)))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.Vector.Generic as GV
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable)
import qualified GHC.Float as GHC (double2Float, float2Double)
import Test.QuickCheck (Arbitrary (..))
import Text.Printf (PrintfArg)

-- |
--
--  __NB__: This type is restricted. The constructor isn't exported (and hopefully the accessor
--          will eventually not be exported). If you feel the need to use `UnsafeC`, you can instead
--          use `kliteral` (or, instead of pattern matching, use `unsafeC`).
--
-- TODO: Don't actually export the constructor. It is currently needed by
-- @Categorifier.C.CTypes.CGeneric.CType.constructorToConPrim@ and
-- @Categorifier.C.CTypes.CGeneric.CType.datatypeToCTypePrim@
-- due to the `Coercible` constraints.
newtype C a = UnsafeC {unsafeC :: a}
  deriving stock
    ( Functor,
      Foldable,
      Traversable
    )
  deriving newtype
    ( Show,
      Num,
      Fractional,
      Eq,
      Storable,
      Arbitrary,
      CBOR.Serialise,
      Aeson.ToJSON,
      Aeson.FromJSON,
      PrintfArg,
      Enum,
      Bounded,
      NFData,
      Hashable,
      Aeson.FromJSONKey,
      Aeson.ToJSONKey,
      Bits.Bits,
      Bits.FiniteBits
    )

instance {-# OVERLAPPABLE #-} Ord a => Ord (C a) where
  UnsafeC x < UnsafeC y = x < y
  UnsafeC x <= UnsafeC y = x <= y
  UnsafeC x > UnsafeC y = x > y
  UnsafeC x >= UnsafeC y = x >= y

instance Ord (C Float) where
  UnsafeC x < UnsafeC y = x < y
  UnsafeC x <= UnsafeC y = x <= y
  UnsafeC x > UnsafeC y = x > y
  UnsafeC x >= UnsafeC y = x >= y
  min = liftA2 (fpZeroComparison min)
  max = liftA2 (fpZeroComparison max)

instance Ord (C Double) where
  UnsafeC x < UnsafeC y = x < y
  UnsafeC x <= UnsafeC y = x <= y
  UnsafeC x > UnsafeC y = x > y
  UnsafeC x >= UnsafeC y = x >= y
  min = liftA2 (fpZeroComparison min)
  max = liftA2 (fpZeroComparison max)

deriving newtype instance Real a => Real (C a)

deriving newtype instance RealFrac a => RealFrac (C a)

instance Eq1 C where
  liftEq eq (UnsafeC x) (UnsafeC y) = x `eq` y

instance SupportsKBits C

instance PrimitivesToCxxType C

-- | Utility function for invoking 'toCxxType' using @'ToCxxType' 'C' a@ instance.  Then calls
-- 'Barbies.bmap' to get rid of the 'C'.
toCxxTypeViaC ::
  forall a g.
  ( ToCxxType C a,
    Applicative g,
    Traversable g,
    CoercibleT g
  ) =>
  g a ->
  CxxType g
toCxxTypeViaC = Barbies.bmap getRidOfComposeC . toCxxType
  where
    getRidOfComposeC :: Compose g C b -> g b
    getRidOfComposeC = fmap unsafeC . getCompose

instance Floating (C Double) where
  pi = UnsafeC pi

  sqrt = fmap sqrt

  (**) = liftA2 Libm.libmPow

  logBase base arg = fmap Libm.libmLog arg / fmap Libm.libmLog base

  log = fmap Libm.libmLog

  exp = fmap Libm.libmExp

  sin = fmap Libm.libmSin

  cos = fmap Libm.libmCos

  tan = fmap Libm.libmTan

  asin = fmap Libm.libmAsin

  acos = fmap Libm.libmAcos

  atan = fmap Libm.libmAtan

  sinh = fmap Libm.libmSinh

  cosh = fmap Libm.libmCosh

  tanh = fmap Libm.libmTanh

  asinh = hyperbolicFail

  acosh = hyperbolicFail

  atanh = hyperbolicFail

instance Floating (C Float) where
  pi = UnsafeC pi

  sqrt = fmap sqrt

  (**) = liftA2 Libm.libmPowf

  logBase base arg = fmap Libm.libmLogf arg / fmap Libm.libmLogf base

  log = fmap Libm.libmLogf

  exp = fmap Libm.libmExpf

  sin = fmap Libm.libmSinf

  cos = fmap Libm.libmCosf

  tan = fmap Libm.libmTanf

  asin = fmap Libm.libmAsinf

  acos = fmap Libm.libmAcosf

  atan = fmap Libm.libmAtanf

  sinh = fmap Libm.libmSinhf

  cosh = fmap Libm.libmCoshf

  tanh = fmap Libm.libmTanhf

  asinh = hyperbolicFail

  acosh = hyperbolicFail

  atanh = hyperbolicFail

hyperbolicFail :: c
hyperbolicFail =
  error . unlines $
    [ "Hyperbolic functions are not supported by C at the moment,",
      "since their implementation in GHC differs so significantly from",
      "that in `libm` and is so badly behaved that they shouldn't be used",
      "in an abstract way (i.e. through the `Floating` class)."
    ]

instance NFData1 C where
  liftRnf arnf (UnsafeC x) = arnf x

instance Hashable1 C where
  liftHashWithSalt hws salt = hws salt . unsafeC

instance Applicative C where
  pure = UnsafeC

  UnsafeC f <*> UnsafeC x = UnsafeC (f x)

  liftA2 f (UnsafeC x) (UnsafeC y) = UnsafeC (f x y)

instance Show1 C where
  liftShowsPrec sp _ prec (UnsafeC a) =
    ("UnsafeC { unsafeC = " <>) . sp prec a . (" }" <>)

instance Preorder a => Preorder (C a) where
  UnsafeC x <~ UnsafeC y = x <~ y

instance Eq (C a) => KEq C (C a) where
  x .== y = UnsafeC $ x == y
  x ./= y = UnsafeC $ x /= y

instance (Eq (C a), Ord (C a)) => KOrd C (C a) where
  x .< y = UnsafeC $ x < y
  x .<= y = UnsafeC $ x <= y
  x .> y = UnsafeC $ x > y
  x .>= y = UnsafeC $ x >= y
  kMin = min
  kMax = max

instance (Bounded a, Integral (C a), PrimIntegral a) => KRound C a where
  kRoundDouble = safeRound round round fromIntegral
  kRoundFloat = safeRound round round fromIntegral

instance KTernary C (C a) where
  kTernary b tr fa = if unsafeC b then tr else fa

instance IsPrimitive a => KLiteral C a where
  kliteral = UnsafeC

instance {-# OVERLAPPABLE #-} PrimIntegral a => KFromIntegral C a where
  -- This uses `kliteral` instead of the `Num (C a)` instance because `FromIntegralCat` doesn't like
  -- converting between wrapped and unwrapped prims.
  kFromIntegral = kliteral . fromIntegral . unsafeC

-- These two instances must use `realToFrac` and not `fromIntegral`, since
-- they have bit-level differences. See thread
-- https://hvsd.slack.com/archives/CSQ9LRUJX/p1611119685179100
instance KFromIntegral C Float where kFromIntegral = realToFrac

instance KFromIntegral C Double where kFromIntegral = realToFrac

kDivSigned :: (Bounded a, Integral a) => C a -> C a -> C a
kDivSigned _ 0 = 0
kDivSigned x y
  -- If we divide the minimum signed integer value by negative one, we will get a floating-point
  -- exception (I know . . .), because it has no positive counterpart in the domain of that type.
  -- In this case, it's not clear how to make a real winning move, but for now we simply mimic what
  -- SBV does and what Haskell does with things like 'abs', and say that this function returns the
  -- dividend (the minimum value, still negative).
  | x == minBound && y == (-1) = x
  | otherwise = liftA2 div x y

kDivUnsigned :: Integral a => C a -> C a -> C a
kDivUnsigned _ 0 = 0
kDivUnsigned x y = liftA2 div x y

kModIntegral :: Integral a => C a -> C a -> C a
kModIntegral x 0 = x
kModIntegral x y = liftA2 mod x y

instance Integral (C Word8) where
  toInteger = toInteger . unsafeC
  quotRem = curry $ bimap kliteral kliteral . uncurry quotRem . bimap unsafeC unsafeC
  div = kDivUnsigned
  mod = kModIntegral

instance Integral (C Word16) where
  toInteger = toInteger . unsafeC
  quotRem = curry $ bimap kliteral kliteral . uncurry quotRem . bimap unsafeC unsafeC
  div = kDivUnsigned
  mod = kModIntegral

instance Integral (C Word32) where
  toInteger = toInteger . unsafeC
  quotRem = curry $ bimap kliteral kliteral . uncurry quotRem . bimap unsafeC unsafeC
  div = kDivUnsigned
  mod = kModIntegral

instance Integral (C Word64) where
  toInteger = toInteger . unsafeC
  quotRem = curry $ bimap kliteral kliteral . uncurry quotRem . bimap unsafeC unsafeC
  div = kDivUnsigned
  mod = kModIntegral

instance Integral (C Int8) where
  toInteger = toInteger . unsafeC
  quotRem = curry $ bimap kliteral kliteral . uncurry quotRem . bimap unsafeC unsafeC
  div = kDivSigned
  mod = kModIntegral

instance Integral (C Int16) where
  toInteger = toInteger . unsafeC
  quotRem = curry $ bimap kliteral kliteral . uncurry quotRem . bimap unsafeC unsafeC
  div = kDivSigned
  mod = kModIntegral

instance Integral (C Int32) where
  toInteger = toInteger . unsafeC
  quotRem = curry $ bimap kliteral kliteral . uncurry quotRem . bimap unsafeC unsafeC
  div = kDivSigned
  mod = kModIntegral

instance Integral (C Int64) where
  toInteger = toInteger . unsafeC
  quotRem = curry $ bimap kliteral kliteral . uncurry quotRem . bimap unsafeC unsafeC
  div = kDivSigned
  mod = kModIntegral

instance Integral (C a) => KDivisible (C a) where
  kDiv = div
  kMod = mod

instance Bits.FiniteBits a => KBits C a where
  testBit (UnsafeC x) = UnsafeC . Bits.testBit x

  setBitTo (UnsafeC x) k (UnsafeC True) = UnsafeC $ Bits.setBit x k
  setBitTo (UnsafeC x) k (UnsafeC False) = UnsafeC $ Bits.clearBit x k

  zeroBits = UnsafeC Bits.zeroBits

instance Eq (KEnum C a) where
  x == y = unsafeC (x .== y)

  x /= y = unsafeC (x ./= y)

instance (GArrays C a, Bounded a, Enum a, Show a) => Show (KEnum C a) where
  showsPrec k x = showsPrec k (fromKEnum x :: a)

instance
  (GArrays C a, CBOR.Serialise a, Bounded a, Enum a, Eq a) =>
  CBOR.Serialise (KEnum C a)
  where
  encode = CBOR.encode . fromKEnum

  decode = fmap toKEnum CBOR.decode

instance (Arbitrary a, Bounded a, Enum a, Eq a) => Arbitrary (KEnum C a) where
  arbitrary = toKEnum <$> arbitrary

instance (Aeson.FromJSON a, Bounded a, Enum a, Eq a) => Aeson.FromJSON (KEnum C a) where
  parseJSON x =
    toKEnum <$> (Aeson.parseJSON x :: Aeson.Parser a)

instance (GArrays C a, Aeson.ToJSON a, Bounded a, Enum a) => Aeson.ToJSON (KEnum C a) where
  toJSON x = Aeson.toJSON (fromKEnum x)

instance (GArrays C a, Lookup a, Bounded a, Enum a, Eq a) => Lookup (KEnum C a) where
  toAccessorTree lens0 = toAccessorTree (lens0 . kenumLens)
    where
      kenumLens :: Lens' (KEnum C a) a
      kenumLens f y = fmap toKEnum (f (fromKEnum y))

-- | Extract enum from 'KEnum'.
fromKEnum :: forall a. (Bounded a, GArrays C a, Enum a) => KEnum C a -> a
fromKEnum (KEnum k) =
  SwitchCase.unsafeIndex (enumFrom minBound) k

newtype FromKEnumInvalidState = FromKEnumInvalidState Word8
  deriving (Show)

instance Exception FromKEnumInvalidState

deriveHasRep ''FromKEnumInvalidState

-- | A safe version of `Unsafe.!!`, which returns `Nothing` if the index is either out of bounds or
--   @< 0@. This will terminate even with infinite lists, which is a common problem with "safe"
--   implementations of indexing.
--
--  __TODO__: A better version of this would prevent a negative argument, but converting between
--            numeric types is fraught.
(!!?) :: Integral b => [a] -> b -> Maybe a
[] !!? _ = Nothing
(y : ys) !!? k =
  case compare k 0 of
    LT -> Nothing
    EQ -> pure y
    GT -> ys !!? (k - 1)

instance KSelect C where
  selectList xs (UnsafeC index) =
    fromMaybe (impureThrow $ KSelectIndexOutOfBounds index) $ xs !!? index

  unsafeBoolToZeroOrOne (UnsafeC False) = 0
  unsafeBoolToZeroOrOne (UnsafeC True) = 1
  -- Needed for "Categorifier" to not get stuck on the specialization.
  {-# INLINE unsafeBoolToZeroOrOne #-}

instance SwitchCase.KIf C where
  kIfThenElse (UnsafeC b) tru fls = if b then tru else fls

instance KFunCall C where
  kCallFunctionWithSpec _name _inspec _outspec fun = fun

newtype KForeignFunctionCallError = HaskellCalleeNotProvided Text deriving (Show)

instance Exception KForeignFunctionCallError

instance KForeignFunctionCall C where
  type KFFCall C a = a
  kffcall _ name _ = fromMaybe . impureThrow $ HaskellCalleeNotProvided name

instance KAnd C where
  UnsafeC x .&& UnsafeC y = UnsafeC (x && y)
  {-# INLINE (.&&) #-}

  UnsafeC x .|| UnsafeC y = UnsafeC (x || y)
  {-# INLINE (.||) #-}

  kNot = fmap not
  {-# INLINE kNot #-}

instance RealFloat a => KIsInfinite C a where
  kIsInfinite (UnsafeC x) = UnsafeC (isInfinite x)

instance RealFloat a => KIsNaN C a where
  kIsNaN (UnsafeC x) = UnsafeC (isNaN x)

instance KConvertFloat C where
  kFloatToDouble (UnsafeC x) = UnsafeC (GHC.float2Double x)

  kDoubleToFloat (UnsafeC x) = UnsafeC (GHC.double2Float x)

instance Lookup a => Lookup (C a) where
  toAccessorTree lens0 = toAccessorTree (lens0 . nativeLens)
    where
      nativeLens :: Lens' (C a) a
      nativeLens f y = fmap UnsafeC (f (unsafeC y))

instance Arbitrary (Barbies.Unit C) where
  arbitrary = pure Barbies.Unit

instance Lookup (Barbies.Unit C)

instance CBOR.Serialise (Barbies.Unit C) where
  encode Barbies.Unit = CBOR.encode ()

  decode = do
    () <- CBOR.decode
    pure Barbies.Unit

instance ToTargetOb a => ToTargetOb (C a) where
  toTargetOb (UnsafeC a) = TargetObW . unTargetObW $ toTargetOb a

type instance TargetOb (C a) = TargetOb a

type instance TargetObTC1 C = CExpr

data IndexOutOfBounds a
  = KSelectIndexOutOfBounds a
  | KSwitchIndexOutOfBounds a
  deriving (Show)

instance (Show a, Typeable a) => Exception (IndexOutOfBounds a)

deriveHasRep ''IndexOutOfBounds

type instance TargetOb (IndexOutOfBounds a) = IndexOutOfBounds (TargetOb a)

instance EqCat Cat a => EqCat Cat (C a) where
  equal = coerceK @(a, a) equal
  notEqual = coerceK @(a, a) notEqual

instance OrdCat Cat a => OrdCat Cat (C a) where
  lessThan = coerceK @(a, a) lessThan
  greaterThan = coerceK @(a, a) greaterThan
  lessThanOrEqual = coerceK @(a, a) lessThanOrEqual
  greaterThanOrEqual = coerceK @(a, a) greaterThanOrEqual

instance {-# OVERLAPPABLE #-} RealToFracCat Cat a b => RealToFracCat Cat a (C b) where
  realToFracK = coerceC ConCat.Category.. realToFracK @Cat @a @b

instance
  ( PrimNum a,
    PrimFractional b,
    CExprTypeLens a,
    CExprTypeLens b,
    TargetOb a ~ CExpr a,
    TargetOb b ~ CExpr b
  ) =>
  RealToFracCat Cat (C a) (C b)
  where
  realToFracK = coerceK @a @b castK

instance
  ( PrimIntegral a,
    PrimNum b,
    CExprTypeLens a,
    CExprTypeLens b,
    TargetOb a ~ CExpr a,
    TargetOb b ~ CExpr b
  ) =>
  FromIntegralCat Cat (C a) (C b)
  where
  fromIntegralC = coerceK @a @b castK

instance (PrimNum a, CExprTypeLens a, ToTargetOb a) => FromIntegralCat Cat Int (C a) where
  fromIntegralC =
    Cat $ toTargetOb . UnsafeC . fromIntegralC . unTargetObW

instance IntegralCat Cat a => IntegralCat Cat (C a) where
  divC = coerceK @(a, a) divC
  modC = coerceK @(a, a) modC

instance IntegralCat' Cat a => IntegralCat' Cat (C a) where
  evenK = coerceK @a evenK
  oddK = coerceK @a oddK
  quotK = coerceK @(a, a) quotK
  remK = coerceK @(a, a) remK

instance MinMaxCat Cat a => MinMaxCat Cat (C a) where
  minC = coerceK @(a, a) minC
  maxC = coerceK @(a, a) maxC

instance IfCat Cat a => IfCat Cat (C a) where
  ifC = coerceK @(Bool, (a, a)) ifC

-- This instance lets us conveniently use `Bool` in the controller, hence allows
-- `if`, `==`, `>` etc., rather than having to use `C Bool`.
instance GArrays C Bool where
  arraysCount pf _ = arraysCount pf (Proxy @(C Bool))

  fromArrays ::
    forall v.
    (forall b. IsPrimitive b => GV.Vector v (C b)) =>
    Arrays (Compose v C) ->
    Either FromArraysError (Bool, Arrays (Compose v C))
  fromArrays = fmap (first unsafeC) . fromArrays @C @(C Bool) @v

  toArrays arrays ref a = toArrays arrays ref (UnsafeC a)

instance GArrays C Word8 where
  arraysCount pf _ = arraysCount pf (Proxy @(C Word8))

  fromArrays ::
    forall v.
    (forall b. IsPrimitive b => GV.Vector v (C b)) =>
    Arrays (Compose v C) ->
    Either FromArraysError (Word8, Arrays (Compose v C))
  fromArrays = fmap (first unsafeC) . fromArrays @C @(C Word8) @v

  toArrays arrays ref a = toArrays arrays ref (UnsafeC a)

instance GArrays C Word16 where
  arraysCount pf _ = arraysCount pf (Proxy @(C Word16))

  fromArrays ::
    forall v.
    (forall b. IsPrimitive b => GV.Vector v (C b)) =>
    Arrays (Compose v C) ->
    Either FromArraysError (Word16, Arrays (Compose v C))
  fromArrays = fmap (first unsafeC) . fromArrays @C @(C Word16) @v

  toArrays arrays ref a = toArrays arrays ref (UnsafeC a)

instance GArrays C Word32 where
  arraysCount pf _ = arraysCount pf (Proxy @(C Word32))

  fromArrays ::
    forall v.
    (forall b. IsPrimitive b => GV.Vector v (C b)) =>
    Arrays (Compose v C) ->
    Either FromArraysError (Word32, Arrays (Compose v C))
  fromArrays = fmap (first unsafeC) . fromArrays @C @(C Word32) @v

  toArrays arrays ref a = toArrays arrays ref (UnsafeC a)

instance GArrays C Word64 where
  arraysCount pf _ = arraysCount pf (Proxy @(C Word64))

  fromArrays ::
    forall v.
    (forall b. IsPrimitive b => GV.Vector v (C b)) =>
    Arrays (Compose v C) ->
    Either FromArraysError (Word64, Arrays (Compose v C))
  fromArrays = fmap (first unsafeC) . fromArrays @C @(C Word64) @v

  toArrays arrays ref a = toArrays arrays ref (UnsafeC a)

------------------------------------------------------------------------------

-- * NativeCat instances for controller functions

instance
  (KRound CExpr a, CExpr a ~ TargetOb a) =>
  NativeCat Cat "Categorifier.C.KTypes.Round.kRoundDouble" (C Double) (C a)
  where
  nativeK = cat kRoundDouble

------------------------------------------------------------------------------

-- * Helper functions

-- | Like `unsafeCoerceK`, but with a `Coercible` constraint for improved type safety.
coerceC :: forall a b. Coercible (TargetOb a) (TargetOb b) => Cat a b
coerceC = unsafeCoerceK

coerceK ::
  forall a b c d.
  (Coercible (TargetOb a) (TargetOb c), Coercible (TargetOb b) (TargetOb d)) =>
  Cat a b ->
  Cat c d
coerceK (Cat f) = Cat $ TargetObW . coerce . unTargetObW . f . TargetObW . coerce . unTargetObW

instance CGeneric (C Bool) where
  type Rep (C Bool) = CG.PrimRep "CBool" "Categorifier.C.KTypes.C" "ktypes-native" (C Bool)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (C Int8) where
  type Rep (C Int8) = CG.PrimRep "CInt8" "Categorifier.C.KTypes.C" "ktypes-native" (C Int8)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (C Int16) where
  type Rep (C Int16) = CG.PrimRep "CInt16" "Categorifier.C.KTypes.C" "ktypes-native" (C Int16)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (C Int32) where
  type Rep (C Int32) = CG.PrimRep "CInt32" "Categorifier.C.KTypes.C" "ktypes-native" (C Int32)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (C Int64) where
  type
    Rep (C Int64) =
      CG.PrimRep "CInt64" "Categorifier.C.KTypes.C" "ktypes-native" (C Int64)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (C Word8) where
  type
    Rep (C Word8) =
      CG.PrimRep "CWord8" "Categorifier.C.KTypes.C" "ktypes-native" (C Word8)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (C Word16) where
  type
    Rep (C Word16) =
      CG.PrimRep "CWord16" "Categorifier.C.KTypes.C" "ktypes-native" (C Word16)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (C Word32) where
  type
    Rep (C Word32) =
      CG.PrimRep "CWord32" "Categorifier.C.KTypes.C" "ktypes-native" (C Word32)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (C Word64) where
  type
    Rep (C Word64) =
      CG.PrimRep "CWord64" "Categorifier.C.KTypes.C" "ktypes-native" (C Word64)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (C Float) where
  type
    Rep (C Float) =
      CG.PrimRep "CFloat" "Categorifier.C.KTypes.C" "ktypes-native" (C Float)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (C Double) where
  type
    Rep (C Double) =
      CG.PrimRep "CDouble" "Categorifier.C.KTypes.C" "ktypes-native" (C Double)
  from = CG.primFrom
  to = CG.primTo

-- | Unlike other `C`-wrapped primitives, `Bool` is only wrapped for compatibility with the old
--   codegen system. So we can freely unwrap it without it being considered like the other
--   primitives would be.
--
--  __TODO__: Once everything is categorified, we should delete this (because there should be no
--            @`C` `Bool`@ remaining in the code).
unCBool :: C Bool -> Bool
unCBool = unsafeC
