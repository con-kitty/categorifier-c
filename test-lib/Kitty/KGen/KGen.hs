{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Kitty.KGen.KGen
  ( KGen (..),

    -- * Helpers
    literal,
    unliteral,
    compareZerosGuard,

    -- * Re-exports for convenience
    SBV.SymVal,
    SBV.IEEEFloatConvertible,
  )
where

import Control.Lens (view)
import Data.Functor.Classes (Show1 (..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.SBV (SBV, (.&&))
import qualified Data.SBV as SBV
import qualified Data.Text as Text (unpack)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Kitty.CTypes.CGeneric (CGeneric)
import qualified Kitty.CTypes.CGeneric as CG
import Kitty.CTypes.Instances (PrimitivesToCxxType)
import Kitty.CTypes.Types (SupportsKBits)
import qualified Kitty.KGen.TH as KGen
import Kitty.KTypes.ArcTan2 (ArcTan2 (..))
import qualified Kitty.KTypes.BooleanLogic as KT (KAnd (..))
import qualified Kitty.KTypes.Conditional as KT (KSelect (..), KTernary (..))
import Kitty.KTypes.Equality (KEq (..))
import Kitty.KTypes.FMod (FMod (..))
import qualified Kitty.KTypes.FromIntegral as KT (KFromIntegral (..))
import qualified Kitty.KTypes.Function as KT
import qualified Kitty.KTypes.IEEE as KT (KConvertFloat (..), KIsInfinite (..), KIsNaN (..))
import qualified Kitty.KTypes.KBits as KT
import Kitty.KTypes.KDivisible (KDivisible (..))
import Kitty.KTypes.KLiteral (KLiteral (..))
import qualified Kitty.KTypes.KType1 as KT (KType1)
import qualified Kitty.KTypes.Libm as Libm
import Kitty.KTypes.Round (KRound (..))
import qualified Kitty.KTypes.Round as Round
import Kitty.KTypes.SwitchCase (KIf)
import Kitty.KTypes.TotalOrder (KOrd (..))
import Kitty.Prim
  ( IsPrimitive,
    PrimGADT (..),
    PrimIntegral,
    Tagged (unTagged),
    arrayCMinimumDefines,
    primCName,
    primGADT,
    prims_,
    _ArrayName,
  )
import PyF (fmt)

-- TODO(matte): This comment is for 'import Kitty.KGen.Interpolate ()'. Please support comments!
-- orphan instance Interpolant (SBV Double)

-- | A value intended for code-generation.
newtype KGen a = KGen {getKGen :: SBV a}
  deriving newtype
    ( Eq,
      Show,
      SBV.EqSymbolic,
      SBV.OrdSymbolic,
      SBV.Uninterpreted
    )
  deriving stock (Generic)

instance Show1 KGen where
  liftShowsPrec _ _ prec (KGen val) =
    ("KGen {getKGen = " <>) . showsPrec prec val . ("}" <>)

literal :: SBV.SymVal a => a -> KGen a
literal = KGen . SBV.literal

unliteral :: SBV.SymVal a => KGen a -> Maybe a
unliteral = SBV.unliteral . getKGen

deriving newtype instance
  (Show a, Bounded a, Integral a, Num a, SBV.SymVal a) =>
  Enum (KGen a)

deriving newtype instance (Ord a, Num a, SBV.SymVal a) => Num (KGen a)

deriving newtype instance (Ord a, SBV.SymVal a, Fractional a) => Fractional (KGen a)

instance SBV.SymVal a => SBV.Mergeable (KGen a)

-- See 'fpMaxH' in 'Data.SBV.Utils.Numeric' for an explanation of this
-- check. The short story is this: it turns out that none of Haskell,
-- SMTLib and common processor hardware agree on what should happen in
-- the case of zeros with differing signs, so SBV represents the
-- result with a symbolic value rather than a concrete one. This
-- causes problems for us both in behavior and in testing of concrete
-- values (against generated C code), so we explicitly codify the
-- behavior here to match the SBV author's preferred behavior.
fpZeroComparison ::
  ( SBV.IEEEFloating a,
    Fractional b,
    SBV.SymVal b,
    Ord b
  ) =>
  (SBV a -> SBV a -> SBV b) ->
  SBV a ->
  SBV a ->
  SBV b
fpZeroComparison f = resolveComparison
  where
    pz v = v `SBV.fpIsEqualObject` 0.0
    nz v = v `SBV.fpIsEqualObject` (-0.0)
    resolveComparison a b =
      SBV.ite (nz a .&& pz b) 0.0
        . SBV.ite (pz a .&& nz b) (-0.0)
        $ f a b

compareZerosGuard ::
  ( SBV.IEEEFloating a,
    Fractional b,
    SBV.SymVal b,
    Ord b
  ) =>
  (SBV a -> SBV a -> SBV b) ->
  KGen a ->
  KGen a ->
  KGen b
compareZerosGuard f (KGen x) (KGen y) = KGen $ fpZeroComparison f x y

unOpLiteral :: SBV.SymVal a => (a -> a) -> SBV a -> SBV a
unOpLiteral f x' = case res of
  Just v -> SBV.literal v
  Nothing ->
    error "Internal error: SBV literal conversion in KGen unary operation!"
  where
    res = f <$> SBV.unliteral x'

binOpLiteral :: SBV.SymVal a => (a -> a -> a) -> SBV a -> SBV a -> SBV a
binOpLiteral f x' y' = case res of
  Just v -> SBV.literal v
  Nothing ->
    error "Internal error: SBV literal conversion in KGen binary operation!"
  where
    res = f <$> SBV.unliteral x' <*> SBV.unliteral y'

liftKGen :: (SBV a -> SBV b) -> KGen a -> KGen b
liftKGen f = KGen . f . getKGen

liftKGen2 ::
  (SBV a -> SBV a1 -> SBV a2) -> KGen a -> KGen a1 -> KGen a2
liftKGen2 f (KGen a) (KGen b) = KGen $ f a b

sbvAtan2 :: SBV Double -> SBV Double -> SBV Double
sbvAtan2 = SBV.cgUninterpret "atan2" mempty (binOpLiteral Libm.libmAtan2)

sbvFMod :: SBV Double -> SBV Double -> SBV Double
sbvFMod = SBV.cgUninterpret "fmod" mempty (binOpLiteral Libm.libmFMod)

sbvExp :: SBV Double -> SBV Double
sbvExp = SBV.cgUninterpret "exp" mempty (unOpLiteral Libm.libmExp)

sbvPow :: SBV Double -> SBV Double -> SBV Double
sbvPow = SBV.cgUninterpret "pow" mempty (binOpLiteral Libm.libmPow)

sbvLogBase :: SBV Double -> SBV Double -> SBV Double
sbvLogBase base arg = sbvLog arg / sbvLog base

sbvLog :: SBV Double -> SBV Double
sbvLog = SBV.cgUninterpret "log" mempty (unOpLiteral Libm.libmLog)

sbvSin :: SBV Double -> SBV Double
sbvSin = SBV.cgUninterpret "sin" mempty (unOpLiteral Libm.libmSin)

sbvCos :: SBV Double -> SBV Double
sbvCos = SBV.cgUninterpret "cos" mempty (unOpLiteral Libm.libmCos)

sbvTan :: SBV Double -> SBV Double
sbvTan = SBV.cgUninterpret "tan" mempty (unOpLiteral Libm.libmTan)

sbvAsin :: SBV Double -> SBV Double
sbvAsin = SBV.cgUninterpret "asin" mempty (unOpLiteral Libm.libmAsin)

sbvAcos :: SBV Double -> SBV Double
sbvAcos = SBV.cgUninterpret "acos" mempty (unOpLiteral Libm.libmAcos)

sbvAtan :: SBV Double -> SBV Double
sbvAtan = SBV.cgUninterpret "atan" mempty (unOpLiteral Libm.libmAtan)

sbvSinh :: SBV Double -> SBV Double
sbvSinh = SBV.cgUninterpret "sinh" mempty (unOpLiteral Libm.libmSinh)

sbvCosh :: SBV Double -> SBV Double
sbvCosh = SBV.cgUninterpret "cosh" mempty (unOpLiteral Libm.libmCosh)

sbvTanh :: SBV Double -> SBV Double
sbvTanh = SBV.cgUninterpret "tanh" mempty (unOpLiteral Libm.libmTanh)

instance ArcTan2 (KGen Double) where
  arctan2 = liftKGen2 sbvAtan2

instance FMod (KGen Double) where
  fmod = liftKGen2 sbvFMod

sbvAtan2f :: SBV Float -> SBV Float -> SBV Float
sbvAtan2f = SBV.cgUninterpret "atan2f" mempty (binOpLiteral Libm.libmAtan2f)

sbvFModf :: SBV Float -> SBV Float -> SBV Float
sbvFModf = SBV.cgUninterpret "fmodf" mempty (binOpLiteral Libm.libmFModf)

sbvExpf :: SBV Float -> SBV Float
sbvExpf = SBV.cgUninterpret "expf" mempty (unOpLiteral Libm.libmExpf)

sbvPowf :: SBV Float -> SBV Float -> SBV Float
sbvPowf = SBV.cgUninterpret "powf" mempty (binOpLiteral Libm.libmPowf)

sbvLogBasef :: SBV Float -> SBV Float -> SBV Float
sbvLogBasef base arg = sbvLogf arg / sbvLogf base

sbvLogf :: SBV Float -> SBV Float
sbvLogf = SBV.cgUninterpret "logf" mempty (unOpLiteral Libm.libmLogf)

sbvSinf :: SBV Float -> SBV Float
sbvSinf = SBV.cgUninterpret "sinf" mempty (unOpLiteral Libm.libmSinf)

sbvCosf :: SBV Float -> SBV Float
sbvCosf = SBV.cgUninterpret "cosf" mempty (unOpLiteral Libm.libmCosf)

sbvTanf :: SBV Float -> SBV Float
sbvTanf = SBV.cgUninterpret "tanf" mempty (unOpLiteral Libm.libmTanf)

sbvAsinf :: SBV Float -> SBV Float
sbvAsinf = SBV.cgUninterpret "asinf" mempty (unOpLiteral Libm.libmAsinf)

sbvAcosf :: SBV Float -> SBV Float
sbvAcosf = SBV.cgUninterpret "acosf" mempty (unOpLiteral Libm.libmAcosf)

sbvAtanf :: SBV Float -> SBV Float
sbvAtanf = SBV.cgUninterpret "atanf" mempty (unOpLiteral Libm.libmAtanf)

sbvSinhf :: SBV Float -> SBV Float
sbvSinhf = SBV.cgUninterpret "sinhf" mempty (unOpLiteral Libm.libmSinhf)

sbvCoshf :: SBV Float -> SBV Float
sbvCoshf = SBV.cgUninterpret "coshf" mempty (unOpLiteral Libm.libmCoshf)

sbvTanhf :: SBV Float -> SBV Float
sbvTanhf = SBV.cgUninterpret "tanhf" mempty (unOpLiteral Libm.libmTanhf)

instance ArcTan2 (KGen Float) where
  arctan2 = liftKGen2 sbvAtan2f

instance FMod (KGen Float) where
  fmod = liftKGen2 sbvFModf

hyperbolicFail :: c
hyperbolicFail =
  error . unlines $
    [ "Hyperbolic functions are not supported by KGen at the moment,",
      "since their implementation in GHC differs so significantly from",
      "that in `libm` and is so badly behaved that they shouldn't be used",
      "in an abstract way (i.e. through the `Floating` class)."
    ]

instance Floating (KGen Double) where
  pi = KGen pi

  sqrt = liftKGen sqrt

  (**) = liftKGen2 sbvPow

  logBase = liftKGen2 sbvLogBase

  log = liftKGen sbvLog

  exp = liftKGen sbvExp

  sin = liftKGen sbvSin

  cos = liftKGen sbvCos

  tan = liftKGen sbvTan

  asin = liftKGen sbvAsin

  acos = liftKGen sbvAcos

  atan = liftKGen sbvAtan

  sinh = liftKGen sbvSinh

  cosh = liftKGen sbvCosh

  tanh = liftKGen sbvTanh

  asinh = hyperbolicFail

  acosh = hyperbolicFail

  atanh = hyperbolicFail

instance Floating (KGen Float) where
  pi = KGen pi

  sqrt = liftKGen sqrt

  (**) = liftKGen2 sbvPowf

  logBase = liftKGen2 sbvLogBasef

  log = liftKGen sbvLogf

  exp = liftKGen sbvExpf

  sin = liftKGen sbvSinf

  cos = liftKGen sbvCosf

  tan = liftKGen sbvTanf

  asin = liftKGen sbvAsinf

  acos = liftKGen sbvAcosf

  atan = liftKGen sbvAtanf

  sinh = liftKGen sbvSinhf

  cosh = liftKGen sbvCoshf

  tanh = liftKGen sbvTanhf

  asinh = hyperbolicFail

  acosh = hyperbolicFail

  atanh = hyperbolicFail

instance SBV.SFiniteBits a => KT.KBits KGen a where
  testBit (KGen x) = KGen . SBV.sTestBit x

  setBitTo (KGen x) k (KGen b) = KGen $ SBV.setBitTo x k b

  zeroBits = KGen SBV.zeroBits

instance SBV.IEEEFloating a => KT.KIsInfinite KGen a where
  kIsInfinite (KGen x) = KGen (SBV.fpIsInfinite x)

instance SBV.IEEEFloating a => KT.KIsNaN KGen a where
  kIsNaN (KGen x) = KGen (SBV.fpIsNaN x)

-- What rounding mode do we want?
-- This one is haskell's default according to SBV Haddocks,
-- and elsewhere in SBV it is assumed to be this one.
instance KT.KConvertFloat KGen where
  kFloatToDouble (KGen x) = KGen (SBV.toSDouble SBV.sRoundNearestTiesToEven x)

  kDoubleToFloat (KGen x) = KGen (SBV.toSFloat SBV.sRoundNearestTiesToEven x)

instance SupportsKBits KGen

instance PrimitivesToCxxType KGen

instance KT.KType1 KGen

instance KT.KSelect KGen where
  unsafeBoolToZeroOrOne (KGen bool') = KGen (SBV.ite bool' 1 0)

  selectList :: forall a. IsPrimitive a => [[KGen a]] -> KGen Word8 -> [KGen a]
  -- error for list of length 0
  selectList [] _ = error "selectList got 0 constructors"
  -- lists of length 1 gets special cased so that SBV doesn't use its mergeArrays code
  selectList ([KGen x0] : xs') (KGen index) = [KGen ret]
    where
      xs = fmap getOneKGen xs'
        where
          getOneKGen [KGen x] = x
          getOneKGen r =
            error $
              "selectList expected scalars but got length "
                <> show (length r)
      ret = case primGADT @a of
        GBool -> SBV.select (x0 : xs) x0 index
        GInt8 -> SBV.select (x0 : xs) x0 index
        GInt16 -> SBV.select (x0 : xs) x0 index
        GInt32 -> SBV.select (x0 : xs) x0 index
        GInt64 -> SBV.select (x0 : xs) x0 index
        GWord8 -> SBV.select (x0 : xs) x0 index
        GWord16 -> SBV.select (x0 : xs) x0 index
        GWord32 -> SBV.select (x0 : xs) x0 index
        GWord64 -> SBV.select (x0 : xs) x0 index
        GFloat -> SBV.select (x0 : xs) x0 index
        GDouble -> SBV.select (x0 : xs) x0 index
  -- merge arrays
  selectList (x0s' : xs') (KGen index) = fmap KGen ret
    where
      ret = case primGADT @a of
        GBool -> SBV.select (x0s : xs) x0s index
        GInt8 -> SBV.select (x0s : xs) x0s index
        GInt16 -> SBV.select (x0s : xs) x0s index
        GInt32 -> SBV.select (x0s : xs) x0s index
        GInt64 -> SBV.select (x0s : xs) x0s index
        GWord8 -> SBV.select (x0s : xs) x0s index
        GWord16 -> SBV.select (x0s : xs) x0s index
        GWord32 -> SBV.select (x0s : xs) x0s index
        GWord64 -> SBV.select (x0s : xs) x0s index
        GFloat -> SBV.select (x0s : xs) x0s index
        GDouble -> SBV.select (x0s : xs) x0s index
      x0s = fmap getKGen x0s'
      xs = fmap getKGen <$> xs'

instance KIf KGen

--      TODO(peddie): global flag for bad things happening, use CallStack for more fun
--      KGen (svUninterpret "enum_double_ret" ret flag)
--      (ret, flag) = SBV.select (map ((,SFalse) . getKGen) xs) (default', STrue) index
--      enumRep = rep (proxyOf kenum)
--        where
--          proxyOf :: KEnum f b -> Proxy b
--          proxyOf = const Proxy

instance KT.KFunCall KGen where
  kCallFunctionWithSpec _name _inspec _outspec fun = fun

instance KT.KAnd KGen where
  (.&&) (KGen a) (KGen b) = KGen (a SBV..&& b)

  (.||) (KGen a) (KGen b) = KGen (a SBV..|| b)

  kNot (KGen a) = KGen (SBV.sNot a)

overKGen :: (SBV a -> SBV a -> SBV b) -> KGen a -> KGen a -> KGen b
overKGen f (KGen x) (KGen y) = KGen (f x y)

protectedDivModHelpers :: String
protectedDivModHelpers =
  [fmt|
#ifndef SIGNUM
#define SIGNUM(x) (((x) == 0) ? 0 : (((x) < 0) ? -1 : 1))
#endif  /* SIGNUM */

#ifndef QUOT
#define QUOT(x, y) (((y) == 0) ? 0 : (x) / (y))
#endif  /* QUOT */

#ifndef REM
#define REM(x, y) (((y) == 0) ? 0 : (x) % (y))
#endif  /* REM */
|]

-- The short story here is that C compilers seem to treat modulo and division a bit specially with
-- regards to signed integer overflow; specifically, @-fwrapv@ does not apply.  We have to handle
-- this case ourselves, as we do in the 'C' and 'CExpr' instances.
--
-- For information on this, see:
--
-- https://github.com/LeventErkok/sbv/issues/468
-- https://github.com/LeventErkok/sbv/issues/474
protectedMod ::
  forall a.
  (SBV.SymVal a, SBV.SDivisible (SBV a), IsPrimitive a) =>
  SBV a ->
  SBV a ->
  SBV a
protectedMod = SBV.cgUninterpret ("PROTECTED_MOD_" <> tyName) cCode hCode
  where
    minVal = Text.unpack $ view (prims_ @a . _ArrayName) arrayCMinimumDefines
    tyName = Text.unpack . unTagged $ primCName @a
    hCode = SBV.sMod
    cCode =
      pure
        [fmt|
{protectedDivModHelpers}

#define PROTECTED_MOD_{tyName}(x, y) \\
  /* 0 `mod` y = 0 */ \\
  ((x) == 0) ? 0 : \\
  /* x `mod` 1 = 0 */ \\
  (((y) == 1) ? 0 : \\
  /* minVal `mod` -1 = 0 */ \\
  ((((x) == {minVal}) && ((y) == -1)) ? 0 : \\
  /* x `mod` 0 = x */ \\
  (((y) == 0) ? (x) : \\
  /* `mod` is not the same as `rem` if the sign changes relative to the denominator */ \\
  ((SIGNUM(y) == (-(SIGNUM(REM(x, y))))) ? ((y) + REM(x, y)) : \\
  /* plain mod */ \\
  REM(x, y)))))
|]

protectedDiv ::
  forall a.
  (SBV.SymVal a, SBV.SDivisible (SBV a), IsPrimitive a) =>
  SBV a ->
  SBV a ->
  SBV a
protectedDiv = SBV.cgUninterpret ("PROTECTED_DIV_" <> tyName) cCode hCode
  where
    minVal = Text.unpack $ view (prims_ @a . _ArrayName) arrayCMinimumDefines
    tyName = Text.unpack . unTagged $ primCName @a
    hCode = SBV.sDiv
    cCode =
      pure
        [fmt|
{protectedDivModHelpers}

#define PROTECTED_DIV_{tyName}(x, y) \\
  /* 0 `div` y = 0 */ \\
  ((x) == 0) ? 0 : \\
  /* x `div` 0 = 0 */ \\
  (((y) == 0) ? 0 : \\
  /* minVal `div` -1 = minVal */ \\
  ((((x) == {minVal}) && ((y) == -1)) ? {minVal} : \\
  /* `div` is not the same as `quot` if the sign changes relative to the denominator */ \\
  ((SIGNUM(y) == -(SIGNUM(REM(x, y)))) ? (QUOT(x, y) - 1) : \\
  /* plain div */ \\
  QUOT(x, y))))
|]

instance KEq KGen (KGen a) where
  (.==) = overKGen (SBV..==)
  (./=) = overKGen (SBV../=)

instance {-# OVERLAPPABLE #-} (Ord a, SBV.SymVal a) => KOrd KGen (KGen a) where
  (.<) = overKGen (SBV..<)
  (.<=) = overKGen (SBV..<=)
  (.>) = overKGen (SBV..>)
  (.>=) = overKGen (SBV..>=)
  kMin = overKGen SBV.smin
  kMax = overKGen SBV.smax

KGen.declareOrdIEEE 'overKGen ''KGen ''Float

KGen.declareOrdIEEE 'overKGen ''KGen ''Double

instance SBV.SymVal a => KT.KTernary KGen (KGen a) where
  kTernary b = SBV.ite (getKGen b)

instance IsPrimitive a => KLiteral KGen a where
  kliteral = case primGADT @a of
    GBool -> literal
    GInt8 -> literal
    GInt16 -> literal
    GInt32 -> literal
    GInt64 -> literal
    GWord8 -> literal
    GWord16 -> literal
    GWord32 -> literal
    GWord64 -> literal
    GFloat -> literal
    GDouble -> literal

instance {-# OVERLAPPABLE #-} SBV.SDivisible (SBV a) => KDivisible (KGen a) where
  kDiv = overKGen SBV.sDiv
  kMod = overKGen SBV.sMod

KGen.declareKDivisibleSigned 'overKGen ''KGen ''Int8 'protectedDiv 'protectedMod

KGen.declareKDivisibleSigned 'overKGen ''KGen ''Int16 'protectedDiv 'protectedMod

KGen.declareKDivisibleSigned 'overKGen ''KGen ''Int32 'protectedDiv 'protectedMod

KGen.declareKDivisibleSigned 'overKGen ''KGen ''Int64 'protectedDiv 'protectedMod

instance (Bounded a, PrimIntegral a, SBV.IEEEFloatConvertible a) => KRound KGen a where
  kRoundFloat = Round.safeRound roundFloat roundFloat roundInt64
    where
      roundFloat = KGen . SBV.fromSFloat SBV.sRNE . getKGen
      roundInt64 = KGen . SBV.sFromIntegral . getKGen

  kRoundDouble = Round.safeRound roundDouble roundDouble roundInt64
    where
      roundDouble = KGen . SBV.fromSDouble SBV.sRNE . getKGen
      roundInt64 = KGen . SBV.sFromIntegral . getKGen

instance KT.KFromIntegral KGen Word8 where
  kFromIntegral :: forall a. PrimIntegral a => KGen a -> KGen Word8
  kFromIntegral = case primGADT @a of
    GInt8 -> liftKGen SBV.sFromIntegral
    GInt16 -> liftKGen SBV.sFromIntegral
    GInt32 -> liftKGen SBV.sFromIntegral
    GInt64 -> liftKGen SBV.sFromIntegral
    GWord8 -> liftKGen SBV.sFromIntegral
    GWord16 -> liftKGen SBV.sFromIntegral
    GWord32 -> liftKGen SBV.sFromIntegral
    GWord64 -> liftKGen SBV.sFromIntegral

instance KT.KFromIntegral KGen Word16 where
  kFromIntegral :: forall a. PrimIntegral a => KGen a -> KGen Word16
  kFromIntegral = case primGADT @a of
    GInt8 -> liftKGen SBV.sFromIntegral
    GInt16 -> liftKGen SBV.sFromIntegral
    GInt32 -> liftKGen SBV.sFromIntegral
    GInt64 -> liftKGen SBV.sFromIntegral
    GWord8 -> liftKGen SBV.sFromIntegral
    GWord16 -> liftKGen SBV.sFromIntegral
    GWord32 -> liftKGen SBV.sFromIntegral
    GWord64 -> liftKGen SBV.sFromIntegral

instance KT.KFromIntegral KGen Word32 where
  kFromIntegral :: forall a. PrimIntegral a => KGen a -> KGen Word32
  kFromIntegral = case primGADT @a of
    GInt8 -> liftKGen SBV.sFromIntegral
    GInt16 -> liftKGen SBV.sFromIntegral
    GInt32 -> liftKGen SBV.sFromIntegral
    GInt64 -> liftKGen SBV.sFromIntegral
    GWord8 -> liftKGen SBV.sFromIntegral
    GWord16 -> liftKGen SBV.sFromIntegral
    GWord32 -> liftKGen SBV.sFromIntegral
    GWord64 -> liftKGen SBV.sFromIntegral

instance KT.KFromIntegral KGen Word64 where
  kFromIntegral :: forall a. PrimIntegral a => KGen a -> KGen Word64
  kFromIntegral = case primGADT @a of
    GInt8 -> liftKGen SBV.sFromIntegral
    GInt16 -> liftKGen SBV.sFromIntegral
    GInt32 -> liftKGen SBV.sFromIntegral
    GInt64 -> liftKGen SBV.sFromIntegral
    GWord8 -> liftKGen SBV.sFromIntegral
    GWord16 -> liftKGen SBV.sFromIntegral
    GWord32 -> liftKGen SBV.sFromIntegral
    GWord64 -> liftKGen SBV.sFromIntegral

instance KT.KFromIntegral KGen Int8 where
  kFromIntegral :: forall a. PrimIntegral a => KGen a -> KGen Int8
  kFromIntegral = case primGADT @a of
    GInt8 -> liftKGen SBV.sFromIntegral
    GInt16 -> liftKGen SBV.sFromIntegral
    GInt32 -> liftKGen SBV.sFromIntegral
    GInt64 -> liftKGen SBV.sFromIntegral
    GWord8 -> liftKGen SBV.sFromIntegral
    GWord16 -> liftKGen SBV.sFromIntegral
    GWord32 -> liftKGen SBV.sFromIntegral
    GWord64 -> liftKGen SBV.sFromIntegral

instance KT.KFromIntegral KGen Int16 where
  kFromIntegral :: forall a. PrimIntegral a => KGen a -> KGen Int16
  kFromIntegral = case primGADT @a of
    GInt8 -> liftKGen SBV.sFromIntegral
    GInt16 -> liftKGen SBV.sFromIntegral
    GInt32 -> liftKGen SBV.sFromIntegral
    GInt64 -> liftKGen SBV.sFromIntegral
    GWord8 -> liftKGen SBV.sFromIntegral
    GWord16 -> liftKGen SBV.sFromIntegral
    GWord32 -> liftKGen SBV.sFromIntegral
    GWord64 -> liftKGen SBV.sFromIntegral

instance KT.KFromIntegral KGen Int32 where
  kFromIntegral :: forall a. PrimIntegral a => KGen a -> KGen Int32
  kFromIntegral = case primGADT @a of
    GInt8 -> liftKGen SBV.sFromIntegral
    GInt16 -> liftKGen SBV.sFromIntegral
    GInt32 -> liftKGen SBV.sFromIntegral
    GInt64 -> liftKGen SBV.sFromIntegral
    GWord8 -> liftKGen SBV.sFromIntegral
    GWord16 -> liftKGen SBV.sFromIntegral
    GWord32 -> liftKGen SBV.sFromIntegral
    GWord64 -> liftKGen SBV.sFromIntegral

instance KT.KFromIntegral KGen Int64 where
  kFromIntegral :: forall a. PrimIntegral a => KGen a -> KGen Int64
  kFromIntegral = case primGADT @a of
    GInt8 -> liftKGen SBV.sFromIntegral
    GInt16 -> liftKGen SBV.sFromIntegral
    GInt32 -> liftKGen SBV.sFromIntegral
    GInt64 -> liftKGen SBV.sFromIntegral
    GWord8 -> liftKGen SBV.sFromIntegral
    GWord16 -> liftKGen SBV.sFromIntegral
    GWord32 -> liftKGen SBV.sFromIntegral
    GWord64 -> liftKGen SBV.sFromIntegral

instance KT.KFromIntegral KGen Float where
  kFromIntegral :: forall a. PrimIntegral a => KGen a -> KGen Float
  kFromIntegral = case primGADT @a of
    GInt8 -> liftKGen (SBV.toSFloat SBV.sRNE)
    GInt16 -> liftKGen (SBV.toSFloat SBV.sRNE)
    GInt32 -> liftKGen (SBV.toSFloat SBV.sRNE)
    GInt64 -> liftKGen (SBV.toSFloat SBV.sRNE)
    GWord8 -> liftKGen (SBV.toSFloat SBV.sRNE)
    GWord16 -> liftKGen (SBV.toSFloat SBV.sRNE)
    GWord32 -> liftKGen (SBV.toSFloat SBV.sRNE)
    GWord64 -> liftKGen (SBV.toSFloat SBV.sRNE)

instance KT.KFromIntegral KGen Double where
  kFromIntegral :: forall a. PrimIntegral a => KGen a -> KGen Double
  kFromIntegral = case primGADT @a of
    GInt8 -> liftKGen (SBV.toSDouble SBV.sRNE)
    GInt16 -> liftKGen (SBV.toSDouble SBV.sRNE)
    GInt32 -> liftKGen (SBV.toSDouble SBV.sRNE)
    GInt64 -> liftKGen (SBV.toSDouble SBV.sRNE)
    GWord8 -> liftKGen (SBV.toSDouble SBV.sRNE)
    GWord16 -> liftKGen (SBV.toSDouble SBV.sRNE)
    GWord32 -> liftKGen (SBV.toSDouble SBV.sRNE)
    GWord64 -> liftKGen (SBV.toSDouble SBV.sRNE)

instance CGeneric (KGen Bool) where
  type
    Rep (KGen Bool) =
      CG.PrimRep "KGenBool" "Kitty.KTypes.KGen" "kgen" (KGen Bool)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (KGen Int8) where
  type
    Rep (KGen Int8) =
      CG.PrimRep "KGenInt8" "Kitty.KTypes.KGen" "kgen" (KGen Int8)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (KGen Int16) where
  type
    Rep (KGen Int16) =
      CG.PrimRep "KGenInt16" "Kitty.KTypes.KGen" "kgen" (KGen Int16)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (KGen Int32) where
  type
    Rep (KGen Int32) =
      CG.PrimRep "KGenInt32" "Kitty.KTypes.KGen" "kgen" (KGen Int32)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (KGen Int64) where
  type
    Rep (KGen Int64) =
      CG.PrimRep "KGenInt64" "Kitty.KTypes.KGen" "kgen" (KGen Int64)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (KGen Word8) where
  type
    Rep (KGen Word8) =
      CG.PrimRep "KGenWord8" "Kitty.KTypes.KGen" "kgen" (KGen Word8)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (KGen Word16) where
  type
    Rep (KGen Word16) =
      CG.PrimRep "KGenWord16" "Kitty.KTypes.KGen" "kgen" (KGen Word16)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (KGen Word32) where
  type
    Rep (KGen Word32) =
      CG.PrimRep "KGenWord32" "Kitty.KTypes.KGen" "kgen" (KGen Word32)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (KGen Word64) where
  type
    Rep (KGen Word64) =
      CG.PrimRep "KGenWord64" "Kitty.KTypes.KGen" "kgen" (KGen Word64)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (KGen Float) where
  type
    Rep (KGen Float) =
      CG.PrimRep "KGenFloat" "Kitty.KTypes.KGen" "kgen" (KGen Float)
  from = CG.primFrom
  to = CG.primTo

instance CGeneric (KGen Double) where
  type
    Rep (KGen Double) =
      CG.PrimRep "KGenDouble" "Kitty.KTypes.KGen" "kgen" (KGen Double)
  from = CG.primFrom
  to = CG.primTo
