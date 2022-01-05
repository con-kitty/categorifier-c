{-# LANGUAGE UndecidableInstances #-}

-- | Threading input symbols through a tagless-final interpreter.
module Kitty.KGenGenerate.Test.InputReader
  ( InputReader (..),
  )
where

import Control.Applicative (liftA2)
import Control.Monad.Trans.Reader (Reader)
import Data.Functor.Compose (Compose)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Stack (HasCallStack, callStack)
import Kitty.KGenGenerate.Test.Error (KSelectIndexError (..), throwKSelectIndexError)
import Kitty.KTypes.ArcTan2 (ArcTan2 (..))
import Kitty.KTypes.BooleanLogic (KAnd (..))
import Kitty.KTypes.Conditional (KSelect (..), KTernary (..))
import Kitty.KTypes.Equality (KEq (..))
import Kitty.KTypes.FMod (FMod (..))
import Kitty.KTypes.FromIntegral (KFromIntegral (..))
import Kitty.KTypes.IEEE (KConvertFloat (..), KIsInfinite (..), KIsNaN (..))
import Kitty.KTypes.KBits (KBits (..))
import Kitty.KTypes.KDivisible (KDivisible (..))
import Kitty.KTypes.KLiteral (KLiteral (..))
import Kitty.KTypes.Round (KRound (..))
import Kitty.KTypes.TotalOrder (KOrd (..))
import Kitty.Prim (Arrays (..), IsPrimitive)

{- Threading inputs through -}

-- | This thing wraps up a value of type @a@ in some arbitrary @f@ with the knowledge that we'll
-- eventually get an 'Arrays (Compose f)' full of inputs. We use it to generate random expressions
-- without knowing what the inputs will be yet.
newtype InputReader f a = InputReader {getInputReader :: Reader (Arrays (Compose Vector f)) (f a)}

instance KLiteral f a => KLiteral (InputReader f) a where
  kliteral = InputReader . pure . kliteral

liftIR2 :: (f a -> f c -> f b) -> InputReader f a -> InputReader f c -> InputReader f b
liftIR2 f (InputReader a) (InputReader b) = InputReader $ liftA2 f a b

liftIR :: (f a -> f b) -> InputReader f a -> InputReader f b
liftIR f (InputReader a) = InputReader $ f <$> a

instance Num (f a) => Num (InputReader f a) where
  (+) = liftIR2 (+)
  (-) = liftIR2 (-)
  (*) = liftIR2 (*)
  negate = liftIR negate
  abs = liftIR abs
  signum = liftIR signum
  fromInteger i = InputReader $ pure (fromInteger i)

instance Fractional (f a) => Fractional (InputReader f a) where
  (/) = liftIR2 (/)
  fromRational r = InputReader $ pure (fromRational r)

instance Floating (f a) => Floating (InputReader f a) where
  pi = InputReader $ pure pi
  logBase = liftIR2 logBase
  (**) = liftIR2 (**)
  exp = liftIR exp
  log = liftIR log
  sin = liftIR sin
  cos = liftIR cos
  tan = liftIR tan
  asin = liftIR asin
  acos = liftIR acos
  atan = liftIR atan
  sinh = liftIR sinh
  cosh = liftIR cosh
  tanh = liftIR tanh
  asinh = liftIR asinh
  acosh = liftIR acosh
  atanh = liftIR atanh

instance ArcTan2 (f a) => ArcTan2 (InputReader f a) where
  arctan2 = liftIR2 arctan2

instance FMod (f a) => FMod (InputReader f a) where
  fmod = liftIR2 fmod

instance KDivisible (f a) => KDivisible (InputReader f a) where
  kMod = liftIR2 kMod
  kDiv = liftIR2 kDiv

instance (KAnd f) => KAnd (InputReader f) where
  (.&&) = liftIR2 (.&&)
  (.||) = liftIR2 (.||)
  kNot = liftIR kNot

instance
  ( KTernary (InputReader f) (InputReader f a),
    KRound f a
  ) =>
  KRound (InputReader f) a
  where
  kRoundDouble = liftIR kRoundDouble
  kRoundFloat = liftIR kRoundFloat

instance (KBits f a) => KBits (InputReader f) a where
  testBit pr idx = liftIR (`testBit` idx) pr
  setBitTo pr idx val = liftIR2 (`setBitTo` idx) pr val
  zeroBits = InputReader (pure zeroBits)

instance (KFromIntegral f b) => KFromIntegral (InputReader f) b where
  kFromIntegral = liftIR kFromIntegral

instance (KConvertFloat f) => KConvertFloat (InputReader f) where
  kFloatToDouble = liftIR kFloatToDouble
  kDoubleToFloat = liftIR kDoubleToFloat

instance KIsInfinite f a => KIsInfinite (InputReader f) a where
  kIsInfinite = liftIR kIsInfinite

instance KIsNaN f a => KIsNaN (InputReader f) a where
  kIsNaN = liftIR kIsNaN

instance KEq f (f a) => KEq (InputReader f) (InputReader f a) where
  (.==) = liftIR2 (.==)
  (./=) = liftIR2 (./=)

instance KOrd f (f a) => KOrd (InputReader f) (InputReader f a) where
  (.<) = liftIR2 (.<)
  (.>) = liftIR2 (.>)
  (.<=) = liftIR2 (.<=)
  (.>=) = liftIR2 (.>=)
  kMin = liftIR2 kMin
  kMax = liftIR2 kMax

instance KTernary f (f a) => KTernary (InputReader f) (InputReader f a) where
  kTernary (InputReader predicate) (InputReader true) (InputReader false) =
    InputReader $ kTernary <$> predicate <*> true <*> false

instance KSelect f => KSelect (InputReader f) where
  selectList lst idx = go lst
    where
      go ::
        (IsPrimitive a, HasCallStack) =>
        [Vector (InputReader f a)] ->
        Vector (InputReader f a)
      go [] = throwKSelectIndexError callStack KSelectMissingCase
      go alternatives =
        pure . InputReader . fmap selectSingleton $
          selectList
            <$> traverse (traverse getInputReader) (fmap checkScalar alternatives)
            <*> getInputReader idx
      checkScalar xs =
        let len = length xs
         in if len == 1
             then xs
             else throwKSelectIndexError callStack $ KSelectInvalidArgument len
      selectSingleton xs =
        let len = length xs
         in if len == 1
             then V.head xs
             else throwKSelectIndexError callStack $ KSelectInvalidReturn len

  unsafeBoolToZeroOrOne = InputReader . fmap unsafeBoolToZeroOrOne . getInputReader
