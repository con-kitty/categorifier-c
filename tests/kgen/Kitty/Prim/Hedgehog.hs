-- | Good hedgehog generators for primitive types.
module Kitty.Prim.Hedgehog
  ( genBool,
    genIntegral,
    genFloating,
    floatingEq,

    -- * Collections of primitive generators
    primitiveGens,
    getPrimitiveGen,
  )
where

import Control.Lens (view)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Hedgehog as H (MonadGen, MonadTest, diff)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Kitty.Prim (Arrays (..), IsPrimitive (..), PrimLens (..))

-- | Generate any 'Integral' value
genIntegral :: forall a m. (H.MonadGen m, Integral a, Bounded a) => m a
genIntegral =
  Gen.choice
    [ Gen.element [0, 1, -1, minBound, maxBound],
      Gen.integral $ Range.exponentialFrom 0 minBound maxBound
    ]

-- | Generate any 'RealFloat' value.
--
-- This generator chooses a value from
--
--     * small ranges around starting points that often cause problems:
--
--         * 0
--         * 1
--         * @pi / 2@
--         * @pi@
--
--     * a general range spanning all numerical values
--
--     * special IEEE values like infinities and @NaN@.
genFloating :: forall a m. (H.MonadGen m, RealFloat a) => m a
genFloating =
  Gen.choice
    . fmap Gen.realFloat
    $
    -- Search around 0, and don't forget -0.0, as well as 1, pi/2 and
    -- pi, as these are often sensitive values for algorithms and
    -- functions.
    aroundPosNeg 0 1e-6
      <> aroundPosNeg 1 1e-6
      <> aroundPosNeg (pi / 2) 1e-6
      <> aroundPosNeg pi 1e-6
      <>
      -- This limit exceeds the max value of doubles, +-1e308
      [ Range.exponentialFloatFrom 0 (-1e322) 1e322,
        Range.singleton $ 1 / 0, -- Infinity
        Range.singleton $ (-1) / 0, -- -Infinity
        Range.singleton $ 0 / 0 -- NaN
      ]
  where
    aroundFloat :: a -> a -> Range.Range a
    aroundFloat float size = Range.exponentialFloatFrom float (float - size) (float + size)
    aroundPosNeg :: a -> a -> [Range.Range a]
    aroundPosNeg float size = [aroundFloat float size, aroundFloat (negate float) size]

-- | A variant on `Hedgehog.===` that identifies NaNs as equals. It still works for non-FP types.
floatingEq :: (H.MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()
floatingEq x y = withFrozenCallStack $ H.diff x eq y
  where
    eq x' y' = x' /= x' && y' /= y' || x' == y'

-- | Generate a 'Bool'ean value
genBool :: H.MonadGen m => m Bool
genBool = Gen.bool

-- | A generator for each primitive type.
primitiveGens :: H.MonadGen m => Arrays m
primitiveGens =
  Arrays
    { arrayBool = genBool,
      arrayInt8 = genIntegral,
      arrayInt16 = genIntegral,
      arrayInt32 = genIntegral,
      arrayInt64 = genIntegral,
      arrayWord8 = genIntegral,
      arrayWord16 = genIntegral,
      arrayWord32 = genIntegral,
      arrayWord64 = genIntegral,
      arrayFloat = genFloating,
      arrayDouble = genFloating
    }

-- | Summon the appropriate generator
getPrimitiveGen :: forall a m. (IsPrimitive a, H.MonadGen m) => m a
getPrimitiveGen = view (prims_ @a) primitiveGens
