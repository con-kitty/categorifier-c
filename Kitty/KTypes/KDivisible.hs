module Kitty.KTypes.KDivisible
  ( KDivisible (..),
  )
where

-- | Symbolic class implementing (integral) division.
-- This class parallels the @Data.SBV(SDivisible)@ but only
-- implements methods which we need so far.
class KDivisible a where
  -- Unused methods which appear in Data.SBV(SDivisible):
  --   kQuotRem :: a -> a -> (a, a)
  --   kDivMod :: a -> a -> (a, a)
  --   kQuot :: a -> a -> a
  --   kRem :: a -> a -> a
  kDiv :: a -> a -> a

  kMod :: a -> a -> a
