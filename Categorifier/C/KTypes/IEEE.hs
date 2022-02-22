{-# LANGUAGE GADTs #-}

module Categorifier.C.KTypes.IEEE
  ( fpZeroComparison,
    KIsInfinite (..),
    KIsNaN (..),
    KConvertFloat (..),
  )
where

-- See 'fpMaxH' in 'Data.SBV.Utils.Numeric' for an explanation of this
-- check. The short story is this: it turns out that none of Haskell,
-- SMTLib and common processor hardware agree on what should happen in
-- the case of zeros with differing signs, so SBV represents the
-- result with a symbolic value rather than a concrete one. This
-- causes problems for us both in behavior and in testing of concrete
-- values (against generated C code), so we explicitly codify the
-- behavior here to match the SBV author's preferred behavior.
fpZeroComparison ::
  RealFloat a => (a -> a -> a) -> a -> a -> a
fpZeroComparison f = resolveComparison
  where
    pz v = v == 0.0 && not (isNegativeZero v)
    nz v = isNegativeZero v
    resolveComparison a b
      | nz a && pz b = 0.0
      | pz a && nz b = -0.0
      | isNaN a = b
      | isNaN b = a
      | otherwise = f a b

class KIsInfinite f a where
  kIsInfinite :: f a -> f Bool

class KIsNaN f a where
  kIsNaN :: f a -> f Bool

class KConvertFloat f where
  kFloatToDouble :: f Float -> f Double

  kDoubleToFloat :: f Double -> f Float
