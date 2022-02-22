{-# LANGUAGE GADTs #-}

-- | Conversion from integral primitive values
module Categorifier.C.KTypes.FromIntegral
  ( KFromIntegral (..),
  )
where

import Categorifier.C.Prim (PrimIntegral, PrimNum)

-- |
-- > "Floating-point rounding errors are as insidious as wood-rot because they are invisible in a
-- > program's text, weaken intended relationships among declared variables and literal constants,
-- > thus undermining the output's correctness."
--
-- -William Kahan
class PrimNum b => KFromIntegral f b where
  kFromIntegral :: PrimIntegral a => f a -> f b
