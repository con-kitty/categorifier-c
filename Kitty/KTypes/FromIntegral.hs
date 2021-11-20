{-# LANGUAGE GADTs #-}

-- |
-- Module      :  Kitty.KTypes.FromIntegral
-- Copyright   :  (c) Kittyhawk 2019
-- License     :  AllRightsReversed
-- Maintainer  :  Matthew Peddie <matt.peddie@kittyhawk.aero>
-- Stability   :  provisional
-- Portability :  GHC
--
-- Conversion from integral primitive values
module Kitty.KTypes.FromIntegral
  ( KFromIntegral (..),
  )
where

import Kitty.Prim (PrimIntegral, PrimNum)

-- |
-- > "Floating-point rounding errors are as insidious as wood-rot because they are invisible in a
-- > program's text, weaken intended relationships among declared variables and literal constants,
-- > thus undermining the output's correctness."
--
-- -William Kahan
class PrimNum b => KFromIntegral f b where
  kFromIntegral :: PrimIntegral a => f a -> f b
