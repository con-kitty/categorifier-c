-- |
-- Module      :  Kitty.KTypes.Conditional
-- Copyright   :  (c) Kittyhawk 2019
-- License     :  AllRightsReversed
-- Maintainer  :  Matthew Peddie <matt.peddie@kittyhawk.aero>
-- Stability   :  provisional
-- Portability :  GHC
--
-- Branching and control flow for code-generatable expressions
module Kitty.KTypes.Conditional
  ( KTernary (..),
    KSelect (..),
  )
where

import Data.Word (Word8)
import Kitty.KTypes.BooleanLogic (KAnd)
import Kitty.Prim (IsPrimitive)

class KAnd f => KTernary f a | a -> f where
  kTernary :: f Bool -> a -> a -> a

-- | Legacy class used to implement 'Kitty.KTypes.SwitchCase.switch' and
-- 'Kitty.KTypes.SwitchCase.kIfThenElse'.
--
-- TODO(greg/peddie): clean this up.
-- TODO(ziyang): can we move KSelect to SwitchCase.hs and merge with KIf?
class KSelect f where
  selectList :: IsPrimitive a => [[f a]] -> f Word8 -> [f a]

  -- | used to implement 'Kitty.KTypes.kIfThenElse', it better just be 0 or 1
  unsafeBoolToZeroOrOne :: f Bool -> f Word8
