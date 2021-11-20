-- |
-- Module      :  Kitty.KTypes.Equality
-- Copyright   :  (c) Kittyhawk 2019
-- License     :  AllRightsReversed
-- Maintainer  :  Matthew Peddie <matt.peddie@kittyhawk.aero>
-- Stability   :  provisional
-- Portability :  GHC
--
-- Equality of primitive values
module Kitty.KTypes.Equality
  ( KEq (..),
  )
where

import Kitty.KTypes.BooleanLogic (KAnd)

infix 4 .==, ./=

class KAnd f => KEq f a | a -> f where
  -- not 'f a -> f a -> f Bool' so that we can have 'Kitty.KTypes.KEnum.KEnum'
  (.==) :: a -> a -> f Bool

  (./=) :: a -> a -> f Bool
