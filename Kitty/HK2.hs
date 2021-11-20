-- |
-- Module      :  Kitty.HK2
-- Copyright   :  (c) Kittyhawk 2018
-- License     :  AllRightsReversed
-- Maintainer  :  Matthew Peddie <matt.peddie@kittyhawk.aero>
-- Stability   :  provisional
-- Portability :  GHC
--
-- = Overview
--
-- A structure of kind @HK2@ takes two arguments: one of kind @(Type -> Type) -> Type@ (a
-- higher-kinded "Barbies" functor) and another of kind @Type -> Type@ (a normal functor).
module Kitty.HK2
  ( module X,
  )
where

import Kitty.HK2.Const as X
import Kitty.HK2.Unit as X
