{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Kitty.HK1.Identity
-- Copyright   :  (c) Kittyhawk 2028
-- License     :  AllRightsReversed
-- Maintainer  :  Matthew Peddie <matt.peddie@kittyhawk.aero>
-- Stability   :  provisional
-- Portability :  GHC
--
-- This is a higher-kinded version of 'Data.Functor.Identity.Identity'.
module Kitty.HK1.Identity
  ( -- * Higher-kinded identity
    Identity1 (..),

    -- ** Newtype isomorphism
    _Identity1, -- from 'makePrisms'
  )
where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Control.Lens.TH (makePrisms, makeWrapped)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Kind (Type)
import GHC.Generics (Generic)

newtype Identity1 a (f :: Type -> Type) = Identity1 {getIdentity1 :: f a}
  deriving stock (Generic, Data)
  deriving newtype (Show, Eq, Ord, NFData, Hashable, Serialise)

makePrisms ''Identity1

makeWrapped ''Identity1
