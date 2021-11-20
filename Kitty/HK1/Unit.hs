{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Kitty.HK1.Unit
-- Copyright   :  (c) Kittyhawk 2018
-- License     :  AllRightsReversed
-- Maintainer  :  Matthew Peddie <matt.peddie@kittyhawk.aero>
-- Stability   :  provisional
-- Portability :  GHC
--
-- This is a higher-kinded version of unit @()@.
--
-- The types in this module are equivalent to the modern, poly-kinded 'Data.Proxy.Proxy'.  They're
-- written as custom types to allow custom instances.
module Kitty.HK1.Unit
  ( Unit0 (..),
    HasUnit0 (..),
  )
where

import Accessors (Lookup (..))
import Codec.Serialise (Serialise (..))
import Control.DeepSeq (NFData (..))
import Control.Lens (AsEmpty (..), makeClassy)
import Data.Data (Data)
import GHC.Generics (Generic)
import Kitty.CExpr.Cat.TargetOb (TargetOb, TargetObTC1)
import Test.QuickCheck (Arbitrary (..))

data Unit0 a = Unit0 deriving (Generic, Data, Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Serialise (Unit0 a) where
  encode Unit0 = encode ()

  decode = do
    () <- decode
    pure Unit0

instance Arbitrary (Unit0 a) where
  arbitrary = pure Unit0

makeClassy ''Unit0

instance Semigroup (Unit0 a) where
  _ <> _ = Unit0

instance Monoid (Unit0 a) where
  mempty = Unit0

  mconcat _ = Unit0

instance AsEmpty (Unit0 a)

instance Lookup (Unit0 a)

instance NFData (Unit0 a)

type instance TargetOb (Unit0 a) = Unit0 a

type instance TargetObTC1 Unit0 = Unit0
