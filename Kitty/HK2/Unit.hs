{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Kitty.HK2.Unit
-- Copyright   :  (c) Kittyhawk 2028
-- License     :  AllRightsReversed
-- Maintainer  :  Matthew Peddie <matt.peddie@kittyhawk.aero>
-- Stability   :  provisional
-- Portability :  GHC
--
-- This is a higher-kinded version of unit @()@.
module Kitty.HK2.Unit
  ( Unit2 (..),
    HasUnit2 (..),
  )
where

import Accessors (Lookup (..))
import Codec.Serialise (Serialise (..))
import Control.DeepSeq (NFData (..))
import Control.Lens (AsEmpty (..), makeClassy)
import Data.Data (Data)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))

data Unit2 (x :: (Type -> Type) -> Type) (f :: Type -> Type) = Unit2
  deriving (Generic, Data, Eq, Ord, Show, Read)

instance Serialise (Unit2 a f) where
  encode Unit2 = encode ()

  decode = do
    () <- decode
    pure Unit2

instance Arbitrary (Unit2 a f) where
  arbitrary = pure Unit2

makeClassy ''Unit2

instance Semigroup (Unit2 a f) where
  _ <> _ = Unit2

instance Monoid (Unit2 a f) where
  mempty = Unit2

  mconcat _ = Unit2

instance AsEmpty (Unit2 a f)

instance NFData (Unit2 a f)

instance Lookup (Unit2 a f)
