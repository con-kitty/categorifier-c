{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Kitty.HK2.Const
-- Copyright   :  (c) Kittyhawk 2018
-- License     :  AllRightsReversed
-- Maintainer  :  Matthew Peddie <matt.peddie@kittyhawk.aero>
-- Stability   :  provisional
-- Portability :  GHC
--
-- This is an "HK2"-flavored version of the 'Data.Functor.Const.Const' functor.
module Kitty.HK2.Const
  ( ConstHK2 (..),

    -- ** 'Control.Lens.Iso'morphism
    _ConstHK2, -- from 'makePrism's
  )
where

import Accessors (Lookup (..))
import qualified Barbies
import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData (..))
import Control.Lens.TH (makePrisms, makeWrapped)
import Data.Data (Data)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Kitty.CExpr.Cat.TargetOb (TargetOb, TargetObTC1)
import Test.QuickCheck (Arbitrary (..))

newtype ConstHK2 a (x :: (Type -> Type) -> Type) (f :: Type -> Type) = ConstHK2 {getConstHK2 :: a}
  deriving newtype (Arbitrary, Serialise, Eq, Ord, Show, Read, NFData)
  deriving stock (Generic, Data)

makePrisms ''ConstHK2

makeWrapped ''ConstHK2

type instance TargetOb (ConstHK2 a x f) = ConstHK2 (TargetOb a) x (TargetObTC1 f)

instance Lookup a => Lookup (ConstHK2 a x f) where
  toAccessorTree lns = toAccessorTree (lns . _ConstHK2)

instance Barbies.FunctorB (ConstHK2 a x)

instance Barbies.ConstraintsB (ConstHK2 a x)

instance Barbies.TraversableB (ConstHK2 a x)

instance Monoid a => Barbies.ApplicativeB (ConstHK2 a x)
