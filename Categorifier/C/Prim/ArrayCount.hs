{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Counting primitives.
module Categorifier.C.Prim.ArrayCount
  ( ArrayCount (..),

    -- * 'Control.Lens.Iso'
    _ArrayCount, -- from makePrisms
  )
where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData (..), NFData1 (..))
import Control.Lens.TH (makePrisms, makeWrapped)
import Data.Data (Data)
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Show1 (..))
import Data.Functor.Const (Const (..))
import Data.Hashable (Hashable (..))
import Data.Hashable.Lifted (Hashable1 (..))
import Data.Semigroup (Sum (..))
import GHC.Generics (Generic, Generic1)

-- | A count of the given (phantom) type @a@.
newtype ArrayCount a = ArrayCount {unArrayCount :: Int}
  deriving stock (Generic, Generic1, Data)
  deriving newtype (Show, Eq, Ord, NFData, Hashable, Serialise)
  deriving (Semigroup, Monoid) via (Sum Int)
  deriving (Eq1, Ord1, Hashable1, NFData1) via (Const Int)

makeWrapped ''ArrayCount

makePrisms ''ArrayCount

-- | We cannot derive this via @'Const' 'Int'@, because it won't say 'ArrayCount' anywhere!
instance Show1 ArrayCount where
  liftShowsPrec _ _ prec x = showsPrec prec x
