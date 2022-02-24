{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Naming primitives.
module Categorifier.C.Prim.ArrayName
  ( ArrayName (..),

    -- * 'Control.Lens.Iso'
    _ArrayName, -- from makePrisms
  )
where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData (..), NFData1 (..))
import Control.Lens.TH (makePrisms, makeWrapped)
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Show1 (..))
import Data.Functor.Const (Const (..))
import Data.Hashable (Hashable (..))
import Data.Hashable.Lifted (Hashable1 (..))
import qualified Data.Text as Text (Text)

-- | A string value naming the type @a@.
newtype ArrayName a = ArrayName {getArrayName :: Text.Text}
  deriving newtype (Show, Eq, Ord, NFData, Hashable)
  deriving (Hashable1, NFData1, Eq1, Ord1) via Const Text.Text
  deriving (Serialise) via Const Text.Text a

-- | We cannot derive this via @'Const' 'Text.Text'@, because it won't say 'ArrayName' anywhere!
instance Show1 ArrayName where
  liftShowsPrec _ _ prec x = showsPrec prec x

makeWrapped ''ArrayName

makePrisms ''ArrayName
