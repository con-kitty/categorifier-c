{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This is a curated version that adds additional third-party orphan instances.
module Kitty.Cat.Client
  ( HasRep (..),
    deriveHasRep,
  )
where

import qualified Barbies
import qualified Barbies.Constraints as Barbies
import qualified Control.Lens.Internal.Iso as Lens
import Data.Either.Validation (Validation)
import Data.Tree (Tree)
import Kitty.Cat.Client.Internal (deriveHasRep)
import Kitty.Plugin.Client (HasRep (..))

deriveHasRep ''Barbies.Dict
deriveHasRep ''Barbies.Unit
deriveHasRep ''Lens.Exchange
deriveHasRep ''Tree
deriveHasRep ''Validation
