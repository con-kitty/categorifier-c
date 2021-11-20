{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | 'Arrays' wherein only one underlying type is needed (e.g. a counter, a label or an AST type).
module Kitty.Prim.ConstArrays
  ( ConstArrays (..),

    -- * 'Control.Lens.Iso'
    _ConstArrays, -- from 'makePrisms'
  )
where

import qualified Barbies
import Control.Lens.TH (makePrisms, makeWrapped)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Traversable (fmapDefault, foldMapDefault)
import GHC.Generics (Generic)
import Kitty.Prim.Base (Arrays (..))

-- | This is a version of 'Arrays' containing vectors of some value which has the same type for each
-- primitive (hence the use of 'Const').  It's mainly useful for its instances of standard type
-- classes ('Functor', 'Foldable', 'Traversable'), which you can use to operate uniformly on the
-- contained values.
newtype ConstArrays t e = ConstArrays {getConstArrays :: Arrays (Compose t (Const e))}
  -- Previously there were 'NFData' and 'Hashable' instances here, but 'Compose' doesn't have those.
  deriving (Eq, Ord, Show, Generic)

makeWrapped ''ConstArrays

makePrisms ''ConstArrays

instance Traversable t => Functor (ConstArrays t) where
  fmap = fmapDefault
  {-# INLINEABLE fmap #-}

instance Traversable t => Foldable (ConstArrays t) where
  foldMap = foldMapDefault
  {-# INLINEABLE foldMap #-}

instance Traversable t => Traversable (ConstArrays t) where
  traverse :: forall f b a. Applicative f => (a -> f b) -> ConstArrays t a -> f (ConstArrays t b)
  traverse f = fmap ConstArrays . Barbies.btraverse goF . getConstArrays
    where
      goF :: forall e. Compose t (Const a) e -> f (Compose t (Const b) e)
      goF = fmap Compose . traverse (fmap Const . f . getConst) . getCompose
  {-# INLINEABLE traverse #-}
