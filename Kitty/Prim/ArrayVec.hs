{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Higher-kinded wrappers.
module Kitty.Prim.ArrayVec
  ( -- * Storing vectors of elements
    ArrayMVec (..),

    -- ** 'Control.Lens.Iso'
    _ArrayMVec,

    -- ** Constraint synonyms
    PolyPrimVector,
    PolyPrimMVector,
  )
where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData (..))
import Control.Lens (Index, IxValue, Ixed (..))
import Control.Lens.TH (makePrisms, makeWrapped)
import Data.Data (Data, Typeable)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import GHC.Generics (Generic)
import Kitty.Prim.Base (Primitives)
import Kitty.TypeUtils.Lists (ConstraintListToTuple, TypeMapConstraintCompose2)

-- | This expands to an constraint of 'G.Vector' @v (f a)@ for all primitive types @a@.
type PolyPrimVector v f =
  ConstraintListToTuple (TypeMapConstraintCompose2 G.Vector v f Primitives)

-- | This expands to constraints of 'MG.MVector' ('G.Mutable' @v@) @(f a)@ and 'G.Vector' @v (f a)@
-- for all primitive types @a@.
type PolyPrimMVector v f =
  ( PolyPrimVector v f,
    ConstraintListToTuple (TypeMapConstraintCompose2 MG.MVector (G.Mutable v) f Primitives)
  )

newtype ArrayMVec v s f a = ArrayMVec {unArrayMVec :: G.Mutable v s (f a)}
  deriving (Generic)

-- GHC cannot derive Traversable for type role-related reasons; see below for the instance.

deriving newtype instance Semigroup (G.Mutable v s (f a)) => Semigroup (ArrayMVec v s f a)

deriving newtype instance Monoid (G.Mutable v s (f a)) => Monoid (ArrayMVec v s f a)

deriving newtype instance Eq (G.Mutable v s (f a)) => Eq (ArrayMVec v s f a)

deriving newtype instance Ord (G.Mutable v s (f a)) => Ord (ArrayMVec v s f a)

deriving newtype instance Show (G.Mutable v s (f a)) => Show (ArrayMVec v s f a)

deriving newtype instance NFData (G.Mutable v s (f a)) => NFData (ArrayMVec v s f a)

deriving newtype instance Serialise (G.Mutable v s (f a)) => Serialise (ArrayMVec v s f a)

deriving stock instance (Functor f, Functor (G.Mutable v s)) => Functor (ArrayMVec v s f)

deriving stock instance (Foldable f, Foldable (G.Mutable v s)) => Foldable (ArrayMVec v s f)

deriving stock instance
  (Traversable f, Traversable (G.Mutable v s)) =>
  Traversable (ArrayMVec v s f)

deriving stock instance
  ( Typeable v,
    Typeable f,
    Typeable s,
    Typeable a,
    Data (G.Mutable v s (f a)),
    Typeable (G.Mutable v s (f a))
  ) =>
  Data (ArrayMVec v s f a)

makeWrapped ''ArrayMVec

makePrisms ''ArrayMVec

type instance Index (ArrayMVec v s f a) = Index (G.Mutable v s (f a))

type instance IxValue (ArrayMVec v s f a) = IxValue (G.Mutable v s (f a))

instance Ixed (G.Mutable v s (f a)) => Ixed (ArrayMVec v s f a) where
  ix i f (ArrayMVec v) = ArrayMVec <$> ix i f v
