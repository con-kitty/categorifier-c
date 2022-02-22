{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- https://ghc.haskell.org/trac/ghc/ticket/14633:

-- | Class for turning data into arrays of consituent types.
module Categorifier.C.PolyVec
  ( PolyVec,
    AllPolyVec,
    pvlengths,

    -- * non-incremental wrappers
    pdevectorize,
    pdevectorizeIncremental,
    pvectorize,
    pvectorizeIncremental,

    -- * meh
    zeroValue,

    -- * errors
    FromArraysError,
    ToArraysError,
  )
where

import qualified Barbies
import Categorifier.C.CTypes.GArrays (FromArraysError, GArrays (..), ToArraysError)
import Categorifier.C.KTypes.KLiteral (KLiteral (..), KLiteralPrimitives)
import Categorifier.C.Prim
  ( ArrayCount (..),
    ArrayMVec (..),
    Arrays (..),
    IsPrimitive,
    PolyPrimMVector,
    _ArrayMVec,
  )
import qualified Categorifier.Common.IO.Exception as Exception
import Control.Lens (over)
import Control.Monad.Primitive (PrimState)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import qualified Data.Bifunctor as Bifunctor (second)
import Data.Bitraversable (bitraverse)
import Data.Functor.Compose (Compose (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import Data.STRef (newSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import Data.Word (Word16, Word32, Word64, Word8)

-- | Type alias for ease of compatability.
type PolyVec f a = GArrays f a

-- | Does @f@ have a 'PolyVec' instance for of all the primitive types?
type AllPolyVec f =
  ( PolyVec f (f Bool),
    PolyVec f (f Word8),
    PolyVec f (f Word16),
    PolyVec f (f Word32),
    PolyVec f (f Word64),
    PolyVec f (f Int8),
    PolyVec f (f Int16),
    PolyVec f (f Int32),
    PolyVec f (f Int64),
    PolyVec f (f Float),
    PolyVec f (f Double)
  )

-- | This is equivalent to memset 0. All fields are False or 0.
zeroValue :: forall f a. (PolyVec f a, KLiteralPrimitives f) => Proxy f -> a
zeroValue =
  either Exception.impureThrow id
    . pdevectorize
    . Barbies.bmapC @(KLiteral f) (Compose . flip V.replicate (kliteral @f 0) . unArrayCount)
    . flip pvlengths (Proxy @a)

-- | Compute the number of primitives of each type contained in the Haskell data.
pvlengths :: GArrays f a => Proxy f -> Proxy a -> Arrays ArrayCount
pvlengths = arraysCount
{-# INLINEABLE pvlengths #-}

-- | Convert from a Haskell type to the 'Arrays' representation.
pvectorize ::
  forall v f a mv.
  ( PolyPrimMVector v f,
    PolyVec f a,
    mv ~ GV.Mutable v,
    forall b. IsPrimitive b => MV.MVector mv (f b),
    forall b. IsPrimitive b => GV.Vector v (f b)
  ) =>
  a ->
  Either ToArraysError (Arrays (Compose v f))
pvectorize a = runST action
  where
    arrayLengths :: Arrays ArrayCount
    arrayLengths = arraysCount (Proxy @f) (Proxy @a)
    allocate ::
      forall b s. MV.MVector (GV.Mutable v) (f b) => ArrayCount b -> ST s (ArrayMVec v s f b)
    allocate = fmap ArrayMVec . MV.new . unArrayCount
    freeze ::
      forall s b. GV.Vector v (f b) => ArrayMVec v (PrimState (ST s)) f b -> ST s (Compose v f b)
    freeze = fmap Compose . GV.freeze . unArrayMVec
    action :: ST s (Either ToArraysError (Arrays (Compose v f)))
    action = do
      mutableArrays :: Arrays (ArrayMVec v s f) <-
        Barbies.btraverseC @IsPrimitive allocate arrayLengths
      maybeErrors <- do
        arrayCountRef <- newSTRef mempty
        toArrays mutableArrays arrayCountRef a
      bitraverse pure (const $ Barbies.btraverseC @IsPrimitive freeze mutableArrays) maybeErrors
{-# INLINEABLE pvectorize #-}

-- | Turn Haskell data into arrays of primitives.
pvectorizeIncremental ::
  forall a v mv s f.
  ( mv ~ GV.Mutable v,
    forall b. IsPrimitive b => MV.MVector mv (f b),
    PolyVec f a
  ) =>
  -- | thing to vectorize into arrays
  a ->
  -- | mutable output buffers
  Arrays (ArrayMVec v s f) ->
  -- | next indices to be written to in
  -- output buffers
  Arrays ArrayCount ->
  -- | new indices after possible writes
  ST s (Either ToArraysError (Arrays ArrayCount))
pvectorizeIncremental value arrays offsets = runExceptT $ do
  arrayCountRef <- lift $ newSTRef mempty
  ExceptT $ toArrays offsetMArrays arrayCountRef value
  pure . (offsets <>) $ pvlengths (Proxy @f) (Proxy @a)
  where
    offsetMArrays =
      Barbies.bzipWithC @IsPrimitive
        (\(ArrayCount o) -> over _ArrayMVec $ MV.unsafeDrop o)
        offsets
        arrays
{-# INLINEABLE pvectorizeIncremental #-}

-- | Convert from a set of 'Arrays' to a Haskell type.
pdevectorize ::
  forall v f a.
  ( (forall b. IsPrimitive b => GV.Vector v (f b)),
    PolyVec f a
  ) =>
  Arrays (Compose v f) ->
  Either FromArraysError a
pdevectorize = Bifunctor.second fst . pdevectorizeIncremental
{-# INLINEABLE pdevectorize #-}

-- | Incrementally turn arrays of primitives into Haskell data, returning either the Haskell data
-- and left-over primitives, or an error message if we ran out of primitives.
pdevectorizeIncremental ::
  forall v f a.
  ( (forall b. IsPrimitive b => GV.Vector v (f b)),
    PolyVec f a
  ) =>
  -- | arrays to devectorize into @a@
  Arrays (Compose v f) ->
  -- | result and remainder arrays
  Either FromArraysError (a, Arrays (Compose v f))
pdevectorizeIncremental = fromArrays
{-# INLINEABLE pdevectorizeIncremental #-}
