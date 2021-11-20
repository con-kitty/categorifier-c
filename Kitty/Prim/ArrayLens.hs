{-# LANGUAGE RankNTypes #-}

-- | Utilities for focusing on primitives with 'Control.Lens.Lens'es.
module Kitty.Prim.ArrayLens
  ( -- * Newtype wrappers for 'Control.Lens.Lens'es
    ArrayLens (..),
    FullBlownArrayLens (..),

    -- * 'Arrays' of wrapped lenses
    lensArray,
    fullBlownLensArray,
    countLenses,
  )
where

import qualified Barbies
import Control.Lens (Lens')
import Kitty.Prim.ArrayCount (ArrayCount, _ArrayCount)
import Kitty.Prim.Base (Arrays, IsPrimitive, prims_)

-- | A 'FullBlownArrayLens' is fully polymorphic in the underlying functors @f@ and @g@; morally:
--
-- > Lens' (Arrays f) (f a)
--
-- This is needed in some situations where an 'ArrayLens' is insufficiently polymorphic.  If
-- possible, you should use an ordinary 'ArrayLens' instead (you will get better error messages, for
-- one).
newtype FullBlownArrayLens a = FullBlownArrayLens
  {getFullBlownArrayLens :: forall f g. Functor g => (f a -> g (f a)) -> Arrays f -> g (Arrays f)}

-- | An 'ArrayLens' is what it sounds like -- a 'Lens'' into 'Arrays'.
newtype ArrayLens f a = ArrayLens {getArrayLens :: Lens' (Arrays f) (f a)}

-- Using 'HK1PolyPure' to fill these in only works because every 'Arrays' field corresponds
-- to a unique primitive.  If we had repeats, the type-level @Rep@ trick would not work, and we
-- would have to write things out.

lensArray :: Arrays (ArrayLens f)
lensArray = Barbies.bpureC @IsPrimitive (ArrayLens prims_)

fullBlownLensArray :: Arrays FullBlownArrayLens
fullBlownLensArray = Barbies.bpureC @IsPrimitive (FullBlownArrayLens prims_)

countLenses ::
  Functor g =>
  [(Int -> g Int) -> Arrays ArrayCount -> g (Arrays ArrayCount)]
countLenses = Barbies.bfoldMapC @IsPrimitive (\(ArrayLens x) -> pure $ x . _ArrayCount) lensArray
