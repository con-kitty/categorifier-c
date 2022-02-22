{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | This module exists only to fill in various gaps in the @barbies@ library for working with
-- higher-kinded functors.
--
-- __There are no new abstractions or concepts introduced in this module__.  Everything here is
-- defined in terms of functions from "Barbies" and components from @base@.  Everything in this
-- module should eventually move upstream into the main @barbies@ library.
module Categorifier.C.Barbies
  ( -- * Traversals

    -- ** Folds with specified associativity
    bfoldr,
    bfoldl,
    bfoldrC,
    bfoldlC,

    -- ** Effects-only traversal
    btraverseC_,

    -- * Zips

    -- ** Side-effecting zips
    bzipWithM,
    bzipWithMC,
    bzipWith3M,
    bzipWith3MC,

    -- ** Effects-only zips
    bzipWithM_,
    bzipWithMC_,
    bzipWith3M_,
    bzipWith3MC_,

    -- * Constraint support
    type (&+&),
  )
where

import qualified Barbies
import Data.Functor (void)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Product (Product (..))
import Data.Monoid (Endo (..))

-- So we can enable @UndecidableSuperClasses@ to define `&+&`.
{-# ANN module "HLint: ignore Avoid restricted extensions" #-}

-- | A `foldr` from "left" to "right" across elements of a `Barbies.TraversableB` structure.
bfoldr ::
  forall x f b.
  Barbies.TraversableB x =>
  (forall a. f a -> b -> b) ->
  b ->
  x f ->
  b
bfoldr fr zr tr = appEndo (Barbies.bfoldMap (Endo . fr) tr) zr
{-# INLINEABLE bfoldr #-}

-- | `bfoldr` with a constraint @c@ on field types.
bfoldrC ::
  forall c x f b.
  (Barbies.TraversableB x, Barbies.ConstraintsB x, Barbies.AllB c x) =>
  (forall a. c a => f a -> b -> b) ->
  b ->
  x f ->
  b
bfoldrC fr zr tr = appEndo (Barbies.bfoldMapC @c (Endo . fr) tr) zr
{-# INLINEABLE bfoldrC #-}

-- | A `foldl` from "left" to "right" across elements of a `Barbies.TraversableB` structure.
--
-- Note that this function is lazy in the accumulator.  Higher-kinded functors ("barbies") are
-- typically products of fixed size rather than recursive sums, so strict accumulation is not
-- critical as with left-associative folds on lazy lists.
bfoldl ::
  forall x f b.
  Barbies.TraversableB x =>
  (forall a. b -> f a -> b) ->
  b ->
  x f ->
  b
bfoldl f z t = bfoldr f' id t z
  where
    f' :: forall a. f a -> (b -> b) -> b -> b
    f' x acc z0 = acc $ f z0 x
{-# INLINEABLE bfoldl #-}

-- | `bfoldl` with a constraint @c@ on field types.
bfoldlC ::
  forall c x f b.
  (Barbies.TraversableB x, Barbies.ConstraintsB x, Barbies.AllB c x) =>
  (forall a. c a => b -> f a -> b) ->
  b ->
  x f ->
  b
bfoldlC f z t = bfoldrC @c f' id t z
  where
    f' :: forall a. c a => f a -> (b -> b) -> b -> b
    f' x acc z0 = acc $ f z0 x
{-# INLINEABLE bfoldlC #-}

-- | This is like `Barbies.btraverseC` where we only care about the side-effects in @m@.
btraverseC_ ::
  forall c x f b m.
  (Barbies.TraversableB x, Barbies.ConstraintsB x, Barbies.AllB c x, Applicative m) =>
  (forall a. c a => f a -> m b) ->
  x f ->
  m ()
btraverseC_ f = void . gone
  where
    gone :: x f -> m (x (Const ()))
    gone = Barbies.btraverseC @c (fmap (const $ Const ()) . f)
{-# INLINEABLE btraverseC_ #-}

-- | A side-effecting zip between two `Barbies.ApplicativeB` structures.
bzipWithM ::
  forall x f g h m.
  (Barbies.ApplicativeB x, Barbies.TraversableB x, Applicative m) =>
  (forall a. f a -> g a -> m (h a)) ->
  x f ->
  x g ->
  m (x h)
bzipWithM f xf = Barbies.bsequence . Barbies.bzipWith (\fa -> Compose . f fa) xf
{-# INLINEABLE bzipWithM #-}

-- | `bzipWithM` where we only care about the side-effects in @m@.
bzipWithM_ ::
  forall x f g h m.
  (Barbies.ApplicativeB x, Barbies.TraversableB x, Applicative m) =>
  (forall a. f a -> g a -> m h) ->
  x f ->
  x g ->
  m ()
bzipWithM_ f xf = void . bzipWithM (\fa -> fmap (const $ Const ()) . f fa) xf
{-# INLINEABLE bzipWithM_ #-}

-- | `bzipWithM` with a constraint @c@ on field types.
bzipWithMC ::
  forall c x f g h m.
  ( Barbies.ApplicativeB x,
    Barbies.TraversableB x,
    Barbies.ConstraintsB x,
    Barbies.AllB c x,
    Applicative m
  ) =>
  (forall a. c a => f a -> g a -> m (h a)) ->
  x f ->
  x g ->
  m (x h)
bzipWithMC f xf = Barbies.bsequence . Barbies.bzipWithC @c (\fa -> Compose . f fa) xf
{-# INLINEABLE bzipWithMC #-}

-- | `bzipWithMC` where we only care about the side-effects in @m@.
bzipWithMC_ ::
  forall c x f g h m.
  ( Barbies.ApplicativeB x,
    Barbies.TraversableB x,
    Barbies.ConstraintsB x,
    Barbies.AllB c x,
    Applicative m
  ) =>
  (forall a. c a => f a -> g a -> m h) ->
  x f ->
  x g ->
  m ()
bzipWithMC_ f xf = void . gone
  where
    gone :: x g -> m (x (Const ()))
    gone = bzipWithMC @c (\fa -> fmap (const $ Const ()) . f fa) xf
{-# INLINEABLE bzipWithMC_ #-}

-- | A side-effecting zip between three `Barbies.ApplicativeB` structures.
bzipWith3M ::
  forall x f g h k m.
  (Barbies.ApplicativeB x, Barbies.TraversableB x, Applicative m) =>
  (forall a. f a -> g a -> h a -> m (k a)) ->
  x f ->
  x g ->
  x h ->
  m (x k)
bzipWith3M f xf xg =
  Barbies.bsequence
    . Barbies.bzipWith (\fa (Pair ga ha) -> Compose $ f fa ga ha) xf
    . Barbies.bzipWith Pair xg
{-# INLINEABLE bzipWith3M #-}

-- | `bzipWith3M` where we only care about the side-effects in @m@.
bzipWith3M_ ::
  forall x f g h k m.
  (Barbies.ApplicativeB x, Barbies.TraversableB x, Applicative m) =>
  (forall a. f a -> g a -> h a -> m k) ->
  x f ->
  x g ->
  x h ->
  m ()
bzipWith3M_ f xf xg =
  void . bzipWith3M (\fa ga -> fmap (const $ Const ()) . f fa ga) xf xg
{-# INLINEABLE bzipWith3M_ #-}

-- | `bzipWith3M` with a constraint @c@ on the field types.
bzipWith3MC ::
  forall c x f g h k m.
  ( Barbies.ApplicativeB x,
    Barbies.TraversableB x,
    Barbies.ConstraintsB x,
    Barbies.AllB c x,
    Applicative m
  ) =>
  (forall a. c a => f a -> g a -> h a -> m (k a)) ->
  x f ->
  x g ->
  x h ->
  m (x k)
bzipWith3MC f xf xg =
  Barbies.bsequence
    . Barbies.bzipWithC @c (\fa (Pair ga ha) -> Compose $ f fa ga ha) xf
    . Barbies.bzipWith Pair xg
{-# INLINEABLE bzipWith3MC #-}

-- | `bzipWith3MC` where we only care about the side-effects in @m@.
bzipWith3MC_ ::
  forall c x f g h k m.
  ( Barbies.ApplicativeB x,
    Barbies.TraversableB x,
    Barbies.ConstraintsB x,
    Barbies.AllB c x,
    Applicative m
  ) =>
  (forall a. c a => f a -> g a -> h a -> m k) ->
  x f ->
  x g ->
  x h ->
  m ()
bzipWith3MC_ f xf xg = void . gone
  where
    gone :: x h -> m (x (Const ()))
    gone = bzipWith3MC @c (\fa ga -> fmap (const $ Const ()) . f fa ga) xf xg
{-# INLINEABLE bzipWith3MC_ #-}

-- | This class is like a `Data.Kind.Constraint`-kinded `Data.Functor.Product`. It's very useful for
--   specifying the constraint on operations like `Barbies.bmapC`.
class (a t, b t) => (a &+& b) t

instance (a t, b t) => (a &+& b) t

infixr 3 &+&
