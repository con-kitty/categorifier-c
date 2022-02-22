{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module Categorifier.C.CExpr.Cat.TargetOb
  ( TargetOb,
    TargetObW (..),
    TargetObTC1,
  )
where

import Data.Either.Validation (Validation)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product)
import qualified Data.Functor.Rep as Representable
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy)
import qualified Data.Semigroup
import GHC.Generics ((:*:) (..), (:+:) (..))
import qualified GHC.Generics as G

-- | A newtype wrapper around `TargetOb`. `TargetOb` is a non-injective type family, and
-- using `TargetOb` directly results in higher simplifier ticks and longer build times.
newtype TargetObW (a :: k) = TargetObW {unTargetObW :: TargetOb a}

-- | 'TargetOb' maps a type into its representation in terms of @CExpr@.
--
-- For a higher-kinded data type @a `Categorifier.C.KTypes.C.C`@, its
-- 'TargetOb' is @a CExpr@. For a primitive type @a@, its 'TargetOb' is @CExpr
-- a@. For types consisting of @(,)@, @Either@ and @(->)@, their
-- 'TargetOb's are composed in the usual way.
type family TargetOb (a :: k) :: Type

type instance TargetOb () = ()

type instance TargetOb Int = Int

type instance TargetOb Integer = Integer

type instance TargetOb Char = Char

type instance TargetOb (a -> b) = TargetOb a -> TargetOb b

type instance TargetOb (Maybe a) = Maybe (TargetOb a)

type instance TargetOb (Compose f g a) = Compose f g (TargetOb a)

type instance TargetOb (Either a b) = Either (TargetOb a) (TargetOb b)

type instance TargetOb (Validation a b) = Validation (TargetOb a) (TargetOb b)

type instance TargetOb (Identity a) = Identity (TargetOb a)

type instance TargetOb (Const a b) = Const (TargetOb a) (TargetOb b)

type instance TargetOb (Data.Semigroup.Product a) = Data.Semigroup.Product (TargetOb a)

type instance TargetOb (Data.Semigroup.Sum a) = Data.Semigroup.Sum (TargetOb a)

type instance TargetOb (Proxy a) = ()

type instance TargetOb (Product f g a) = Product (TargetObTC1 f) (TargetObTC1 g) a

type instance TargetOb [a] = [TargetOb a]

type instance TargetOb (NonEmpty a) = NonEmpty (TargetOb a)

type instance TargetOb (Representable.WrappedRep f) = Representable.WrappedRep f

-- | @TargetOb@ for type constructors of kind @Type -> Type@.
type family TargetObTC1 (f :: Type -> Type) :: Type -> Type

type instance TargetObTC1 G.U1 = G.U1

type instance TargetObTC1 (G.K1 i c) = G.K1 i (TargetOb c)

type instance TargetObTC1 (G.M1 i c f) = G.M1 i c (TargetObTC1 f)

type instance TargetObTC1 (f :*: g) = TargetObTC1 f :*: TargetObTC1 g

type instance TargetObTC1 (f :+: g) = TargetObTC1 f :+: TargetObTC1 g

type instance TargetObTC1 (Product f g) = (Product (TargetObTC1 f) (TargetObTC1 g))

-------------------------------------------------------------------------------

-- * Generics

type instance TargetOb (G.U1 p) = (G.U1 p)

type instance TargetOb (G.K1 i c p) = G.K1 i (TargetOb c) p

type instance TargetOb (G.M1 i c f p) = G.M1 i c (TargetObTC1 f) p

type instance TargetOb ((f :*: g) p) = (TargetObTC1 f :*: TargetObTC1 g) p

type instance TargetOb ((f :+: g) p) = (TargetObTC1 f :+: TargetObTC1 g) p

-------------------------------------------------------------------------------

-- Tuples

type instance TargetOb (a, b) = (TargetOb a, TargetOb b)

type instance TargetOb (a, b, c) = (TargetOb a, TargetOb b, TargetOb c)

type instance TargetOb (a, b, c, d) = (TargetOb a, TargetOb b, TargetOb c, TargetOb d)

type instance
  TargetOb (a, b, c, d, e) =
    (TargetOb a, TargetOb b, TargetOb c, TargetOb d, TargetOb e)

type instance
  TargetOb (a, b, c, d, e, f) =
    (TargetOb a, TargetOb b, TargetOb c, TargetOb d, TargetOb e, TargetOb f)

type instance
  TargetOb (a, b, c, d, e, f, g) =
    (TargetOb a, TargetOb b, TargetOb c, TargetOb d, TargetOb e, TargetOb f, TargetOb g)

type instance
  TargetOb (a, b, c, d, e, f, g, h) =
    (TargetOb a, TargetOb b, TargetOb c, TargetOb d, TargetOb e, TargetOb f, TargetOb g, TargetOb h)

type instance
  TargetOb (a, b, c, d, e, f, g, h, i) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i
    )

type instance
  TargetOb (a, b, c, d, e, f, g, h, i, j) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i,
      TargetOb j
    )

type instance
  TargetOb (a, b, c, d, e, f, g, h, i, j, k) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i,
      TargetOb j,
      TargetOb k
    )

type instance
  TargetOb (a, b, c, d, e, f, g, h, i, j, k, l) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i,
      TargetOb j,
      TargetOb k,
      TargetOb l
    )

type instance
  TargetOb (a, b, c, d, e, f, g, h, i, j, k, l, m) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i,
      TargetOb j,
      TargetOb k,
      TargetOb l,
      TargetOb m
    )

type instance
  TargetOb (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i,
      TargetOb j,
      TargetOb k,
      TargetOb l,
      TargetOb m,
      TargetOb n
    )

type instance
  TargetOb (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i,
      TargetOb j,
      TargetOb k,
      TargetOb l,
      TargetOb m,
      TargetOb n,
      TargetOb o
    )

type instance
  TargetOb (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i,
      TargetOb j,
      TargetOb k,
      TargetOb l,
      TargetOb m,
      TargetOb n,
      TargetOb o,
      TargetOb p
    )

type instance
  TargetOb (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i,
      TargetOb j,
      TargetOb k,
      TargetOb l,
      TargetOb m,
      TargetOb n,
      TargetOb o,
      TargetOb p,
      TargetOb q
    )

type instance
  TargetOb (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i,
      TargetOb j,
      TargetOb k,
      TargetOb l,
      TargetOb m,
      TargetOb n,
      TargetOb o,
      TargetOb p,
      TargetOb q,
      TargetOb r
    )

type instance
  TargetOb (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i,
      TargetOb j,
      TargetOb k,
      TargetOb l,
      TargetOb m,
      TargetOb n,
      TargetOb o,
      TargetOb p,
      TargetOb q,
      TargetOb r,
      TargetOb s
    )

type instance
  TargetOb (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i,
      TargetOb j,
      TargetOb k,
      TargetOb l,
      TargetOb m,
      TargetOb n,
      TargetOb o,
      TargetOb p,
      TargetOb q,
      TargetOb r,
      TargetOb s,
      TargetOb t
    )

type instance
  TargetOb (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i,
      TargetOb j,
      TargetOb k,
      TargetOb l,
      TargetOb m,
      TargetOb n,
      TargetOb o,
      TargetOb p,
      TargetOb q,
      TargetOb r,
      TargetOb s,
      TargetOb t,
      TargetOb u
    )

type instance
  TargetOb (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =
    ( TargetOb a,
      TargetOb b,
      TargetOb c,
      TargetOb d,
      TargetOb e,
      TargetOb f,
      TargetOb g,
      TargetOb h,
      TargetOb i,
      TargetOb j,
      TargetOb k,
      TargetOb l,
      TargetOb m,
      TargetOb n,
      TargetOb o,
      TargetOb p,
      TargetOb q,
      TargetOb r,
      TargetOb s,
      TargetOb t,
      TargetOb u,
      TargetOb v
    )
