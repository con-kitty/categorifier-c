{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Some type-level tools for making a generic polyvectorize. To use
-- this, you may need to turn on a number of fun extensions, like
-- @DataKinds@, @ConstraintKinds@ and @PolyKinds@.
module Kitty.TypeUtils.Lists
  ( TypeMap,
    TypeMapConstraintCompose2,
    AllInstancesOf,
    ConstraintListToTuple,
  )
where

import Data.Kind (Constraint)

-- | This is 'Data.List.map' at the type level -- you can apply a higher-kinded type to a list of
-- types.
type family TypeMap f list where
  TypeMap f (a ': as) = f a ': TypeMap f as
  TypeMap f '[] = '[]

-- | This is just 'TypeMap', but sadly GHC does not allow partial application at the type level.
-- This means we can't build things up or tuple them first and then map; we have to make our @map@
-- function match exactly the shape of what we want to apply.
type family TypeMapConstraintCompose2 constraint x y list where
  TypeMapConstraintCompose2 _ _ _ '[] = '[]
  TypeMapConstraintCompose2 constraint x y (a ': as) =
    constraint x (y a) ': TypeMapConstraintCompose2 constraint x y as

-- | This type is of kind 'Constraint' and lets you apply a type class
-- constraint to all the members of a type-level list.
--
-- Example:
--
--   butts :: AllInstancesOf Num '[a, b] => a -> a -> b -> b -> (a, b)
--   butts x y a b = (x + y, a + b)
type AllInstancesOf typeclass list =
  ConstraintListToTuple (TypeMap typeclass list)

-- | Convert between type-level lists of 'Constraint's and tuples.
-- The trick here is that tuples all of whose member types are of kind
-- 'Constraint' are also of kind 'Constraint' -- this lets us avoid
-- the limitations GHC puts on the kinds involved with type families.
type family
  ConstraintListToTuple (list :: [Constraint]) =
    (tuple :: Constraint) | tuple -> list
  where
  ConstraintListToTuple '[] = ()
  ConstraintListToTuple (a ': as) = (a, ConstraintListToTuple as)
