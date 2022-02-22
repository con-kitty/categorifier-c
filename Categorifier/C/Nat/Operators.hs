{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}

module Categorifier.C.Nat.Operators
  ( type (*),
    type (+),
    type (-),
    type (<=),
  )
where

import qualified Data.Type.Nat as Nat

-- | Rename of `Nat.Mult`, equivalent of `GHC.TypeNats.*`.
type n * m = Nat.Mult n m

-- | Rename of `Nat.Plus`, equivalent of `GHC.TypeNats.+`.
type n + m = Nat.Plus n m

-- | Equivalent of `GHC.TypeNats.-`.
type family (n :: Nat.Nat) - (m :: Nat.Nat) :: Nat.Nat where
  n - 'Nat.Z = n
  'Nat.S n - 'Nat.S m = n - m

-- | Equivalent of `GHC.TypeNats.<=`.
type family (n :: Nat.Nat) <= (m :: Nat.Nat) :: Bool where
  'Nat.Z <= m = 'True
  'Nat.S n <= 'Nat.Z = 'False
  'Nat.S n <= 'Nat.S m = n <= m
