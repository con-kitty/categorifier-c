-- | Equality of primitive values
module Categorifier.C.KTypes.Equality
  ( KEq (..),
  )
where

import Categorifier.C.KTypes.BooleanLogic (KAnd)

infix 4 .==, ./=

class KAnd f => KEq f a | a -> f where
  -- not 'f a -> f a -> f Bool' so that we can have 'Categorifier.C.KTypes.KEnum.KEnum'
  (.==) :: a -> a -> f Bool

  (./=) :: a -> a -> f Bool
