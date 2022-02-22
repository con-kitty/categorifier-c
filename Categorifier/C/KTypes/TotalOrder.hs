-- | Total ordering on code generation primitive values
module Categorifier.C.KTypes.TotalOrder
  ( KOrd (..),
    kMinimum,
    kMaximum,
  )
where

import Categorifier.C.KTypes.Equality (KEq)
import Data.Foldable (foldl', toList)

infix 4 .<, .<=, .>, .>=

class KEq f a => KOrd f a | a -> f where
  -- not 'f a -> f a -> f Bool' so that we can have 'KEnum'
  (.<) :: a -> a -> f Bool

  (.<=) :: a -> a -> f Bool

  (.>) :: a -> a -> f Bool

  (.>=) :: a -> a -> f Bool

  kMin :: a -> a -> a

  kMax :: a -> a -> a

kMinimum :: (Foldable g, KOrd f a) => g a -> a
kMinimum xs = case toList xs of
  [] -> error "kMinimum: empty structure"
  y0 : ys -> foldl' kMin y0 ys

kMaximum :: (Foldable g, KOrd f a) => g a -> a
kMaximum xs = case toList xs of
  [] -> error "kMaximum: empty structure"
  y0 : ys -> foldl' kMax y0 ys
