-- | Higher-kinded version of 'Data.Bits.Bits'.
module Categorifier.C.KTypes.KBits
  ( KBits (..),
  )
where

import Data.Proxy (Proxy (..))

class KBits f a where
  testBit :: f a -> Int -> f Bool
  setBitTo :: f a -> Int -> f Bool -> f a
  zeroBits :: f a

instance KBits Proxy a where
  testBit Proxy _ = Proxy
  setBitTo Proxy _ Proxy = Proxy
  zeroBits = Proxy
