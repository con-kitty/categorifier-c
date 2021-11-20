{-# LANGUAGE ConstraintKinds #-}

module Kitty.KTypes.KLiteral
  ( KLiteral (..),
    KLiteralPrimitives,
    false,
    true,
  )
where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Proxy (Proxy (..))
import Data.Word (Word8, Word16, Word32, Word64)
import Kitty.Prim (IsPrimitive)

class IsPrimitive a => KLiteral f a where
  -- | This lifts a Haskell expression to a flight controller literal.
  --
  --  __NB__: This should not be used on actual Haskell literals (e.g., `True`, @0@, @1.523@). We
  --          explicitly provide `true` and `false` here for the booleans and as every
  --         `Kitty.KTypes.KType1.KType1` instance implies `Num`, numeric literals are automatically
  --          interpreted in the correct @f@.
  kliteral :: a -> f a

instance IsPrimitive a => KLiteral Proxy a where
  kliteral = const Proxy

true :: KLiteral f Bool => f Bool
true = kliteral True

false :: KLiteral f Bool => f Bool
false = kliteral False

-- | Does @f@ have a 'KLiteral' instance for of all the integer primitive types?
type KLiteralPrimitives f =
  ( KLiteral f Bool,
    KLiteral f Word8,
    KLiteral f Word16,
    KLiteral f Word32,
    KLiteral f Word64,
    KLiteral f Int8,
    KLiteral f Int16,
    KLiteral f Int32,
    KLiteral f Int64,
    KLiteral f Float,
    KLiteral f Double
  )
