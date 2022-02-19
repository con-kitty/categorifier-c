{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pattern synonyms for common applications of 'Prim'.
module Kitty.Prim.Patterns
  ( -- * Patterns for @'Prim' 'Proxy'@ (i.e. an enum of primitive types).
    pattern BoolType,
    pattern Int8Type,
    pattern Int16Type,
    pattern Int32Type,
    pattern Int64Type,
    pattern Word8Type,
    pattern Word16Type,
    pattern Word32Type,
    pattern Word64Type,
    pattern FloatType,
    pattern DoubleType,

    -- * Patterns for @'Prim' 'Identity'@ (i.e. one primitive value per constructor).
    pattern BoolVal,
    pattern Int8Val,
    pattern Int16Val,
    pattern Int32Val,
    pattern Int64Val,
    pattern Word8Val,
    pattern Word16Val,
    pattern Word32Val,
    pattern Word64Val,
    pattern FloatVal,
    pattern DoubleVal,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word32, Word64, Word8)
import Kitty.Prim.Base (Prim (..))

{-# COMPLETE
  BoolType,
  Int8Type,
  Int16Type,
  Int32Type,
  Int64Type,
  Word8Type,
  Word16Type,
  Word32Type,
  Word64Type,
  FloatType,
  DoubleType
  #-}

pattern BoolType :: Prim Proxy
pattern BoolType = PrimBool Proxy

pattern Int8Type :: Prim Proxy
pattern Int8Type = PrimInt8 Proxy

pattern Int16Type :: Prim Proxy
pattern Int16Type = PrimInt16 Proxy

pattern Int32Type :: Prim Proxy
pattern Int32Type = PrimInt32 Proxy

pattern Int64Type :: Prim Proxy
pattern Int64Type = PrimInt64 Proxy

pattern Word8Type :: Prim Proxy
pattern Word8Type = PrimWord8 Proxy

pattern Word16Type :: Prim Proxy
pattern Word16Type = PrimWord16 Proxy

pattern Word32Type :: Prim Proxy
pattern Word32Type = PrimWord32 Proxy

pattern Word64Type :: Prim Proxy
pattern Word64Type = PrimWord64 Proxy

pattern FloatType :: Prim Proxy
pattern FloatType = PrimFloat Proxy

pattern DoubleType :: Prim Proxy
pattern DoubleType = PrimDouble Proxy

{-# COMPLETE
  BoolVal,
  Int8Val,
  Int16Val,
  Int32Val,
  Int64Val,
  Word8Val,
  Word16Val,
  Word32Val,
  Word64Val,
  FloatVal,
  DoubleVal
  #-}

pattern BoolVal :: Bool -> Prim Identity
pattern BoolVal x = PrimBool (Identity x)

pattern Int8Val :: Int8 -> Prim Identity
pattern Int8Val x = PrimInt8 (Identity x)

pattern Int16Val :: Int16 -> Prim Identity
pattern Int16Val x = PrimInt16 (Identity x)

pattern Int32Val :: Int32 -> Prim Identity
pattern Int32Val x = PrimInt32 (Identity x)

pattern Int64Val :: Int64 -> Prim Identity
pattern Int64Val x = PrimInt64 (Identity x)

pattern Word8Val :: Word8 -> Prim Identity
pattern Word8Val x = PrimWord8 (Identity x)

pattern Word16Val :: Word16 -> Prim Identity
pattern Word16Val x = PrimWord16 (Identity x)

pattern Word32Val :: Word32 -> Prim Identity
pattern Word32Val x = PrimWord32 (Identity x)

pattern Word64Val :: Word64 -> Prim Identity
pattern Word64Val x = PrimWord64 (Identity x)

pattern FloatVal :: Float -> Prim Identity
pattern FloatVal x = PrimFloat (Identity x)

pattern DoubleVal :: Double -> Prim Identity
pattern DoubleVal x = PrimDouble (Identity x)
