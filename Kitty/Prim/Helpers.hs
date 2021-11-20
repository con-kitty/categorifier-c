{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Assorted utilities for working with primitive types.  Many are just convenience wrappers for
-- functionality provided by 'IsPrimitive'.
module Kitty.Prim.Helpers
  ( -- * Subsets of primitive types
    allPrimTypes,
    numericPrimTypes,
    floatingPrimTypes,
    integralPrimTypes,
    signedIntPrimTypes,
    unsignedIntPrimTypes,

    -- * Helpers for the 'IsPrimitive' class
    toPrimVal,
    primSizeBytes,
    primTypeProxy,
    primTypeOf,
    primTypeOf',
    primCName,

    -- * Operations on 'Prim'
    getPrimHaskellName,
    getIsPrimitiveRep,
    getPrimCName,
    getPrimCNameNoT,

    -- * Collections
    arrayNames,
    arrayPrims,
    arrayPrimTypes,
    arrayCMinimumDefines,
    arrayCMaximumDefines,
  )
where

import qualified Barbies
import Control.Lens (over)
import Data.Bifunctor (bimap)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged (..))
import qualified Data.Text as Text (Text, pack, stripSuffix)
import qualified Data.Text.Lens as Text (unpacked)
import qualified Data.Type.Nat as Nat
import Data.Typeable (TypeRep, typeRep)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Generics.Deriving.Enum (GEnum (..))
import Kitty.Prim.ArrayName (ArrayName (..))
import Kitty.Prim.Base (Arrays, IsPrimitive (..), Prim, PrimType, PrimVal, polyIndexArrays)
import Kitty.Prim.Patterns
  ( pattern BoolType,
    pattern DoubleType,
    pattern FloatType,
    pattern Int16Type,
    pattern Int32Type,
    pattern Int64Type,
    pattern Int8Type,
    pattern Word16Type,
    pattern Word32Type,
    pattern Word64Type,
    pattern Word8Type,
  )
import qualified Text.Casing as Casing (screamingSnake)

-- | @'allPrimTypes' = enumerated@
--
-- 'allPrimTypes' contains the primitive types in the order they occur in 'Prim' and
-- 'Kitty.Prim.Base.Arrays'.
allPrimTypes :: [PrimType]
allPrimTypes = genum

-- |
-- >>> floatingPrimTypes == [ FloatType, DoubleType ]
-- True
floatingPrimTypes :: [PrimType]
floatingPrimTypes = [FloatType, DoubleType]

-- |
-- >>> import qualified Data.List as List ( delete )
-- >>> integralPrimTypes == foldr delete allPrimTypes [ BoolType, FloatType, DoubleType ]
-- True
integralPrimTypes :: [PrimType]
integralPrimTypes =
  [ Int8Type,
    Int16Type,
    Int32Type,
    Int64Type,
    Word8Type,
    Word16Type,
    Word32Type,
    Word64Type
  ]

-- |
-- >>> signedIntPrimTypes == [ Int8Type, Int16Type, Int32Type, Int64Type ]
-- True
signedIntPrimTypes :: [PrimType]
signedIntPrimTypes = [Int8Type, Int16Type, Int32Type, Int64Type]

-- |
-- >>> unsignedIntPrimTypes = [ Word8Type, Word16Type, Word32Type, Word64Type ]
-- True
unsignedIntPrimTypes :: [PrimType]
unsignedIntPrimTypes = [Word8Type, Word16Type, Word32Type, Word64Type]

-- |
-- >>> import qualified Data.List as List ( delete )
-- >>> numericPrimTypes = delete BoolType allPrimTypes
-- True
numericPrimTypes :: [PrimType]
numericPrimTypes =
  [ Int8Type,
    Int16Type,
    Int32Type,
    Int64Type,
    Word8Type,
    Word16Type,
    Word32Type,
    Word64Type,
    FloatType,
    DoubleType
  ]

-- | Get the size of @a@ in bytes.  Amenable to use with @-XTypeApplications@, e.g.
--
-- > unTagged $ primSizeBytes @Bool
primSizeBytes :: forall a. (Nat.SNatI (PrimSizeBytes a)) => Tagged a Int
primSizeBytes = Tagged $ Nat.reflectToNum (Proxy @(PrimSizeBytes a))
{-# INLINEABLE primSizeBytes #-}

-- | Get the name of the equivalent type to @a@ in the C language (e.g. @uint8_t@ for
-- 'Data.Word.Word8').  Amenable to use with @-XTypeApplications@, e.g.
--
-- > unTagged $ primCName @Bool
primCName :: forall a. KnownSymbol (PrimCName a) => Tagged a Text.Text
primCName = Tagged . Text.pack $ symbolVal (Proxy @(PrimCName a))
{-# INLINEABLE primCName #-}

-- | Output the 'PrimType' corresponding to @a@.
primTypeProxy :: forall a. IsPrimitive a => Proxy a -> PrimType
primTypeProxy _ = unTagged $ primType @a
{-# INLINEABLE primTypeProxy #-}

-- | Output the 'PrimType' corresponding to @a@.
primTypeOf :: forall a. IsPrimitive a => PrimType
primTypeOf = unTagged $ primType @a
{-# INLINEABLE primTypeOf #-}

-- | Output the 'PrimType' corresponding to @a@.
primTypeOf' :: forall proxy a. IsPrimitive a => proxy a -> PrimType
primTypeOf' _ = unTagged $ primType @a
{-# INLINEABLE primTypeOf' #-}

-- | Wrap the input in the appropriate 'PrimVal' constructor corresponding to @a@.
toPrimVal :: forall a. IsPrimitive a => a -> PrimVal
toPrimVal = toPrim . Identity
{-# INLINEABLE toPrimVal #-}

-- | An array of the C names of all primitive types without the @_t@ suffix where applicable.  This
-- seems to be required for various parts of code generation and only exists for backwards
-- compatibility.
arrayNames :: Arrays ArrayName
arrayNames = Barbies.bpureC @IsPrimitive name
  where
    name :: forall a. IsPrimitive a => ArrayName a
    name = ArrayName . removeT . unTagged $ primCName @a
    removeT a = fromMaybe a $ Text.stripSuffix "_t" a
{-# INLINEABLE arrayNames #-}

-- | An array of the C names of all primitive types.
arrayCNames :: Arrays ArrayName
arrayCNames = Barbies.bpureC @IsPrimitive name
  where
    name :: forall a. IsPrimitive a => ArrayName a
    name = ArrayName . unTagged $ primCName @a

arrayPrimTypes :: Arrays (Const PrimType)
arrayPrimTypes = Barbies.bpureC @IsPrimitive summonPrimType
  where
    summonPrimType :: forall a. IsPrimitive a => Const PrimType a
    summonPrimType = Const $ toPrim (Proxy @a)
{-# INLINEABLE arrayPrimTypes #-}

extremumDefines :: PrimType -> (Text.Text, Text.Text)
extremumDefines BoolType = ("false", "true")
extremumDefines DoubleType = ("DBL_MIN", "DBL_MAX")
extremumDefines FloatType = ("FLT_MIN", "FLT_MAX")
extremumDefines intType = bimap prependName prependName ("MIN", "MAX")
  where
    prependName x = polyIndexArrays @IsPrimitive convert arrayNames intType <> "_" <> x
    convert _proxy (ArrayName name) =
      over Text.unpacked Casing.screamingSnake name

-- | This set of 'Arrays' contains the names of the maximum-bound values of each C type,
-- e.g. @true@, @DBL_MAX@, @UINT8_MAX@ and so on.
arrayCMaximumDefines :: Arrays ArrayName
arrayCMaximumDefines =
  Barbies.bmap (ArrayName . snd . extremumDefines . getConst) arrayPrimTypes
{-# INLINEABLE arrayCMaximumDefines #-}

-- | This set of 'Arrays' contains the names of the minimum-bound values of each C type,
-- e.g. @false@, @DBL_MIN@, @UINT8_MIN@ and so on.
arrayCMinimumDefines :: Arrays ArrayName
arrayCMinimumDefines =
  Barbies.bmap (ArrayName . fst . extremumDefines . getConst) arrayPrimTypes
{-# INLINEABLE arrayCMinimumDefines #-}

-- | Get the C name corresponding to the primitive type present.  This will give the same answer for
-- any @f@; any primitive value is not used.
--
-- This can be called at the signature @'getPrimCName' :: 'PrimType' -> 'Text.Text'@.
getPrimCName :: Prim f -> Text.Text
getPrimCName = polyIndexArrays @IsPrimitive (const getArrayName) arrayCNames

-- | This is the same as 'getPrimCName', except that things like @int8_t@ will lose their trailing
-- @_t@.
getPrimCNameNoT :: Prim f -> Text.Text
getPrimCNameNoT = polyIndexArrays @IsPrimitive (const getArrayName) arrayNames

-- | This 'Arrays' has a 'TypeRep' for the corresponding primitive type in each position.
arrayTypeReps :: Arrays (Const TypeRep)
arrayTypeReps = Barbies.bpureC @IsPrimitive rep
  where
    rep :: forall a. IsPrimitive a => Const TypeRep a
    rep = Const $ typeRep (Proxy @a)

-- | This 'Arrays' has a 'PrimType' for the corresponding primitive type in each position.
arrayPrims :: Arrays (Const PrimType)
arrayPrims = Barbies.bpureC @IsPrimitive constPrim
  where
    constPrim :: forall a. IsPrimitive a => Const (Prim Proxy) a
    constPrim = Const $ toPrim (Proxy @a)

-- | Get the 'TypeRep' for the primitive type represented by the input.  This will give the same
-- answer for any @f@; any primitive value is not used.
--
-- This can be called at the signature @'getIsPrimitiveRep' :: 'PrimType' -> 'TypeRep'@.
getIsPrimitiveRep :: forall f. Prim f -> TypeRep
getIsPrimitiveRep = polyIndexArrays @IsPrimitive (const getConst) arrayTypeReps

-- | Get the Haskell name for the type indicated by the primitive type present.  This will give the
-- same answer for any @f@; any primitive value is not used.
--
-- This can be called at the signature @'getPrimHaskellName' :: 'PrimType' -> 'Text.Text'@.
getPrimHaskellName :: Prim f -> Text.Text
getPrimHaskellName = Text.pack . show . getIsPrimitiveRep
