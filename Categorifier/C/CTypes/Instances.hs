{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Categorifier.C.CTypes.Instances
  ( ToCxxType (),
    PrimitivesToCxxType,
  )
where

import Accessors.Dynamic (DConstructor, DData, DField, DSimpleEnum)
import qualified Barbies
import Categorifier.C.CTypes.KTypeRep (toKTypeRep)
import Categorifier.C.CTypes.Render (infiniteTupleFieldNames, renderCType)
import Categorifier.C.CTypes.ToCxxType
  ( CoercibleT,
    ToCxxType (..),
    ToCxxTypeError (..),
    toRecordNamedCxxType,
  )
import Categorifier.C.CTypes.Types
  ( CCon,
    CConF (..),
    CNat (..),
    CStructF (..),
    CType,
    CTypeF (..),
    CxxPrim (..),
    CxxType (..),
    DcName (..),
    IsHaskellList (..),
    IsTuple (..),
    Prim (..),
    SupportsKBits,
    discard,
    oneLevelCxxArrayToMaybeCArray,
  )
import Categorifier.C.Recursion (hembed)
import qualified Categorifier.Common.IO.Exception as Exception
import Control.Lens ((^.), _1, _2, _3, _4, _5, _6)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.Functor (void)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Data.Functor.Sum (Sum)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Tree (Tree)
import Data.Type.Nat (Nat)
import qualified Data.Type.Nat as Nat
import Data.Typeable (Typeable, typeRep)
import Data.Vec.Lazy (Vec)
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word64, Word8)
import PyF (fmt)

----------------  All the C primitives ------------------
instance SupportsKBits f => ToCxxType f (f Word8) where
  toCxxType = CxxTypeCType . CTypePrim . PrimWord8 . Compose
  {-# INLINEABLE toCxxType #-}

instance SupportsKBits f => ToCxxType f (f Word16) where
  toCxxType = CxxTypeCType . CTypePrim . PrimWord16 . Compose
  {-# INLINEABLE toCxxType #-}

instance SupportsKBits f => ToCxxType f (f Word32) where
  toCxxType = CxxTypeCType . CTypePrim . PrimWord32 . Compose
  {-# INLINEABLE toCxxType #-}

instance SupportsKBits f => ToCxxType f (f Word64) where
  toCxxType = CxxTypeCType . CTypePrim . PrimWord64 . Compose
  {-# INLINEABLE toCxxType #-}

instance SupportsKBits f => ToCxxType f (f Int8) where
  toCxxType = CxxTypeCType . CTypePrim . PrimInt8 . Compose
  {-# INLINEABLE toCxxType #-}

instance SupportsKBits f => ToCxxType f (f Int16) where
  toCxxType = CxxTypeCType . CTypePrim . PrimInt16 . Compose
  {-# INLINEABLE toCxxType #-}

instance SupportsKBits f => ToCxxType f (f Int32) where
  toCxxType = CxxTypeCType . CTypePrim . PrimInt32 . Compose
  {-# INLINEABLE toCxxType #-}

instance SupportsKBits f => ToCxxType f (f Int64) where
  toCxxType = CxxTypeCType . CTypePrim . PrimInt64 . Compose
  {-# INLINEABLE toCxxType #-}

instance SupportsKBits f => ToCxxType f (f Double) where
  toCxxType = CxxTypeCType . CTypePrim . PrimDouble . Compose
  {-# INLINEABLE toCxxType #-}

instance SupportsKBits f => ToCxxType f (f Float) where
  toCxxType = CxxTypeCType . CTypePrim . PrimFloat . Compose
  {-# INLINEABLE toCxxType #-}

instance SupportsKBits f => ToCxxType f (f Bool) where
  toCxxType = CxxTypeCType . CTypePrim . PrimBool . Compose
  {-# INLINEABLE toCxxType #-}

-------------------- The C++ primitives ------------------
instance SupportsKBits f => ToCxxType f (f BS.ByteString) where
  toCxxType = CxxTypePrim . PrimByteString . Compose
  {-# INLINEABLE toCxxType #-}

instance SupportsKBits f => ToCxxType f (f T.Text) where
  toCxxType = CxxTypePrim . PrimString . Compose
  {-# INLINEABLE toCxxType #-}

------------------ primitives defined in terms of other primitives ---------------
-- Define 'Int' in terms of 'Int64'.
instance (Functor f, SupportsKBits f) => ToCxxType f (f Int) where
  toCxxType = toCxxType . fmap (fmap (fromIntegral :: Int -> Int64))
  {-# INLINEABLE toCxxType #-}

-- Define the 'Char' instance in terms of 'Int8'.
instance (Applicative f, SupportsKBits f) => ToCxxType f (f Char) where
  toCxxType = toCxxType . fmap (fmap (toEnum . fromEnum :: Char -> Int8))
  {-# INLINEABLE toCxxType #-}

-- There 'String' one overlaps because it's a list of characters but this seems to be OK.
-- If it does come up, consider changing the @ToCxxType [a]@ instance to require 'Typeable'
-- and detect thee whether it's a 'String' or other list.
instance {-# OVERLAPS #-} (Functor f, SupportsKBits f) => ToCxxType f (f String) where
  toCxxType = toCxxType . fmap (fmap T.pack)
  {-# INLINEABLE toCxxType #-}

-- Lazy versions.
instance (Functor f, SupportsKBits f) => ToCxxType f (f TL.Text) where
  toCxxType = toCxxType . fmap (fmap TL.toStrict)
  {-# INLINEABLE toCxxType #-}

instance (Functor f, SupportsKBits f) => ToCxxType f (f BSL.ByteString) where
  toCxxType = toCxxType . fmap (fmap BSL.toStrict)
  {-# INLINEABLE toCxxType #-}

--------------------- the container types --------------------
instance (ToCxxType f k, ToCxxType f v) => ToCxxType f (M.Map k v) where
  toCxxType =
    CxxTypeMap
      (discard (toCxxType (Proxy @k) :: CxxType (Compose Proxy f)))
      (discard (toCxxType (Proxy @v) :: CxxType (Compose Proxy f)))
      . toCxxTypes
    where
      toCxxTypes = fmap keyValuePairToCxxType . traverse M.toList
        where
          keyValuePairToCxxType ::
            (Applicative g, Traversable g, CoercibleT g) =>
            g (k, v) ->
            (CxxType (Compose g f), CxxType (Compose g f))
          keyValuePairToCxxType kv = (toCxxType (fmap fst kv), toCxxType (fmap snd kv))
  {-# INLINEABLE toCxxType #-}

instance ToCxxType f a => ToCxxType f (V.Vector a) where
  toCxxType = CxxTypeVector (IsHaskellList False) proxyType . toCxxTypes
    where
      proxyType = discard (toCxxType (Proxy @a) :: CxxType (Compose Proxy f))
      toCxxTypes = fmap toCxxType . V.toList . sequenceA
  {-# INLINEABLE toCxxType #-}

instance ToCxxType f a => ToCxxType f [a] where
  -- Lists represented as C++ vectors.
  toCxxType = CxxTypeVector (IsHaskellList True) proxyType . toCxxTypes
    where
      proxyType = discard (toCxxType (Proxy @a) :: CxxType (Compose Proxy f))
      toCxxTypes = fmap toCxxType . sequenceA
  {-# INLINEABLE toCxxType #-}

instance (ToCxxType f a, Nat.SNatI k) => ToCxxType f (Vec (k :: Nat) a) where
  toCxxType =
    either (Exception.impureThrow . CxxTypeNotCType) id
      . oneLevelCxxArrayToMaybeCArray
      . CxxTypeArray natInt aType
      . fmap toCxxType
      . toList
      . sequenceA
    where
      natInt = CNatInt (Nat.reflectToNum (Proxy @k))
      aType :: CxxType Proxy
      aType = discard (toCxxType (Proxy @a) :: CxxType (Compose Proxy f))
  {-# INLINEABLE toCxxType #-}

-- Unit.
instance SupportsKBits f => ToCxxType f ()

------------------------- Tuples -------------------------
-- Haskell Tuples are handles as C structs ('CStruct') if all fields of the tuple
-- are C types ('CxxTypeCType').
instance (ToCxxType f a0, ToCxxType f a1, Typeable a0, Typeable a1) => ToCxxType f (a0, a1) where
  toCxxType xs = case (toCxxType (fmap (^. _1) xs), toCxxType (fmap (^. _2) xs)) of
    (CxxTypeCType x0, CxxTypeCType x1) ->
      CxxTypeCType $
        CTypeStruct $
          CStruct
            { csTypeRep = toKTypeRep (typeRep (Proxy @(a0, a1))),
              csCon = makeTupleNormalCon [x0, x1]
            }
    (x0, x1) -> CxxTypeTuple [x0, x1]
  {-# INLINEABLE toCxxType #-}

instance
  ( ToCxxType f a0,
    ToCxxType f a1,
    ToCxxType f a2,
    Typeable a0,
    Typeable a1,
    Typeable a2
  ) =>
  ToCxxType f (a0, a1, a2)
  where
  toCxxType x =
    case (toCxxType (fmap (^. _1) x), toCxxType (fmap (^. _2) x), toCxxType (fmap (^. _3) x)) of
      (CxxTypeCType x0, CxxTypeCType x1, CxxTypeCType x2) ->
        CxxTypeCType $
          CTypeStruct $
            CStruct
              { csTypeRep = toKTypeRep (typeRep (Proxy @(a0, a1, a2))),
                csCon = makeTupleNormalCon [x0, x1, x2]
              }
      (x0, x1, x2) -> CxxTypeTuple [x0, x1, x2]
  {-# INLINEABLE toCxxType #-}

instance
  ( ToCxxType f a0,
    ToCxxType f a1,
    ToCxxType f a2,
    ToCxxType f a3,
    Typeable a0,
    Typeable a1,
    Typeable a2,
    Typeable a3
  ) =>
  ToCxxType f (a0, a1, a2, a3)
  where
  toCxxType x = case ( toCxxType (fmap (^. _1) x),
                       toCxxType (fmap (^. _2) x),
                       toCxxType (fmap (^. _3) x),
                       toCxxType (fmap (^. _4) x)
                     ) of
    (CxxTypeCType x0, CxxTypeCType x1, CxxTypeCType x2, CxxTypeCType x3) ->
      CxxTypeCType $
        CTypeStruct $
          CStruct
            { csTypeRep = toKTypeRep (typeRep (Proxy @(a0, a1, a2, a3))),
              csCon = makeTupleNormalCon [x0, x1, x2, x3]
            }
    (x0, x1, x2, x3) -> CxxTypeTuple [x0, x1, x2, x3]
  {-# INLINEABLE toCxxType #-}

instance
  ( ToCxxType f a0,
    ToCxxType f a1,
    ToCxxType f a2,
    ToCxxType f a3,
    ToCxxType f a4,
    Typeable a0,
    Typeable a1,
    Typeable a2,
    Typeable a3,
    Typeable a4
  ) =>
  ToCxxType f (a0, a1, a2, a3, a4)
  where
  toCxxType x = case ( toCxxType (fmap (^. _1) x),
                       toCxxType (fmap (^. _2) x),
                       toCxxType (fmap (^. _3) x),
                       toCxxType (fmap (^. _4) x),
                       toCxxType (fmap (^. _5) x)
                     ) of
    (CxxTypeCType x0, CxxTypeCType x1, CxxTypeCType x2, CxxTypeCType x3, CxxTypeCType x4) ->
      CxxTypeCType $
        CTypeStruct $
          CStruct
            { csTypeRep = toKTypeRep (typeRep (Proxy @(a0, a1, a2, a3, a4))),
              csCon = makeTupleNormalCon [x0, x1, x2, x3, x4]
            }
    (x0, x1, x2, x3, x4) -> CxxTypeTuple [x0, x1, x2, x3, x4]
  {-# INLINEABLE toCxxType #-}

instance
  ( ToCxxType f a0,
    ToCxxType f a1,
    ToCxxType f a2,
    ToCxxType f a3,
    ToCxxType f a4,
    ToCxxType f a5,
    Typeable a0,
    Typeable a1,
    Typeable a2,
    Typeable a3,
    Typeable a4,
    Typeable a5
  ) =>
  ToCxxType f (a0, a1, a2, a3, a4, a5)
  where
  toCxxType x = case ( toCxxType (fmap (^. _1) x),
                       toCxxType (fmap (^. _2) x),
                       toCxxType (fmap (^. _3) x),
                       toCxxType (fmap (^. _4) x),
                       toCxxType (fmap (^. _5) x),
                       toCxxType (fmap (^. _6) x)
                     ) of
    ( CxxTypeCType x0,
      CxxTypeCType x1,
      CxxTypeCType x2,
      CxxTypeCType x3,
      CxxTypeCType x4,
      CxxTypeCType x5
      ) ->
        CxxTypeCType $
          CTypeStruct $
            CStruct
              { csTypeRep = toKTypeRep (typeRep (Proxy @(a0, a1, a2, a3, a4, a5))),
                csCon = makeTupleNormalCon [x0, x1, x2, x3, x4, x5]
              }
    (x0, x1, x2, x3, x4, x5) -> CxxTypeTuple [x0, x1, x2, x3, x4, x5]
  {-# INLINEABLE toCxxType #-}

makeTupleNormalCon :: [CType f] -> CCon f
makeTupleNormalCon elements = case NE.nonEmpty (zip infiniteTupleFieldNames elements) of
  Nothing -> error [fmt|makeTupleNormalCon got no elemements for tuple|]
  Just neNamedElements ->
    CNormalCon
      (DcName dcname)
      YesIAmATuple
      $ fmap hembed <$> neNamedElements
  where
    dcname =
      T.intercalate "_" $
        "Tuple{length elements}" : fmap renderCType elements

------------------------- Higher-kinded types -------------------------

instance SupportsKBits f => ToCxxType f (Barbies.Unit f) where
  toCxxType = toCxxType . void
  {-# INLINEABLE toCxxType #-}

-- | Notice that @f@ and @unusedF@ are different.
-- This is because they are never applied!
instance (SupportsKBits f, ToCxxType f a) => ToCxxType f (Const a (unusedF :: Type -> Type)) where
  toCxxType = toCxxType . fmap getConst
  {-# INLINEABLE toCxxType #-}

-------------------------- Third-party types ----------------------

instance ToCxxType f (g (h a)) => ToCxxType f (Compose g h a) where
  toCxxType = toCxxType . fmap getCompose
  {-# INLINEABLE toCxxType #-}

instance ToCxxType f a => ToCxxType f (Identity a) where
  toCxxType = toCxxType . fmap runIdentity
  {-# INLINEABLE toCxxType #-}

instance ToCxxType f a => ToCxxType f (Const a b) where
  toCxxType = toCxxType . fmap getConst
  {-# INLINEABLE toCxxType #-}

instance
  ( ToCxxType f (g a),
    ToCxxType f (h a),
    Typeable g,
    Typeable h,
    Typeable a
  ) =>
  ToCxxType f (Sum g h a)

instance
  ( ToCxxType f (g a),
    ToCxxType f (h a),
    Typeable g,
    Typeable h,
    Typeable a
  ) =>
  ToCxxType f (Product g h a)
  where
  toCxxType = either Exception.impureThrow id . toRecordNamedCxxType ("first" :| ["second"])
  {-# INLINEABLE toCxxType #-}

instance ToCxxType f (a (b f)) => ToCxxType f (Compose a b f) where
  toCxxType = toCxxType . fmap getCompose
  {-# INLINEABLE toCxxType #-}

instance (Typeable a, ToCxxType f a) => ToCxxType f (Maybe a)

instance (Typeable a, Typeable b, ToCxxType f a, ToCxxType f b) => ToCxxType f (Either a b)

instance (Typeable a, ToCxxType f a) => ToCxxType f (Tree a)

------------------------ Non-higher-kinded types ----------------------
-- These exist so that we can have 'ToCxxType' instances for types
-- that aren't higher-kinded, such as generic-accessors types that are
-- part of the netgraph and therefore (currently) need instances,
-- but also for 'C' versions of higher-kinded types.
-- To avoid code duplication, these should all *only* be defined in terms
-- of the higher-kinded versions so the instances must always be of the form:
--
-- >  toCxxType = toCxxType . fmap (pure :: a -> f a)

instance {-# OVERLAPS #-} (Applicative f, SupportsKBits f) => ToCxxType f String where
  toCxxType = toCxxType . fmap (pure :: String -> f String)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f Bool where
  toCxxType = toCxxType . fmap (pure :: Bool -> f Bool)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f Double where
  toCxxType = toCxxType . fmap (pure :: Double -> f Double)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f Float where
  toCxxType = toCxxType . fmap (pure :: Float -> f Float)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f Int8 where
  toCxxType = toCxxType . fmap (pure :: Int8 -> f Int8)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f Int16 where
  toCxxType = toCxxType . fmap (pure :: Int16 -> f Int16)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f Int32 where
  toCxxType = toCxxType . fmap (pure :: Int32 -> f Int32)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f Int64 where
  toCxxType = toCxxType . fmap (pure :: Int64 -> f Int64)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f Word8 where
  toCxxType = toCxxType . fmap (pure :: Word8 -> f Word8)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f Word16 where
  toCxxType = toCxxType . fmap (pure :: Word16 -> f Word16)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f Word32 where
  toCxxType = toCxxType . fmap (pure :: Word32 -> f Word32)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f Word64 where
  toCxxType = toCxxType . fmap (pure :: Word64 -> f Word64)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f Char where
  toCxxType = toCxxType . fmap (pure :: Char -> f Char)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f Int where
  toCxxType = toCxxType . fmap (pure :: Int -> f Int)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f TL.Text where
  toCxxType = toCxxType . fmap (pure :: TL.Text -> f TL.Text)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f T.Text where
  toCxxType = toCxxType . fmap (pure :: T.Text -> f T.Text)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f BSL.ByteString where
  toCxxType = toCxxType . fmap (pure :: BSL.ByteString -> f BSL.ByteString)
  {-# INLINEABLE toCxxType #-}

instance (Applicative f, SupportsKBits f) => ToCxxType f BS.ByteString where
  toCxxType = toCxxType . fmap (pure :: BS.ByteString -> f BS.ByteString)
  {-# INLINEABLE toCxxType #-}

-- These are third-party types that are not higher-kinded.
instance (Applicative f, SupportsKBits f) => ToCxxType f DField

instance (Applicative f, SupportsKBits f) => ToCxxType f DData

instance (Applicative f, SupportsKBits f) => ToCxxType f DConstructor

instance (Applicative f, SupportsKBits f) => ToCxxType f DSimpleEnum

class
  ( ToCxxType f (f Int8),
    ToCxxType f (f Int16),
    ToCxxType f (f Int32),
    ToCxxType f (f Int64),
    ToCxxType f (f Word8),
    ToCxxType f (f Word16),
    ToCxxType f (f Word32),
    ToCxxType f (f Word64),
    ToCxxType f (f Float),
    ToCxxType f (f Double),
    ToCxxType f (f Bool),
    ToCxxType f (f BS.ByteString),
    ToCxxType f (f T.Text)
    -- forall a . IsPrimitive a => ToCxxType f (f a)
  ) =>
  PrimitivesToCxxType f
