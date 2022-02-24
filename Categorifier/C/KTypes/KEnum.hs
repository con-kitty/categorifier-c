{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Treat 'Word8's as 'Enum's.
module Categorifier.C.KTypes.KEnum
  ( KEnum (..),
    toKEnum,
  )
where

import Categorifier.C.CExpr.Cat.TargetOb (TargetOb, TargetObTC1)
import Categorifier.C.CTypes.CGeneric.Class (CGeneric)
import Categorifier.C.CTypes.GArrays (GArrays)
import Categorifier.C.CTypes.Render (renderCxxType)
import Categorifier.C.CTypes.ToCxxType (ToCxxType (..))
import Categorifier.C.CTypes.Types (CEnum (..), CTypeF (..), CxxType (..))
import Categorifier.C.KTypes.BooleanLogic (KAnd)
import Categorifier.C.KTypes.Equality (KEq (..))
import Categorifier.C.KTypes.KLiteral (KLiteral, kliteral)
import Categorifier.C.KTypes.TotalOrder (KOrd (..))
import Categorifier.Client (deriveHasRep)
import Data.Functor.Compose (Compose (..))
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import PyF (fmt)

-- | newtype wrapper around Word8 with functions for treating it as its phantom type
-- TODO(greg): if we define this as:
--   newtype KEnum a f = KEnum (f Word8)
-- then do all our troubles go away?
newtype KEnum f a = KEnum (f Word8) deriving (Generic)

-- | Convert a 'KEnum' to a 'CEnum' by first creating a @'CEnum' 'Proxy'@ to get the type right, and
-- then converting the 'Proxy' to a 'Functor' container type by taking the 'KEnum's 'Word8' and
-- putting it into the @'CEnum' 'Proxy'@.
-- TODO(greg): define 'toKEnum and 'fromKEnum in terms of this function.
kenumToCEnum :: forall f g a. (ToCxxType f a, Functor g) => g (KEnum f a) -> CEnum (Compose g f)
kenumToCEnum =
  let enumCxxType :: CEnum (Compose Proxy f)
      enumCxxType = case toCxxType (Proxy @a) of
        CxxTypeCType (CTypeEnum cenum) -> cenum
        otherType -> error [fmt|KEnum got a non-enum type {renderCxxType otherType}|]
      getKEnumWord :: KEnum f a -> f Word8
      getKEnumWord (KEnum x) = x
      toEnumCxxType :: g (KEnum f a) -> CEnum (Compose g f)
      toEnumCxxType kenum = enumCxxType {ceData = Compose word8}
        where
          word8 :: g (f Word8)
          word8 = getKEnumWord <$> kenum
   in toEnumCxxType

instance CGeneric (KEnum f a)

instance GArrays f (KEnum f a)

instance
  ( ToCxxType f a
  ) =>
  ToCxxType f (KEnum f a)
  where
  toCxxType = CxxTypeCType . CTypeEnum . kenumToCEnum

deriveHasRep ''KEnum

type instance TargetOb (KEnum f a) = KEnum (TargetObTC1 f) a

instance (KAnd f, KEq f (f Word8)) => KEq f (KEnum f a) where
  KEnum x .== KEnum y = x .== y

  KEnum x ./= KEnum y = x ./= y

instance (KOrd f (f Word8)) => KOrd f (KEnum f a) where
  (.<) (KEnum x) (KEnum y) = (.<) x y

  (.<=) (KEnum x) (KEnum y) = (.<=) x y

  (.>) (KEnum x) (KEnum y) = (.>) x y

  (.>=) (KEnum x) (KEnum y) = (.>=) x y

  kMin (KEnum x) (KEnum y) = KEnum $ kMin x y

  kMax (KEnum x) (KEnum y) = KEnum $ kMax x y

-- | convert enum to 'KEnum'
toKEnum ::
  forall f a.
  (Bounded a, Enum a, Eq a, KLiteral f Word8) =>
  a ->
  KEnum f a
toKEnum x = either error (KEnum . kliteral) $ toIndex allEnums
  where
    allEnums :: [a]
    allEnums = enumFrom minBound
    maxK :: Word8
    maxK = maxBound
    toIndex :: [a] -> Either String Word8
    toIndex =
      maybe
        (Left "enum doesn't match anything in enumFrom minBound")
        ( \i ->
            if i >= fromIntegral maxK
              then Left $ "enum too big (" <> show maxK <> ")"
              else pure $ fromIntegral i
        )
        . elemIndex' x

    elemIndex' e = go (0 :: Int)
      where
        go idx (y : ys)
          | y == e = pure idx
          | otherwise = go (idx + 1) ys
        go _idx [] = Nothing
