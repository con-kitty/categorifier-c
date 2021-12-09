{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Treat 'Word8's as 'Enum's.
module Kitty.KTypes.KEnum
  ( KEnum (..),
  )
where

import Data.Functor.Compose (Compose (..))
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Kitty.CExpr.Cat.TargetOb (TargetOb, TargetObTC1)
import Kitty.CTypes.CGeneric.Class (CGeneric)
import Kitty.CTypes.GArrays (GArrays)
import Kitty.CTypes.Render (renderCxxType)
import Kitty.CTypes.ToCxxType (ToCxxType (..))
import Kitty.CTypes.Types (CEnum (..), CTypeF (..), CxxType (..))
import Kitty.KTypes.BooleanLogic (KAnd)
import Kitty.KTypes.Equality (KEq (..))
import Kitty.KTypes.TotalOrder (KOrd (..))
import qualified Kitty.Plugin.Client as Client
import PyF (fmt)

-- | Newtype wrapper around Word8 with functions for treating it as its phantom type.
--
--  __TODO__(greg): if we define this as @newtype `KEnum` a f = `KEnum` (f `Word8`)@ then do all our
--                  troubles go away?
newtype KEnum f a = KEnum (f Word8) deriving (Generic)

-- | Convert a 'KEnum' to a 'CEnum' by first creating a @'CEnum' 'Proxy'@ to get the type right, and
--   then converting the 'Proxy' to a 'Functor' container type by taking the 'KEnum's 'Word8' and
--   putting it into the @'CEnum' 'Proxy'@.
--
--  __TODO__(greg): define 'toKEnum and 'fromKEnum in terms of this function.
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

instance (Client.HasRep a, (f Word8, b) ~ Client.Rep a, Monoid b) => Client.HasRep (KEnum f a) where
  type Rep (KEnum f a) = Client.Rep a
  abst = KEnum . fst
  repr (KEnum w) = (w, mempty)

-- instance (Client.HasRep a, (C Word8, b) ~ Client.Rep a, Monoid b) => Client.HasRep (KEnum f a) where
--   type Rep (KEnum f a) = (f Word8, b)
--   abst = KEnum . fst
--   repr (KEnum w) = (w, mempty)

type instance TargetOb (KEnum f a) = KEnum (TargetObTC1 f) (TargetOb a)

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
