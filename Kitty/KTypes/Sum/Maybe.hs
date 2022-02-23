{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | A lifted 'Maybe' type.
module Kitty.KTypes.Sum.Maybe
  ( KMaybe,
    withKMaybe,
    toKMaybe,
    kJust,
    kNothing,
    unsafeKMaybe,
  )
where

import Accessors (Lookup (..))
import Codec.Serialise (Serialise)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Kitty.CExpr.Cat.TargetOb (TargetOb, TargetObTC1)
import Kitty.CTypes.CGeneric.Class (CGeneric)
import Kitty.CTypes.GArrays (GArrays (..))
import Kitty.CTypes.ToCxxType (ToCxxType (..))
import Kitty.CTypes.Types (SupportsKBits)
import Kitty.KTypes.C (C (..))
import Kitty.KTypes.Conditional (KSelect)
import Kitty.KTypes.KLiteral (KLiteralPrimitives)
import Kitty.KTypes.SwitchCase (unsafeIndex)
import Kitty.Plugin.Client (deriveHasRep)
import Kitty.PolyVec (PolyVec, zeroValue)

-- | Lifted version of `Maybe`.
data KMaybe f a = UnsafeKMaybe {maybeSelector :: f Word8, just :: a} deriving (Generic)

kJust :: Num (f Word8) => a -> KMaybe f a
kJust = UnsafeKMaybe 1

kNothing :: forall f a. (Num (f Word8), KLiteralPrimitives f) => PolyVec f a => KMaybe f a
kNothing = UnsafeKMaybe 0 . zeroValue $ Proxy @f

withKMaybe :: (PolyVec f r, KSelect f) => KMaybe f a -> (Maybe a -> r) -> r
withKMaybe (UnsafeKMaybe s j) f = unsafeIndex [f Nothing, f $ Just j] s

toKMaybe :: (PolyVec f a, Num (f Word8), KLiteralPrimitives f) => Maybe a -> KMaybe f a
toKMaybe = maybe kNothing kJust

unsafeKMaybe :: KMaybe C a -> Maybe a
unsafeKMaybe (UnsafeKMaybe i v) = case i of
  0 -> Nothing
  1 -> Just v
  _ -> error "Impossible! KMaybe should only ever be 0 or 1"

instance
  ( ToCxxType f a,
    Typeable f,
    Typeable a,
    SupportsKBits f
  ) =>
  ToCxxType f (KMaybe f a)

instance CGeneric (KMaybe f a)

instance GArrays f a => GArrays f (KMaybe f a)

instance Lookup a => Lookup (KMaybe C a)

instance Serialise a => Serialise (KMaybe C a)

type instance TargetOb (KMaybe f a) = KMaybe (TargetObTC1 f) (TargetOb a)

deriveHasRep ''KMaybe
