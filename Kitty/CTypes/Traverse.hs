{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Generic traversals over 'CType'.
-- You can implement other types of traversals on top of these for example using Writer and Monoid.
-- Peddie says the standard way to get `foldMap` from `traverse` is to do
-- `getConst . traverse (Const . f)`. Perhaps in the future we will export those other traversals
-- from this module.
module Kitty.CTypes.Traverse
  ( CTraversalFuns (..),
    traverseCType,
    traverseCCon,
    CTraversalFuns_ (..),
    traverseCType_,
    traverseCCon_,
    PrimChangedType (..),
  )
where

import Data.Functor (($>))
import Data.Word (Word8)
import Kitty.CTypes.Types
  ( CBitfield (..),
    CBitfieldPrim (..),
    CCon,
    CConF (..),
    CEnum (..),
    CStruct,
    CStructF (..),
    CType,
    CTypeF (..),
    CUnion,
    CUnionF (..),
    RfName,
    bfprimToPrim,
  )
import Kitty.Common.IO.Exception (Exception, impureThrow)
import Kitty.Prim (Prim (..))
import Kitty.Recursion (BFix, hembed, hproject)

-- | Functions for what to do on primitives and unions.
data CTraversalFuns m f g = CTraversalFuns
  { ctfHandlePrim :: Prim f -> m (Prim g),
    ctfHandleEnum :: CEnum f -> m (g Word8),
    -- | returns the new tag and constructor
    ctfHandleUnionCon :: CUnion f -> m (g Word8, CCon g)
  }

data PrimChangedType = PrimChangedType

-- | __TODO__: This should go away, see https://kitty-hawk.atlassian.net/browse/SW-2658
instance Show PrimChangedType where
  show PrimChangedType = "when traversing a bitfield, prim changed type"

-- | __TODO__: This should go away, see https://kitty-hawk.atlassian.net/browse/SW-2658
instance Exception PrimChangedType

{-# INLINEABLE traverseCEnum #-}
traverseCEnum :: Functor m => CTraversalFuns m f g -> CEnum f -> m (CEnum g)
traverseCEnum (CTraversalFuns _ f _) cenum@(CEnum rep cons _) = CEnum rep cons <$> f cenum

{-# INLINEABLE traverseCStruct #-}
traverseCStruct :: Applicative m => CTraversalFuns m f g -> CStruct f -> m (CStruct g)
traverseCStruct f (CStruct rep con) = CStruct rep <$> traverseCCon f con

{-# INLINEABLE traverseCCon #-}
traverseCCon :: forall m f g. Applicative m => CTraversalFuns m f g -> CCon f -> m (CCon g)
traverseCCon CTraversalFuns {} (CNullaryCon dcName) = pure (CNullaryCon dcName)
traverseCCon (CTraversalFuns f _ _) (CBitfieldCon (CBitfield dcName fields value)) =
  CBitfieldCon . CBitfield dcName fields . unsafePrimToBfPrim <$> f (bfprimToPrim value)
  where
    unsafePrimToBfPrim :: Prim g -> CBitfieldPrim g
    unsafePrimToBfPrim (PrimWord8 x) = BfpUInt8 x
    unsafePrimToBfPrim (PrimWord16 x) = BfpUInt16 x
    unsafePrimToBfPrim (PrimWord32 x) = BfpUInt32 x
    unsafePrimToBfPrim (PrimWord64 x) = BfpUInt64 x
    unsafePrimToBfPrim _ = impureThrow PrimChangedType
traverseCCon f (CNormalCon dcName tup fields) = CNormalCon dcName tup <$> traverse g fields
  where
    g :: (RfName, BFix CTypeF f) -> m (RfName, BFix CTypeF g)
    g (name, x) = (name,) . hembed <$> traverseCType f (hproject x)

{-# INLINEABLE traverseCUnion #-}
traverseCUnion :: forall m f g. Applicative m => CTraversalFuns m f g -> CUnion f -> m (CUnion g)
traverseCUnion (CTraversalFuns _primFun _enumFun unionFun) cunion@(CUnion rep cons _ _) =
  uncurry (CUnion rep cons) <$> unionFun cunion

{-# INLINEABLE traverseCType #-}
traverseCType :: Applicative m => CTraversalFuns m f g -> CType f -> m (CType g)
traverseCType f (CTypeEnum x) = CTypeEnum <$> traverseCEnum f x
traverseCType f (CTypeStruct x) = CTypeStruct <$> traverseCStruct f x
traverseCType f (CTypeUnion x) = CTypeUnion <$> traverseCUnion f x
traverseCType f (CTypeArray nat elemType xs) =
  CTypeArray nat elemType <$> traverse (fmap hembed . traverseCType f . hproject) xs
traverseCType (CTraversalFuns f _ _) (CTypePrim x) = CTypePrim <$> f x

-- | Variant of 'CTraversalFuns' which is only run for its effect.
data CTraversalFuns_ m f = CTraversalFuns_
  { ctfHandlePrim_ :: Prim f -> m (),
    ctfHandleEnum_ :: CEnum f -> m (),
    ctfHandleUnionCon_ :: CUnion f -> m ()
  }

{-# INLINEABLE fromTraversalFuns_ #-}
fromTraversalFuns_ :: Applicative m => CTraversalFuns_ m f -> CTraversalFuns m f f
fromTraversalFuns_ foldFuns =
  CTraversalFuns
    { ctfHandlePrim = \prim -> ctfHandlePrim_ foldFuns prim $> prim,
      ctfHandleEnum = \enum -> ctfHandleEnum_ foldFuns enum $> ceData enum,
      ctfHandleUnionCon = \unionCon ->
        ctfHandleUnionCon_ foldFuns unionCon $> (cuTag unionCon, cuCon unionCon)
    }

-- | Variant of 'traverseCType_' which is only run for its effect.
{-# INLINEABLE traverseCType_ #-}
traverseCType_ :: Applicative m => CTraversalFuns_ m f -> CType f -> m ()
traverseCType_ traversalFuns_ ctype = traverseCType (fromTraversalFuns_ traversalFuns_) ctype $> ()

-- | Variant of 'traverseCCon_' which is only run for its effect.
{-# INLINEABLE traverseCCon_ #-}
traverseCCon_ :: Applicative m => CTraversalFuns_ m f -> CCon f -> m ()
traverseCCon_ traversalFuns_ ccon = traverseCCon (fromTraversalFuns_ traversalFuns_) ccon $> ()
