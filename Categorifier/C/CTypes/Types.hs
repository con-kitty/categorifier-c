{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | C and C++ model of Haskell types and data.
module Categorifier.C.CTypes.Types
  ( -- * C/C++ type model
    CxxType (..),
    CType,
    CTypeF (..),
    pattern CTypePrim',
    pattern CTypeEnum',
    CxxStruct (..),
    CStruct,
    CStructF (..),
    CxxUnion (..),
    CUnion,
    CUnionF (..),
    CxxCon (..),
    CCon,
    CConF (..),
    IsTuple (..),
    CxxOrCCon (..),
    CBitfield (..),
    CBitfieldPrim (..),
    CxxPrim (..),
    Prim (..),
    allCxxPrims,
    CEnum (..),
    CNat (..),
    cnatValue,
    DcName (..),
    IsHaskellList (..),
    CUnionCon,
    CUnionConF (..),
    CxxUnionCon (..),

    -- * Record field name - don't export the constructor, must be used sanitized
    RfName,
    makeRfName,
    sanitizedRfName,

    -- * Type tags
    Identity (..),

    -- * bitfields
    bfprimToPrim,
    toBitfieldPrim,
    fromBitfieldPrim,
    fromBitfieldPrim',

    -- * helpers
    cconNumFields,
    cxxConNumFields,
    conNumFields,
    cconName,
    cxxConName,
    cxxOrCConName,

    -- * Helpers
    SupportsKBits,
    oneLevelCxxArrayToMaybeCArray,
    discard,
    allEnumCons,
    enumDcName,
  )
where

import qualified Barbies
import Categorifier.C.CTypes.KTypeRep (KTypeRep)
import Categorifier.C.KTypes.KBits (KBits, setBitTo, testBit)
import Categorifier.C.KTypes.KLiteral (KLiteral, kliteral)
import Categorifier.C.Prim (IsPrimitive, Prim (..))
import Categorifier.C.Recursion (BFix, hembed)
import Control.DeepSeq (NFData, NFData1)
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import qualified GHC.Generics as Generics
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Generics.Deriving.Enum (GEnum (..))
import PyF (fmt)

-- | A general C type. The type parameter in practice will either be 'Proxy' or 'Identity'. 'Proxy'
-- is a type-level description of a type, 'Identity' also carries along data. The common
-- representation for types and data avoids code duplication.
--
-- This is written as the fixed-point of a higher-kinded functor @l@ in order to avoid direct
-- recursion, since `CTypeF` is used within the code-generation system and the CCC plugin doesn't
-- yet deal with mutually-recursive data types.
data CTypeF (l :: (Type -> Type) -> Type) (f :: Type -> Type)
  = CTypeEnum (CEnum f)
  | CTypeStruct (CStructF l f)
  | CTypeUnion (CUnionF l f)
  | -- | Array length, element type, and optional element values.
    CTypeArray CNat (l Proxy) [l f]
  | CTypePrim (Prim f)
  deriving (Generic)

-- | This pattern synonym can be used to avoid type ambiguity about the type parameter @l@ when
-- trying to build a primitive-valued `CType`.
pattern CTypePrim' :: Prim f -> CType f
pattern CTypePrim' p = CTypePrim p

-- | This pattern synonym can be used to avoid type ambiguity about the type parameter @l@ when
-- trying to build an enum-valued `CType`.
pattern CTypeEnum' :: CEnum f -> CType f
pattern CTypeEnum' e = CTypeEnum e

{-# COMPLETE CTypeEnum', CTypeStruct, CTypeUnion, CTypeArray, CTypePrim' #-}

-- | This type is slightly odd (it'd be more commonly defined as the isomorphic type
--   @type `CType` = `BFix` `CTypeF`@), but here has the outermost layer unwrapped (exposing the
--   data constructors) in order to minimize changes to other places in the code base.
--
--   In future (if defined at all), `CType` may be defined as @`BFix` CData `Proxy`@ (i.e., a /type/
--   vs @type CTerm = `BFix` CData `Identity`@, which would represent a /term/).
type CType = CTypeF (BFix CTypeF)

-- | If a CxxTypeVector is actually a list in haskell.
-- This information is needed because the CBOR serialise instances are different.
newtype IsHaskellList = IsHaskellList Bool deriving (Eq, Ord, Show, Generic)

instance NFData IsHaskellList

-- | A general C++ type. See documentation for 'CType' regarding element types and data.
data CxxType (f :: Type -> Type)
  = CxxTypeCType (CType f)
  | CxxTypeStruct (CxxStruct f)
  | CxxTypeUnion (CxxUnion f)
  | -- | Array length, element type, and optional element values.
    CxxTypeArray CNat (CxxType Proxy) [CxxType f]
  | CxxTypePrim (CxxPrim f)
  | CxxTypeTuple [CxxType f]
  | -- | List/vector tag, element type, and optional element values.
    -- Because the 'Traversable' instance for 'Proxy' calls 'pure', and the
    -- 'Applicative' instance of '[]' returns a list with one element,
    -- if you do @toCxxType (Proxy @[Foo])@, you will get a 'CxxTypeVector' whose
    -- last field is a list of one element which is the same as its second to last
    -- field. It is tempting to remove the second-to-last field completely, but
    -- this would cause us to have to pattern match on a list, trusting that it always
    -- has one element, which seems like it would be very annoying.
    -- TODO(greg/peddie/guillaume): how can we get rid of the @(CxxType Proxy)@ cleanly?
    CxxTypeVector IsHaskellList (CxxType Proxy) [CxxType f]
  | -- | Key type, value type, and optional list of (key, value) pairs.
    CxxTypeMap (CxxType Proxy) (CxxType Proxy) [(CxxType f, CxxType f)]
  deriving (Generic)

-- | One of the constructors from a union.
data CUnionConF l f = CUnionCon (CUnionF l f) (CConF l f) deriving (Generic)

type CUnionCon = CUnionConF (BFix CTypeF)

-- | One of the constructors from a union.
data CxxUnionCon f = CxxUnionCon (CxxUnion f) (CxxOrCCon f) deriving (Generic)

-- | Data constructor name.
newtype DcName = DcName {unDcName :: T.Text} deriving (Eq, Ord, Show, Generic)

instance NFData DcName

-- | Record field name. Can only be accessed with 'sanitizedRfName' which removes @'@s.
newtype RfName = UnsafeUnsanitizedRfName T.Text deriving (Eq, Ord, Show, Generic)

instance NFData RfName

-- So #foo can represents the rfName "foo"
instance KnownSymbol symbol => IsLabel symbol RfName where
  fromLabel = makeRfName (T.pack (symbolVal (Proxy @symbol)))

-- | Create an 'RfName' (doesn't change the 'T.Text').
makeRfName :: T.Text -> RfName
makeRfName = UnsafeUnsanitizedRfName
{-# INLINEABLE makeRfName #-}

-- | Get the text under the 'RfName'. Changes @'@ to @_@.
sanitizedRfName :: RfName -> T.Text
sanitizedRfName (UnsafeUnsanitizedRfName x) = T.replace "'" "_" x
{-# INLINEABLE sanitizedRfName #-}

-- | Enumeration type.
data CEnum (f :: Type -> Type) = CEnum
  { ceTypeRep :: KTypeRep,
    ceCons :: V.Vector DcName,
    ceData :: f Word8
  }
  deriving (Generic)

-- | Struct type.
data CStructF l f = CStruct
  { -- | Used for 'Eq' and 'Ord' instances.
    csTypeRep :: KTypeRep,
    csCon :: CConF l f
  }
  deriving (Generic)

type CStruct = CStructF (BFix CTypeF)

instance
  (NFData1 f, NFData (l f), forall p. IsPrimitive p => NFData (f p)) =>
  NFData (CStructF l f)

-- | C++ Struct type.
data CxxStruct f = CxxStruct
  { -- | Used for 'Eq' and 'Ord' instances.
    cxxsTypeRep :: KTypeRep,
    cxxsCon :: CxxCon f
  }
  deriving (Generic)

instance
  ( NFData1 f,
    NFData (f T.Text),
    NFData (f ByteString),
    forall p. IsPrimitive p => NFData (f p)
  ) =>
  NFData (CxxStruct f)

-- | Union type, representated as a tag and a union of constructors.
data CUnionF l f = CUnion
  { -- | Used for 'Eq' and 'Ord' instances.
    cuTypeRep :: KTypeRep,
    cuCons :: V.Vector (CConF l Proxy),
    cuTag :: f Word8,
    cuCon :: CConF l f
  }
  deriving (Generic)

type CUnion = CUnionF (BFix CTypeF)

instance
  (NFData1 f, NFData (l Proxy), NFData (l f), forall p. IsPrimitive p => NFData (f p)) =>
  NFData (CUnionF l f)

-- | C++ union type, representated as mapbox::variant. Not used for real time work
-- because it can be variable length.
data CxxUnion f = CxxUnion
  { -- | Used for 'Eq' and 'Ord' instances.
    cxxuTypeRep :: KTypeRep,
    cxxuCons :: V.Vector (CxxOrCCon Proxy),
    cxxuTag :: f Word8,
    cxxuCon :: CxxOrCCon f
  }
  deriving (Generic)

instance
  ( NFData1 f,
    NFData (f T.Text),
    NFData (f ByteString),
    forall p. IsPrimitive p => NFData (f p)
  ) =>
  NFData (CxxUnion f)

data IsTuple = NotTuple | YesIAmATuple deriving (Generic, Show, Eq, Ord)

instance NFData IsTuple

-- | C struct constructor. Can be the only one in a struct or one of many in a union.
data CConF l f
  = CNullaryCon DcName
  | CNormalCon DcName IsTuple (NE.NonEmpty (RfName, l f))
  | CBitfieldCon (CBitfield f)
  deriving (Generic)

type CCon = CConF (BFix CTypeF)

data CBitfield f = CBitfield DcName (NE.NonEmpty RfName) (CBitfieldPrim f) deriving (Generic)

-- | 'SupportsKBits' exists because when we call 'Categorifier.C.CTypes.ToCxxType.ToCxxType' on bitvectors,
-- it converts the bits to a list of 'Bool's. I'm not sure if this is the best way or not but it
-- works well enough for now. It makes all 'Categorifier.C.CTypes.ToCxxType.ToCxxType' calls require the
-- constraint though.  TODO(greg): Once 'Categorifier.C.PolyVec.PolyVec' is gone and the design is complete,
-- see if it is more elevant overall to represent the bitvector as a 'Word' type.
--
-- This could just as well be a type synonym, but that would lead to a few harmless declarations of
-- @UndecidableInstances@.
class
  ( KBits f Word8,
    KLiteral f Word8,
    KBits f Word16,
    KLiteral f Word16,
    KBits f Word32,
    KLiteral f Word32,
    KBits f Word64,
    KLiteral f Word64
  ) =>
  SupportsKBits f

-- | TODO(Greg H.): the claim is that some field is always `Identity` or `Proxy`, but for the
-- comparison code, we need this instance, which exists only for `Categorifier.C.KTypes.C.C` and
-- (here, sketchily) `Proxy`.
instance SupportsKBits Proxy

toBitfieldPrim ::
  forall f g.
  (Applicative g, SupportsKBits f) =>
  NE.NonEmpty (RfName, Compose g f Bool) ->
  Either Int (CBitfieldPrim (Compose g f))
toBitfieldPrim fields
  | n <= 8 = pure . BfpUInt8 $ toBitfieldPrim' (Compose (pure (kliteral 0)))
  | n <= 16 = pure . BfpUInt16 $ toBitfieldPrim' (Compose (pure (kliteral 0)))
  | n <= 32 = pure . BfpUInt32 $ toBitfieldPrim' (Compose (pure (kliteral 0)))
  | n <= 64 = pure . BfpUInt64 $ toBitfieldPrim' (Compose (pure (kliteral 0)))
  | otherwise = Left n
  where
    toBitfieldPrim' :: forall a. KBits f a => Compose g f a -> Compose g f a
    toBitfieldPrim' (Compose acc0) = Compose (setBitsFromList indexedBits acc0)
      where
        setBitsFromList :: [(Int, g (f Bool))] -> g (f a) -> g (f a)
        setBitsFromList ((k, val) : others) acc =
          setBitsFromList others (setBitTo <$> acc <*> pure k <*> val)
        setBitsFromList [] acc = acc
    indexedBits :: [(Int, g (f Bool))]
    indexedBits = zipWith f [0 ..] (NE.toList fields)
      where
        f :: Int -> (RfName, Compose g f Bool) -> (Int, g (f Bool))
        f k (_, Compose x) = (k, x)
    n = NE.length fields
{-# INLINEABLE toBitfieldPrim #-}

fromBitfieldPrim ::
  forall f g.
  (Applicative g, SupportsKBits f) =>
  CBitfield (Compose g f) ->
  NE.NonEmpty (RfName, Compose g f Bool)
fromBitfieldPrim (CBitfield _ fieldNames value) = case value of
  -- TODO(MP): This could probably done as a one-liner via an 'HK1Foldable' instance.
  BfpUInt8 prim -> extractBit prim
  BfpUInt16 prim -> extractBit prim
  BfpUInt32 prim -> extractBit prim
  BfpUInt64 prim -> extractBit prim
  where
    extractBit :: KBits f n => Compose g f n -> NE.NonEmpty (RfName, Compose g f Bool)
    extractBit p = fmap (fmap (testBit' p)) indexedFieldNames
    testBit' :: KBits f a => Compose g f a -> Int -> Compose g f Bool
    testBit' (Compose prim) index = Compose $ flip testBit index <$> prim
    indexedFieldNames :: NE.NonEmpty (RfName, Int)
    indexedFieldNames = NE.zip fieldNames (0 NE.:| [1 ..])
{-# INLINEABLE fromBitfieldPrim #-}

-- temporary, for render
--
-- It's not straightforward to unify this code with the function above.  This is due to the "partial
-- functional dependency" issue that causes confusion around things like 'PolyVec' -- we have
-- constraints on @f@ (see 'fromBitfieldPrim') and no way to articulate them if there's no exact
-- corresponding type (as in this situation).
--
-- The upshot is that I think we'd have to bring in 'C' or something else with 'KBits'
-- instances in order to call 'fromBitfieldPrim' directly.  Another option would be to make the two
-- type arguments of 'KBits' independent like in some of the other classes, so that we could place
-- the appropriate constraint constraint directly on the type of the argument to be passed to
-- 'testBit''.
fromBitfieldPrim' :: CBitfield Identity -> NE.NonEmpty (RfName, Bool)
fromBitfieldPrim' (CBitfield _ fieldNames value) = case value of
  BfpUInt8 prim -> extractBit prim
  BfpUInt16 prim -> extractBit prim
  BfpUInt32 prim -> extractBit prim
  BfpUInt64 prim -> extractBit prim
  where
    extractBit p = fmap (fmap (Bits.testBit p)) indexedFieldNames
    indexedFieldNames :: NE.NonEmpty (RfName, Int)
    indexedFieldNames = NE.zip fieldNames (0 NE.:| [1 ..])
{-# INLINEABLE fromBitfieldPrim' #-}

data CBitfieldPrim f
  = BfpUInt8 (f Word8)
  | BfpUInt16 (f Word16)
  | BfpUInt32 (f Word32)
  | BfpUInt64 (f Word64)
  deriving (Generic)

bfprimToPrim :: CBitfieldPrim f -> Prim f
bfprimToPrim (BfpUInt8 x) = PrimWord8 x
bfprimToPrim (BfpUInt16 x) = PrimWord16 x
bfprimToPrim (BfpUInt32 x) = PrimWord32 x
bfprimToPrim (BfpUInt64 x) = PrimWord64 x
{-# INLINEABLE bfprimToPrim #-}

-- | C++ struct constructor. Can be the only one in a struct or one of many in a variant.
data CxxCon f = CxxNormalCon DcName (NE.NonEmpty (RfName, CxxType f)) deriving (Generic)

-- | Type level Nat representation. Can either be a number or a
-- 'Categorifier.C.Math.Linear.Vectorize.vlength'.
data CNat
  = -- | type level nat like the dimension in @Vec (n :: Nat) a@.
    CNatInt Int
  | -- | 'Categorifier.C.Math.Linear.Vectorize.vlength'
    CNatType Int KTypeRep
  deriving (Eq, Ord, Show, Generic)

instance NFData CNat

cnatValue :: CNat -> Int
cnatValue (CNatInt r) = r
cnatValue (CNatType r _) = r
{-# INLINEABLE cnatValue #-}

-- | The only supported C++ primitives (besides the C primitives), in canonical order.
data CxxPrim f
  = PrimString (f T.Text)
  | PrimByteString (f ByteString)
  deriving (Generic)

instance GEnum (CxxPrim Proxy)

-- | All of the 'CxxPrim's in canonical order.
allCxxPrims :: [CxxPrim Proxy]
allCxxPrims = genum
{-# INLINEABLE allCxxPrims #-}

-- | Either a C++ or C struct constructor.
data CxxOrCCon f
  = CxxCon (CxxCon f)
  | CCon (CConF (BFix CTypeF) f)
  deriving (Generic)

deriving instance Eq (CEnum Proxy)

deriving instance Ord (CEnum Proxy)

deriving instance Show (CEnum Proxy)

instance
  (NFData1 f, forall p. IsPrimitive p => NFData (f p)) =>
  NFData (CEnum f)

deriving instance Eq (CBitfieldPrim Proxy)

deriving instance Ord (CBitfieldPrim Proxy)

deriving instance Show (CBitfieldPrim Proxy)

instance
  (NFData1 f, forall p. IsPrimitive p => NFData (f p)) =>
  NFData (CBitfieldPrim f)

deriving instance Eq (CBitfield Proxy)

deriving instance Ord (CBitfield Proxy)

deriving instance Show (CBitfield Proxy)

instance
  (NFData1 f, forall p. IsPrimitive p => NFData (f p)) =>
  NFData (CBitfield f)

deriving instance Eq (l Proxy) => Eq (CConF l Proxy)

deriving instance Ord (l Proxy) => Ord (CConF l Proxy)

deriving instance Show (l Proxy) => Show (CConF l Proxy)

instance
  (NFData1 f, NFData (l f), forall p. IsPrimitive p => NFData (f p)) =>
  NFData (CConF l f)

deriving instance Eq (CxxCon Proxy)

deriving instance Ord (CxxCon Proxy)

deriving instance Show (CxxCon Proxy)

instance
  ( NFData1 f,
    NFData (f T.Text),
    NFData (f ByteString),
    forall p. IsPrimitive p => NFData (f p)
  ) =>
  NFData (CxxCon f)

deriving instance Eq (CxxOrCCon Proxy)

deriving instance Ord (CxxOrCCon Proxy)

deriving instance Show (CxxOrCCon Proxy)

instance
  ( NFData1 f,
    NFData (f T.Text),
    NFData (f ByteString),
    forall p. IsPrimitive p => NFData (f p)
  ) =>
  NFData (CxxOrCCon f)

deriving instance Eq (l Proxy) => Eq (CTypeF l Proxy)

deriving instance Ord (l Proxy) => Ord (CTypeF l Proxy)

deriving instance Show (l Proxy) => Show (CTypeF l Proxy)

instance
  (NFData1 f, NFData (l Proxy), NFData (l f), forall p. IsPrimitive p => NFData (f p)) =>
  NFData (CTypeF l f)

deriving instance Eq (CxxType Proxy)

deriving instance Ord (CxxType Proxy)

deriving instance Show (CxxType Proxy)

instance
  ( NFData1 f,
    NFData (f T.Text),
    NFData (f ByteString),
    forall p. IsPrimitive p => NFData (f p)
  ) =>
  NFData (CxxType f)

deriving instance Eq (CxxPrim Proxy)

deriving instance Ord (CxxPrim Proxy)

deriving instance Show (CxxPrim Proxy)

instance
  ( NFData1 f,
    NFData (f T.Text),
    NFData (f ByteString),
    forall p. IsPrimitive p => NFData (f p)
  ) =>
  NFData (CxxPrim f)

deriving instance Eq (l Proxy) => Eq (CUnionConF l Proxy)

deriving instance Ord (l Proxy) => Ord (CUnionConF l Proxy)

deriving instance Show (l Proxy) => Show (CUnionConF l Proxy)

instance
  (NFData1 f, NFData (l Proxy), NFData (l f), forall p. IsPrimitive p => NFData (f p)) =>
  NFData (CUnionConF l f)

deriving instance Eq (CxxUnionCon Proxy)

deriving instance Ord (CxxUnionCon Proxy)

deriving instance Show (CxxUnionCon Proxy)

instance
  ( NFData1 f,
    NFData (f T.Text),
    NFData (f ByteString),
    forall p. IsPrimitive p => NFData (f p)
  ) =>
  NFData (CxxUnionCon f)

------------------------------ IMPORTANT! Do not change these! ----------------------------
-- CxxType supports `Data.Tree.Tree` and other structures which maybe be infinite.
-- If you used a naive equality/show/ord comparison on that CxxType it would go into
-- an infinite loop. We break the loop by comparing 'Proxy'-tagged types by their type reps.
--
-- C vs C++:
-- While you can model recursive types like 'Data.Tree.Tree' with 'CxxType', you cannot model
-- legal recursive 'CType's since structs are all statically-known size and we don't support
-- pointers. Unfortunately you could still write it in Haskell and get an infinite loop before
-- the C compiler gets a chance to reject it.
--
-- I have been bitten by this. In fact right now `Data.Tree.Tree` goes into an infinite loop
-- somewhere in the codegen process. But that's a regression and I need to track it down.
instance Show (CUnionF l Proxy) where
  showsPrec k = showsPrec k . cuTypeRep

instance Show (CxxUnion Proxy) where
  showsPrec k = showsPrec k . cxxuTypeRep

instance Show (CStructF l Proxy) where
  showsPrec k = showsPrec k . csTypeRep

instance Show (CxxStruct Proxy) where
  showsPrec k = showsPrec k . cxxsTypeRep

instance Eq (CStructF l Proxy) where
  (==) = over2 csTypeRep (==)

instance Eq (CxxStruct Proxy) where
  (==) = over2 cxxsTypeRep (==)

instance Eq (CUnionF l Proxy) where
  (==) = over2 cuTypeRep (==)

instance Eq (CxxUnion Proxy) where
  (==) = over2 cxxuTypeRep (==)

instance Ord (CStructF l Proxy) where
  compare = over2 csTypeRep compare

instance Ord (CxxStruct Proxy) where
  compare = over2 cxxsTypeRep compare

instance Ord (CUnionF l Proxy) where
  compare = over2 cuTypeRep compare

instance Ord (CxxUnion Proxy) where
  compare = over2 cxxuTypeRep compare

over2 :: (a -> b) -> (b -> b -> c) -> a -> a -> c
over2 f g x y = g (f x) (f y)

-- | Check if this is a 'CxxTypeArray' of a 'CType'. Don't descend further than one level in the
--   tree as each type is responsible for greedily checking itself on construction.
--
--   If it finds an array of `CType`, but finds an element that isn't a `CType` (which should be
--   impossible) it fails, returning the element.
oneLevelCxxArrayToMaybeCArray :: forall f. CxxType f -> Either (CxxType f) (CxxType f)
oneLevelCxxArrayToMaybeCArray type0@(CxxTypeArray nat cxxElemType cxxElems) = case cxxElemType of
  CxxTypeCType celemType ->
    let convertElem :: CxxType f -> Either (CxxType f) (CType f)
        convertElem (CxxTypeCType r) = pure r
        convertElem r = Left r
     in CxxTypeCType . CTypeArray nat (hembed celemType) . fmap hembed
          <$> traverse convertElem cxxElems
  _ -> pure type0
oneLevelCxxArrayToMaybeCArray r = pure r
{-# INLINEABLE oneLevelCxxArrayToMaybeCArray #-}

-- I'm not sure if these HK1Functor instances are a good idea or not.
instance Barbies.FunctorB CxxType where
  bmap :: forall f g. (forall a. f a -> g a) -> CxxType f -> CxxType g
  bmap f (CxxTypeCType x) = CxxTypeCType $ Barbies.bmap f x
  bmap f (CxxTypeStruct x) = CxxTypeStruct $ Barbies.bmap f x
  bmap f (CxxTypeUnion x) = CxxTypeUnion $ Barbies.bmap f x
  bmap f (CxxTypeArray nat x y) = CxxTypeArray nat x (Barbies.bmap f <$> y)
  bmap f (CxxTypePrim x) = CxxTypePrim $ Barbies.bmap f x
  bmap f (CxxTypeTuple xs) = CxxTypeTuple $ Barbies.bmap f <$> xs
  bmap f (CxxTypeVector ihl x y) = CxxTypeVector ihl x (Barbies.bmap f <$> y)
  bmap f (CxxTypeMap x y zs) = CxxTypeMap x y (g <$> zs)
    where
      g :: (CxxType f, CxxType f) -> (CxxType g, CxxType g)
      g (z1, z2) = (Barbies.bmap f z1, Barbies.bmap f z2)
  {-# INLINEABLE bmap #-}

instance Barbies.FunctorB CEnum

instance Barbies.FunctorB l => Barbies.FunctorB (CStructF l)

instance Barbies.FunctorT CStructF

instance Barbies.FunctorB l => Barbies.FunctorB (CUnionF l)

instance Barbies.FunctorT CUnionF

instance Barbies.FunctorB CxxStruct

instance Barbies.FunctorB CxxUnion

instance Barbies.FunctorB l => Barbies.FunctorB (CTypeF l)

instance Barbies.FunctorT CTypeF

instance Barbies.FunctorB CxxOrCCon

instance Barbies.FunctorB CxxCon

instance Barbies.FunctorB l => Barbies.FunctorB (CConF l)

instance Barbies.FunctorT CConF where
  tmap f = \case
    CNullaryCon name -> CNullaryCon name
    CNormalCon name tup fields -> CNormalCon name tup $ fmap (fmap f) fields
    CBitfieldCon bf -> CBitfieldCon bf

instance Barbies.ConstraintsB CBitfield

instance Barbies.FunctorB CBitfield

instance Barbies.FunctorB CxxPrim

instance Barbies.ConstraintsB CBitfieldPrim

instance Barbies.FunctorB CBitfieldPrim

-- | Use the HK1Functor instance to replace anything with 'Proxy'
discard :: forall x f. Barbies.FunctorB x => x f -> x Proxy
discard = Barbies.bmap discard'
  where
    discard' :: f a -> Proxy a
    discard' _ = Proxy
{-# INLINEABLE discard #-}

-- Helpers
conNumFields :: CxxOrCCon Proxy -> Int
conNumFields (CCon x) = cconNumFields x
conNumFields (CxxCon x) = cxxConNumFields x
{-# INLINEABLE conNumFields #-}

cxxConNumFields :: CxxCon Proxy -> Int
cxxConNumFields (CxxNormalCon _ fields) = NE.length fields
{-# INLINEABLE cxxConNumFields #-}

cconNumFields :: CConF l Proxy -> Int
cconNumFields (CBitfieldCon (CBitfield _ fields _)) = NE.length fields
cconNumFields CNullaryCon {} = 0
cconNumFields (CNormalCon _ _ fields) = NE.length fields
{-# INLINEABLE cconNumFields #-}

cconName :: CConF l f -> DcName
cconName (CNullaryCon r) = r
cconName (CNormalCon r _ _) = r
cconName (CBitfieldCon (CBitfield r _ _)) = r
{-# INLINEABLE cconName #-}

cxxConName :: CxxCon f -> DcName
cxxConName (CxxNormalCon r _) = r
{-# INLINEABLE cxxConName #-}

cxxOrCConName :: CxxOrCCon f -> DcName
cxxOrCConName (CxxCon r) = cxxConName r
cxxOrCConName (CCon r) = cconName r
{-# INLINEABLE cxxOrCConName #-}

allEnumCons :: CEnum f -> [CEnum Identity]
allEnumCons cenum = take n (toEnumData <$> [0 ..])
  where
    n = V.length (ceCons cenum)
    toEnumData k = cenum {ceData = k}
{-# INLINEABLE allEnumCons #-}

enumDcName :: CEnum Identity -> DcName
enumDcName CEnum {ceTypeRep = rep, ceCons = cons, ceData = Identity k} =
  case cons V.!? fromIntegral k of
    Just r -> r
    Nothing ->
      error [fmt|enum {show rep} got out-of-range value {k}: {show cons} ({show $ V.length cons})|]
{-# INLINEABLE enumDcName #-}
