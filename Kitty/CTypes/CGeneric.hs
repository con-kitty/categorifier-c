{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A toolkit like "GHC.Generics" that is aware of C language representation issues.
--
-- For an example of how to use this toolkit to operate generically on Haskell types while
-- respecting their C representation constraints, see "Kitty.CTypes.CGeneric.CxxType".  For examples
-- of how to imbue your data types with a `CGeneric` representation, see
-- "Kitty.CTypes.CGeneric.Test".
--
-- This module is intended to be imported qualified, e.g. as @CG@.
module Kitty.CTypes.CGeneric
  ( -- * Generic representation of types that can be generated to C code
    CGeneric (..),

    -- ** Deriving instances via `CGeneric`
    CGenerically (..),

    -- ** Primitive types
    Prim (..),

    -- ** Bitfields
    Bitfield (..),
    RepBitfield,
    AsBitfield (..),
    AsBitfieldNewtype (..),

    -- *** Bitfield helpers
    BitfieldCompatible,
    BitfieldWordType,
    GCoercible,

    -- ** Arrays
    Array (..),

    -- *** Normal arrays
    RepArray,
    AsArray (..),

    -- *** Flattened arrays
    RepFlattenedArray,
    AsFlattenedArray (..),

    -- ** Enumerations
    Enum (..),
    EnumCompatible,
    CountEnum (..),
    GIsEnum,

    -- ** Providing field names
    WithFieldNames,
    ExplicitFieldNames (..),

    -- * Tools for instance-writers
    CountFields,
    CountConstructors,

    -- ** Implementing primitive instances
    PrimRep,
    primFrom,
    primTo,

    -- * Default instance generation

    -- ** `Generic`-based deriving of `Rep`
    GRep,

    -- ** `Generic`-based deriving of `from`
    CanGenericFrom,
    GFrom (..),

    -- ** `Generic`-based deriving of `to`
    CanGenericTo,
    GTo (..),

    -- ** Bitfield handling
    Bits (..),
  )
where

import Accessors (Lookup (..))
import Codec.Serialise (Serialise)
import Data.Bifunctor (first, second)
import Data.Coerce (coerce)
import Data.Functor (void)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Type.Nat (Nat (..))
import qualified Data.Type.Nat as Nat
import Data.Typeable (Typeable)
import Data.Vec.Lazy (Vec (..))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
  ( C1,
    D1,
    Generic,
    Generic1,
    K1 (..),
    M1 (..),
    Meta (..),
    Par1 (..),
    Rec0,
    Rec1 (..),
    S1,
    (:*:) (..),
    (:+:) (..),
    (:.:) (..),
  )
import qualified GHC.Generics as G
import Kitty.CExpr.Cat.TargetOb (TargetOb)
import Kitty.CTypes.CGeneric.Class
  ( Array (..),
    ArraySize,
    Bitfield (..),
    Bits (..),
    CGeneric (..),
    CanGenericFrom,
    CanGenericTo,
    CountConstructors,
    CountEnum (..),
    CountFields,
    Enum (..),
    ExplicitFieldNames (..),
    GArrayType,
    GFrom (..),
    GIsEnum,
    GRep,
    GTo (..),
    Prim (..),
    PrimRep,
    WithFieldNames,
    primFrom,
    primTo,
  )
import Kitty.CTypes.GArrays (GArrays)
import Kitty.CTypes.ToCxxType (ToCxxType (..))
import Kitty.CTypes.Types (SupportsKBits)
import qualified Kitty.Cat.Client.Internal as ConCat
import qualified Kitty.KTypes.KBits as KBits
import Kitty.Nat.Operators (type (*), type (+), type (<=))
import Test.QuickCheck (Arbitrary)
import Prelude hiding (Enum)

-- | `CGenerically` is provided mainly to be used with @-XDerivingVia@ to derive other instances via
-- a `CGeneric` instance.  Individual instances are provided as orphans within the appropriate
-- modules (e.g. "Kitty.CTypes.CGeneric.Serialise").  It is the `CGeneric` version of the
-- [@Generically@](https://hackage.haskell.org/package/generic-data/docs/Generic-Data.html#t:Generically)  [ ](DONTLINTLINELENGTH)
-- wrapper from the [@generic-data@](https://hackage.haskell.org/package/generic-data) package.
--
-- == Usage examples from the "Kitty.CTypes.CGeneric.Test" module
--
-- This section shows how to use `CGenerically` with @-XDerivingVia@ to derive instances.
--
-- > import qualified Codec.Serialise as Serialise
-- > import qualified Kitty.CTypes.CGeneric.Serialise as CG
-- > import Kitty.CTypes.CGeneric (CGeneric (..), CGenerically (..))
--
-- === Simple types
--
-- A simple type with no arguments can easily be endowed with a `Codec.Serialise.Serialise`
-- instance:
--
-- > data MetaSyn
-- >   = Foo
-- >   | Bar
-- >   | Baz
-- >   | Quux
-- >   deriving (Eq, Show, Enum, Bounded, Generic, CGeneric)
-- >   deriving (Serialise.Serialise) via CGenerically MetaSyn
--
-- === Higher-kinded data
--
-- A type parameterized over `Kitty.KTypes.C.C` and similar type constructors typically
-- needs to have its parameter specialized to `Kitty.KTypes.C.C`, which can be done in a
-- standalone deriving-via declaration:
--
-- > deriving via CGenerically (MyType C) instance Serialise.Serialise (MyType C)
--
-- === Functors
--
-- A functor type typically needs to have a constraint written on its argument.  Be careful to note
-- which @Serialise@ class comes from which module here:
--
-- > data Rotors a = Rotors
-- >   { rotorA :: a,
-- >     rotorB :: a,
-- >     rotorC :: a
-- >   }
-- >   deriving (Eq, Show, Generic, Generic1, Functor, Foldable, Traversable)
-- >   deriving (CGeneric) via AsArray (Rotors a)
-- >
-- > deriving via
-- >   CGenerically (Rotors a)
-- >   instance
-- >     CG.Serialise a =>
-- >     Serialise.Serialise (Rotors a)
--
-- == Internal use
--
-- This is also used to provide functions which require going through an upstream class without
-- requiring instances of this upstream class for any of our own `CGeneric` data.  For example, in
-- the "Kitty.CTypes.CGeneric.Serialise" module, `Kitty.CTypes.CGeneric.Serialise.serialise` is
-- defined using `CGenerically`:
--
-- > serialise :: Serialise a => a -> ByteString
-- > serialise = Serialise.serialise . CGenerically
--
-- The important thing here is that the unqualified `Kitty.CTypes.CGeneric.Serialise.Serialise`
-- constraint requires only a `CGeneric` instance; it is different to the upstream
-- `Codec.Serialise.Serialise` class from the @serialise@ package.  This function can be used
-- without any instances of the upstream class.
newtype CGenerically a = CGenerically {cGenerically :: a}

type family MaybeArrayType cgrep where
  MaybeArrayType (D1 _dm x) = MaybeArrayType x
  MaybeArrayType (C1 _dm x) = MaybeArrayType x
  MaybeArrayType (Array _size elem) = 'Just elem
  MaybeArrayType _ = 'Nothing

-- | This constraint implies that polymorphic arguments of kind @`Type` -> `Type`@ can be used in
-- enum representation.
type EnumCompatible f = (KBits.KBits f Word8, Typeable f)

{- Generic representation -}

type family And a b where
  And 'True 'True = 'True
  And _ _ = 'False

-- | Can the type represented generically by @rep@ be treated as a C bitfield?
type family GCanBeBitfield rep where
  GCanBeBitfield (D1 _m x) = GCanBeBitfield x
  GCanBeBitfield (C1 _m x) = GCanBeBitfield x
  GCanBeBitfield (a :*: b) = And (GCanBeBitfield a) (GCanBeBitfield b)
  GCanBeBitfield (S1 ms (Rec0 Bool)) = 'True
  GCanBeBitfield (S1 ms (Rec0 (f Bool))) = 'True
  GCanBeBitfield (S1 ms (Rec0 a)) = 'False
  GCanBeBitfield (a :+: b) = 'False

type family GBitfieldSize rep where
  GBitfieldSize (D1 _m x) = GBitfieldSize x
  GBitfieldSize (C1 _m x) = GBitfieldSize x
  GBitfieldSize (a :*: b) = GBitfieldSize a + GBitfieldSize b
  GBitfieldSize (S1 ms (Rec0 Bool)) = 'S 'Z
  GBitfieldSize (S1 ms (Rec0 (f Bool))) = 'S 'Z
-- The only time we should encounter this is with a wrapping newtype.
  GBitfieldSize (S1 ms (Rec0 a)) = GBitfieldSize (G.Rep a)

type family JustEq a b where
  JustEq ('Just x) ('Just x) = 'Just x
  JustEq 'Nothing 'Nothing = 'Nothing

type family GBitfieldWrapper rep where
  GBitfieldWrapper (D1 _m x) = GBitfieldWrapper x
  GBitfieldWrapper (C1 _m x) = GBitfieldWrapper x
  GBitfieldWrapper (a :*: b) = JustEq (GBitfieldWrapper a) (GBitfieldWrapper b)
  GBitfieldWrapper (S1 ms (Rec0 Bool)) = 'Nothing
  GBitfieldWrapper (S1 ms (Rec0 (f Bool))) = 'Just f

type GWordType rep = GWordType' (GBitfieldWrapper rep) (GBitfieldSize rep)

type family GWordType' wrap size where
  GWordType' ('Just f) size = f (GWordPrimitiveType size)
  GWordType' 'Nothing size = GWordPrimitiveType size

type GWordPrimitiveType size =
  GWordPrimitiveType'
    (size <= Nat.FromGHC 8)
    (size <= Nat.FromGHC 16)
    (size <= Nat.FromGHC 32)
    (size <= Nat.FromGHC 64)

type family GWordPrimitiveType' c8 c16 c32 c64 where
  GWordPrimitiveType' 'False 'False 'False 'True = Word64
  GWordPrimitiveType' 'False 'False 'True 'True = Word32
  GWordPrimitiveType' 'False 'True 'True 'True = Word16
  GWordPrimitiveType' 'True 'True 'True 'True = Word8

-- | This type family allows you to mark a type that should be treated as a bitfield so that GHC can
-- derive appropriately. You can get an automatically generated implementation of `Rep`, `to`
-- and `from` for a type which is to be represented as a bitfield if you use the `RepBitfield`
-- type family to explain what the `Rep` should be.  For example, the complete instance for
-- @Switches@ (see documentation for `Bitfield` for the type definition) consists of the
-- following:
--
-- > instance ( BitfieldCompatible f (Switches f),
-- >            Generic (Switches f)
-- >          ) => CGeneric (Switches f) where
-- >   type Rep (Switches f) = RepBitfield (G.Rep (Switches f))
--
-- Also see `AsBitfield` for how to use the @deriving via@ feature to do the same thing.
type RepBitfield rep = RepBitfield' (GWordType rep) (GCanBeBitfield rep) rep

type family RepBitfield' word ok rep where
  RepBitfield' word 'True (D1 dm (C1 cm (a :*: b))) =
    D1 dm (C1 cm (Bitfield word (a :*: b)))
  RepBitfield' word 'True (D1 dm (C1 cm (S1 sm (Rec0 (f Bool))))) =
    D1 dm (C1 cm (Bitfield word (S1 sm (Rec0 (f Bool)))))
  RepBitfield' word 'True (D1 dm (C1 cm (S1 sm (Rec0 Bool)))) =
    D1 dm (C1 cm (Bitfield word (S1 sm (Rec0 Bool))))

-- | This type synonym produces the type used to represent @a@ as a bitfield in its `Rep`.
type BitfieldWordType a = GWordType (G.Rep a)

-- | This type synonym produces the underlying unsigned integral type representing a bitfield in its
-- `Rep`, stripping off any wrappers it finds (e.g. `Kitty.KTypes.C.C` or
-- `Data.Functor.Identity.Identity`).
type BitfieldPrimitiveWordType a = GWordPrimitiveType (GBitfieldSize (G.Rep a))

-- | This constraint implies that polymorphic arguments of kind @`Type` -> `Type`@ can be used in
-- bitfield representation via the given @word@ type.
type BitfieldCompatible f a =
  ( KBits.KBits f (BitfieldPrimitiveWordType a),
    Typeable f
  )

-- | This wrapper allows you to mark that a type should be generically represented as a C bitfield.
--
-- == Marking one field of a structure
--
-- The following code ensures that the field @aBitfield@ of @MyStruct@ will be encoded as a
-- bitfield.
--
-- > data MyStruct f = MyStruct
-- >   { someOtherField :: f Double
-- >   , aBitfield :: AsBitfield (V3 (f Bool))
-- >   }
--
-- This is furthermore the only practical way to represent a functor (`Linear.V3.V3` in the above
-- example) full of @f `Bool`@s as a bitfield.  Unfortunately, this requires you to manually deal
-- with the `AsBitfield` wrapper.  It is more convenient to define a dedicated type or @newtype@
-- wrapper and mark that type as always corresponding to a bitvector, as described below.
--
-- == Marking a whole structure using @deriving via@
--
-- The following example is equivalent to the instance definition given in the `RepBitfield`
-- documentation.
--
-- > import Kitty.CTypes.CGeneric (CGeneric, AsBitfield (..))
-- >
-- > deriving via AsBitfield (Switches f)
-- >   instance BitfieldCompatible f (Switches f) =>
-- >   CGeneric (Switches f)
--
-- __You must write this as a standalone deriving statement__ in order to provide the
-- `BitfieldCompatible` constraint on @f@ when it appears the instance head.
--
-- Note that you must import the constructor (@`AsBitfield` (..)@) in order for @deriving via@ to
-- understand how to `Data.Coerce.coerce`.
newtype AsBitfield a = AsBitfield {getAsBitfield :: a}
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving newtype (Serialise, Arbitrary)

ConCat.deriveHasRep ''AsBitfield

type instance TargetOb (AsBitfield a) = AsBitfield (TargetOb a)

instance
  ( Generic a,
    GFrom 'False (G.Rep a) (RepBitfield (G.Rep a)),
    GTo 'False (G.Rep a) (RepBitfield (G.Rep a)),
    GArrays f (RepBitfield (G.Rep a) ())
  ) =>
  GArrays f (AsBitfield a)

instance Lookup a => Lookup (AsBitfield a) where
  toAccessorTree lensbwrapper = toAccessorTree (lensbwrapper . unwrap)
    where
      unwrap k (AsBitfield a) = AsBitfield <$> k a

instance
  ( Generic a,
    GFrom 'False (G.Rep a) (RepBitfield (G.Rep a)),
    GTo 'False (G.Rep a) (RepBitfield (G.Rep a))
  ) =>
  CGeneric (AsBitfield a)
  where
  type Rep (AsBitfield a) = RepBitfield (G.Rep a)
  from = gFrom . G.from . getAsBitfield
  to = AsBitfield . G.to . gTo
  {-# INLINEABLE from #-}
  {-# INLINEABLE to #-}

instance (ToCxxType f a, SupportsKBits f) => ToCxxType f (AsBitfield a) where
  toCxxType = toCxxType . fmap getAsBitfield

type GCoercible a b n m p mc ms =
  (Generic a, Generic b, G.Rep a ~ D1 ('MetaData n m p 'True) (C1 mc (S1 ms (Rec0 b))))

-- | Unwrap a newtype.  The proxy must refer to the newtype wrapper.
gcoerce ::
  forall a b x n m p mc ms.
  GCoercible a b n m p mc ms =>
  Proxy a ->
  G.Rep a x ->
  G.Rep b x
gcoerce Proxy = G.from . unK1 . unM1 . unM1 . unM1

-- | Re-wrap a value with a newtype.  The proxy must refer to the newtype wrapper.
gUncoerce ::
  forall a b n m p mc ms x.
  GCoercible a b n m p mc ms =>
  Proxy a ->
  G.Rep b x ->
  G.Rep a x
gUncoerce Proxy = M1 . M1 . M1 . K1 . G.to

-- | This is an unfortunate hack.
--
-- We don't have a good way to assess at the type level whether something is composed of only
-- boolean fields in memory in the face of newtypes and nesting.  The primary reasons for this are:
--
--   * We cannot dispatch when evaluating the `GCanBeBitfield` type family equations (and related
--     ones) whether or not a given type has an instance of `Generic`.  If we try to call out to
--    `G.Rep` inside our type family when we hit a record field and the target type @a@ does not
--     have a `Generic` instance (for example if it's a primitive type), GHC will simply not
--     evaluate `G.Rep` and we'll get a highly confusing error
--
--   * We can manually add cases for all the C primitive types to our type family to get around the
--     previous problem, but we would have to extend all our Rep handling to accommodate nested
--     structs (or at least wrapped ones).  This makes things a good deal more complicated in all
--     the instances; currently we only deal with nested functors via `Generic1`-based
--     `Rep` structures, which is a good way to keep things separate.
--
-- To avoid taking the time to solve this problem well, we provide this wrapper, for which the user
-- must manually specify the number of newtype wrappers around the type of interest.
--
-- @x@ is the non-`newtype` type we expect to be at the bottom of @n@ layers of wrapping.
--
-- == Marking individual fields of a structure
--
-- > data HasAFrame frame f = HasAFrame {
-- >   fieldWithAFrame :: AsBitfieldNewtype (V3 (f Bool)) (V3T frame (f Bool)),
-- >   anotherFieldWithAFrame :: AsBitfieldNewtype (V3 (f Bool)) (Identity (V3T frame (f Bool)))
-- >   } deriving (Generic, CGeneric)
--
-- == Marking a whole structure
--
-- If you want support for this, ask in [@#hs_codegen@](https://hvsd.slack.com/archives/CSQ9LRUJX)
-- on Slack.
--
-- == Usage tips
--
-- If you don't import the constructor of the relevant newtype wrapper with @(..)@, you will get
-- errors related to GHC's inability to match representations.  It needs to see the constructor to
-- use the `Data.Coerce.Coercible` instance.
--
-- Unfortunately for the moment you must pass @x@ as the argument to `BitfieldCompatible`; for
-- example:
--
-- > hasAFrameToCType ::
-- >   ( Applicative g,
-- >     Traversable g,
-- >     Typeable frame,
-- >     BitfieldCompatible f (V3 (f Bool))
-- >   ) =>
-- >   g (HasAFrame frame f) -> CType (Compose g f)
-- > hasAFrameToCType = CG.toCType
--
-- == Passing through multiple newtypes
--
-- If you want support for this, ask in [@#hs_codegen@](https://hvsd.slack.com/archives/CSQ9LRUJX)
-- on Slack.
newtype AsBitfieldNewtype x a = AsBitfieldNewtype {getAsBitfieldNewtype :: a}
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving newtype (Serialise, Arbitrary)

ConCat.deriveHasRep ''AsBitfieldNewtype

type instance TargetOb (AsBitfieldNewtype x a) = AsBitfieldNewtype (TargetOb x) (TargetOb a)

instance
  ( GCoercible a x nm md pkg mc ms,
    GFrom 'False (G.Rep x) (RepBitfield (Rep x)),
    GTo 'False (G.Rep x) (RepBitfield (Rep x)),
    GArrays f (RepBitfield (Rep x) ())
  ) =>
  GArrays f (AsBitfieldNewtype x a)

instance Lookup a => Lookup (AsBitfieldNewtype x a) where
  toAccessorTree lensbwrapper = toAccessorTree (lensbwrapper . unwrap)
    where
      unwrap k (AsBitfieldNewtype a) = AsBitfieldNewtype <$> k a

instance
  ( GCoercible a x nm md pkg mc ms,
    GFrom 'False (G.Rep x) (Rep (AsBitfieldNewtype x a)),
    GTo 'False (G.Rep x) (Rep (AsBitfieldNewtype x a))
  ) =>
  CGeneric (AsBitfieldNewtype x a)
  where
  -- It should be safe to use the `Rep` of @x@ here, because it must be a product of some kind,
  -- and in that case, the only difference between the `G.Rep` and the `Rep` is that the latter may
  -- have the user-supplied field name information needed to enable conversion to @CType@.
  type Rep (AsBitfieldNewtype x a) = RepBitfield (Rep x)
  from = gFrom . gcoerce (Proxy @a) . G.from . getAsBitfieldNewtype
  to = AsBitfieldNewtype . G.to . gUncoerce (Proxy @a) . gTo

instance (ToCxxType f a, SupportsKBits f) => ToCxxType f (AsBitfieldNewtype x a) where
  toCxxType = toCxxType . fmap getAsBitfieldNewtype

-- | This type family allows you to mark a type that should be treated as a C array so that GHC can
-- derive appropriately.  Just like with `Bitfield`, you may use the `RepArray` type family to
-- manually inform GHC that the type in question is to be represented as a C array rather than a
-- product; this will allow `to` and `from` to be generated automatically.  For example, the
-- complete instance for `Kitty.Config.Types.Rotors` consists of the following:
--
-- > instance CGeneric (Rotors a) where
-- >   type Rep (Rotors a) = RepArray (G.Rep (Rotors a))
--
-- Also see `AsArray` for how to use the @deriving via@ feature to do the same thing.
type RepArray rep = RepArray' (ArraySize rep) (GArrayType rep) rep

-- | `RepArray'` takes in a @size@ argument for the case where we statically know the size of a
-- vector but have no proper `Generic` structure -- it really is just a flat array.  The first
-- equation calculates the appropriate `Rep` based on the given "GHC.Generics" `G.Rep` (@rep@).  The
-- second equation calculates it based on the given size, using only the `D1` and `C1` metadata from
-- the given `G.Rep`.
type family RepArray' size elem rep where
  RepArray' size ('Just ('Just elem)) (D1 dm (C1 cm (a :*: b))) =
    D1 dm (C1 cm (Array size elem))
  RepArray' size ('Just ('Just elem)) (D1 dm (C1 cm (S1 _sm (Rec0 (Vec size elem))))) =
    D1 dm (C1 cm (Array size elem))

-- | This wrapper allows you to mark that a type should be generically represented as a C array.
--
-- == Marking one field of a structure
--
-- This wrapper can be used to mark a field in a larger structure.  Consider the standard structure
--
-- > data AsAStruct f = AsAStruct
-- >   { field1 :: f Double,
-- >     field2 :: V3 (f Double),
-- >   }
--
-- This structure will be represented as a struct with two fields, the former a `Double` and the
-- latter a struct of type `Linear.V3.V3`.  The following similar structure
--
-- > data AsAnArray f = AsAnArray
-- >   { field1 :: f Double,
-- >     field2 :: AsArray (V3 (f Double)),
-- >   }
--
-- will be represented instead as a struct with two fields, the latter a C array of `Double`s of
-- length 3, thanks to the presence of the `AsArray` newtype wrapper.
--
-- == Marking a whole structure using @deriving via@
--
-- The final line in the following example is equivalent to the instance definition given in the
-- `RepArray` documentation.
--
-- > import Kitty.CTypes.CGeneric (CGeneric, AsArray (..))
-- >
-- > data Rotors a = Rotors
-- >   { rotorA :: a,
-- >     rotorB :: a,
-- >     rotorC :: a
-- >   } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
-- >     deriving CGeneric via AsArray (Rotors a)
--
-- Note that you must import the constructor (@`AsArray` (..)@) in order for @deriving via@ to
-- understand how to `Data.Coerce.coerce`.
newtype AsArray a = AsArray {getAsArray :: a} deriving (Eq, Ord, Show, Generic)

instance
  ( Generic a,
    GFrom 'False (G.Rep a) (RepArray (G.Rep a)),
    GTo 'False (G.Rep a) (RepArray (G.Rep a))
  ) =>
  CGeneric (AsArray a)
  where
  type Rep (AsArray a) = RepArray (G.Rep a)
  from = gFrom . G.from . getAsArray
  to = AsArray . G.to . gTo
  {-# INLINEABLE from #-}
  {-# INLINEABLE to #-}

-- | This wrapper allows you to mark that a type should be generically represented as a C array,
-- independent of functor nesting structure.
--
-- __The marked structure must have `Generic1` and `Functor` instances__ for this wrapper to work.
--
-- == Marking one field of a structure
--
-- This works just like `AsArray`:
--
-- > data AsAFlatArray f = AsAFlatArray
-- >   { field1 :: f Double,
-- >     field2 :: AsFlattenedArray (V3 (Product V3 V2)) (f Double)
-- >   }
--
-- The second field will be represented as an array of 15 (@3 * (2 + 3)@) `Double`s.
--
-- == Marking a whole structure using @deriving via@
--
-- This also works just like `AsArray`, but note that we must derive `Generic1` and `Functor`:
--
-- > data Irregular a = Irregular
-- >   { field1 :: a,
-- >     field2 :: V3 a,
-- >     field3 :: Product V3 Rotors a
-- >   } deriving (Generic, Generic1, Functor)
-- >     deriving CGeneric via AsFlattenedArray Irregular a
newtype AsFlattenedArray f a = AsFlattenedArray {getAsFlattenedArray :: f a}
  deriving (Eq, Ord, Show, Generic)

type family FlattenArraySize rep1f where
  FlattenArraySize (D1 _dm x) = FlattenArraySize x
  FlattenArraySize (C1 _cm x) = FlattenArraySize x
  FlattenArraySize (S1 _sm x) = FlattenArraySize x
  FlattenArraySize (a :*: b) = FlattenArraySize a + FlattenArraySize b
-- This is for `Compose` and similar.
  FlattenArraySize (a :.: b) = FlattenArraySize (G.Rep1 a) * FlattenArraySize b
  FlattenArraySize (Rec1 f) = FlattenArraySize (G.Rep1 f)
-- Keep in mind that with nested functors, this will only appear at the actual level of the
-- functor argument, thanks to `:.:`.
  FlattenArraySize Par1 = 'S 'Z
-- This equation is for backwards-compatibility with `ArraySize`
  FlattenArraySize (Rec0 a) = 'S 'Z

type family RepFA size elem rep1f where
  RepFA size elem (D1 dm (C1 cm (_a :*: _b))) =
    D1 dm (C1 cm (Array size elem))
  RepFA size elem (D1 dm (C1 cm (S1 _sm (_a :.: _b)))) =
    D1 dm (C1 cm (Array size elem))

-- | This type is to `AsFlattenedArray` as `RepArray` is to `AsArray`.  You can use it to
-- compute a `Rep` which will represent your type as a flattened array from the type's `Rep`, just
-- like you can do for normal arrays using `RepArray`.  See the `RepArray` docs for an example.
type RepFlattenedArray f a = RepFA (FlattenArraySize (G.Rep1 f)) a (G.Rep1 f)

instance
  ( Generic1 f,
    -- This is always going to be a functor, because it'll be a `Rep` containing a
    -- `Array`.  The final type argument is phantom.
    Functor (RepFlattenedArray f a),
    (G.Rep1 f :: Type -> Type) ~ D1 dm (C1 cm more),
    (RepFlattenedArray f a :: Type -> Type)
      ~ D1 dm (C1 cm (Array (FlattenArraySize (G.Rep1 f)) a)),
    GFrom1 a (G.Rep1 f) (RepFlattenedArray f a),
    GTo1 a (G.Rep1 f) (RepFlattenedArray f a)
  ) =>
  CGeneric (AsFlattenedArray f a)
  where
  type Rep (AsFlattenedArray f a) = RepFlattenedArray f a
  from = void . gFrom1 @a . G.from1 . getAsFlattenedArray
  to =
    AsFlattenedArray
      . G.to1
      . gTo1 @a
      . coerce @(RepFlattenedArray f a ()) @(RepFlattenedArray f a a)

{- Array handling -}

-- | Class for getting array fields when there are nested functors
class FlattenArrayFields (offset :: Nat) size elem rep x | elem rep x -> size where
  fieldsToArrayFlat :: rep x -> Vec offset elem -> Vec (size + offset) elem
  arrayToFieldsFlat :: Vec (size + offset) elem -> (rep x, Vec offset elem)

instance
  ( FlattenArrayFields offset size elem f x
  ) =>
  FlattenArrayFields offset size elem (S1 m f) x
  where
  fieldsToArrayFlat = fieldsToArrayFlat . unM1
  {-# INLINEABLE fieldsToArrayFlat #-}

  arrayToFieldsFlat = first M1 . arrayToFieldsFlat
  {-# INLINEABLE arrayToFieldsFlat #-}

-- | A product of fields
instance
  ( FlattenArrayFields (rsize + offset) lsize elem a x,
    FlattenArrayFields offset rsize elem b x,
    lsize + rsize ~ size,
    lsize + (rsize + offset) ~ (size + offset)
  ) =>
  FlattenArrayFields offset size elem (a :*: b) x
  where
  fieldsToArrayFlat (a :*: b) = fieldsToArrayFlat a . fieldsToArrayFlat b
  {-# INLINEABLE fieldsToArrayFlat #-}

  arrayToFieldsFlat =
    (\(v1, (v2, s)) -> (v1 :*: v2, s)) . second arrayToFieldsFlat . arrayToFieldsFlat
  {-# INLINEABLE arrayToFieldsFlat #-}

-- | This instance goes through fields which contain a functor of elements; it is used only when
-- producing a flattened (`AsFlattenedArray` / `RepFlattenedArray`) array.
instance
  ( FlattenArrayFields offset size elem repf x,
    D1 dm (C1 cm repf) ~ G.Rep1 f,
    Generic1 f
  ) =>
  FlattenArrayFields offset size elem (Rec1 f) x
  where
  fieldsToArrayFlat = fieldsToArrayFlat . unM1 . unM1 . G.from1 . unRec1
  {-# INLINEABLE fieldsToArrayFlat #-}

  arrayToFieldsFlat = first (Rec1 . G.to1 . M1 . M1) . arrayToFieldsFlat
  {-# INLINEABLE arrayToFieldsFlat #-}

-- | This instance occurs when we have arrived at the `elem` corresponding to a type.  As we move
-- between instances of `FlattenArrayFields`, we keep the `elem` the same from the original functor
-- type, so we know when it matches the last arg of this class that we're in the right place.
instance FlattenArrayFields offset ('S 'Z) elem Par1 elem where
  fieldsToArrayFlat (Par1 x) = (x :::)
  {-# INLINEABLE fieldsToArrayFlat #-}

  arrayToFieldsFlat (h ::: t) = (Par1 h, t)
  {-# INLINEABLE arrayToFieldsFlat #-}

-- | This instance is triggered when we have unwrapped an @a `:.:` Rec1 b@ and are working our way
-- through the structure of the @a@.  Since we now have a @`Rec1` b elem@ in the final class
-- argument instead of an @elem@, it's time to unwrap the @b@ with its `Generic1` instance.
instance
  ( FlattenArrayFields offset size elem nest elem,
    D1 dm (C1 cm nest) ~ G.Rep1 b,
    Generic1 b
  ) =>
  FlattenArrayFields offset size elem Par1 (Rec1 b elem)
  where
  fieldsToArrayFlat = fieldsToArrayFlat . unM1 . unM1 . G.from1 . unRec1 . unPar1
  {-# INLINEABLE fieldsToArrayFlat #-}

  arrayToFieldsFlat = first (Par1 . Rec1 . G.to1 . M1 . M1) . arrayToFieldsFlat
  {-# INLINEABLE arrayToFieldsFlat #-}

instance
  ( sizea ~ FlattenArraySize (G.Rep1 a),
    sizeb ~ FlattenArraySize b,
    size ~ (sizea * sizeb),
    FlattenArrayFields offset size elem (a :.: b) elem
  ) =>
  FlattenArrayFields offset size elem Par1 ((a :.: b) elem)
  where
  fieldsToArrayFlat (Par1 adotb) = fieldsToArrayFlat adotb
  {-# INLINEABLE fieldsToArrayFlat #-}

  arrayToFieldsFlat = first Par1 . arrayToFieldsFlat
  {-# INLINEABLE arrayToFieldsFlat #-}

-- | This instance just unwraps the `:.:` newtype and does the same operation on the unwrapped
-- composite functor.
--
-- NB: due to the action of `G.from1` and `G.to1`, the @nest@ type will have a `Par1` constructor
-- wrapping a @`Rec1` b elem@.  It is important not to apply `FlattenArraySize` to this unwrapped
-- @nest@ type, because the `Par1`s will mislead it.  This is not a type-safety issue, as we match
-- @elem@ and @x@ in the instances, but it will lead to compile errors.
instance
  ( sizea ~ FlattenArraySize (G.Rep1 a),
    sizeb ~ FlattenArraySize b,
    size ~ (sizea * sizeb),
    FlattenArrayFields offset size elem nest (b elem),
    D1 dm (C1 cm nest) ~ G.Rep1 a,
    Generic1 a
  ) =>
  FlattenArrayFields offset size elem (a :.: b) elem
  where
  fieldsToArrayFlat = fieldsToArrayFlat . unM1 . unM1 . G.from1 . unComp1
  {-# INLINEABLE fieldsToArrayFlat #-}

  arrayToFieldsFlat = first (Comp1 . G.to1 . M1 . M1) . arrayToFieldsFlat
  {-# INLINEABLE arrayToFieldsFlat #-}

-- | This class can be used to produce a `from` implementation by processing the `G.Rep1` of a
-- functor instead of processing the `G.Rep` of a fully applied type.  It is used only to produce
-- instances which represent nested functors as flattened arrays; see `AsFlattenedArray`.
--
-- The key difference between this class and `GFrom` is that the type argument @elem@ of the
-- `G.Rep1` in `gFrom1` is fixed in the class definition; this allows traversing through many layers
-- of functor without losing track of what base type we're dealing with.
class GFrom1 elem rep cgrep where
  gFrom1 :: rep elem -> cgrep elem

-- | See `GFrom1`.
class GTo1 elem rep cgrep where
  gTo1 :: cgrep elem -> rep elem

instance
  ( FlattenArrayFields 'Z size elem repf elem,
    size + 'Z ~ size
  ) =>
  GFrom1 elem (D1 dm (C1 cm repf)) (D1 dm (C1 cm (Array size elem)))
  where
  gFrom1 = M1 . M1 . Array . flip fieldsToArrayFlat VNil . unM1 . unM1
  {-# INLINEABLE gFrom1 #-}

instance
  ( FlattenArrayFields 'Z size elem repf elem,
    size + 'Z ~ size
  ) =>
  GTo1 elem (D1 dm (C1 cm repf)) (D1 dm (C1 cm (Array size elem)))
  where
  gTo1 = M1 . M1 . fst . arrayToFieldsFlat @'Z . getArray . unM1 . unM1
  {-# INLINEABLE gTo1 #-}
