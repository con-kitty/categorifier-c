{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- So we can define a boolean ring.
{-# OPTIONS_GHC -Wno-orphans #-}
-- This is easier than doing UNPACK on every field.
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Primitive types for all Kittyhawk code generation systems.
--
-- This module introduces a higher-kinded sum ('Prim') and a higher-kinded product ('Arrays') of all
-- primitive types in 'Primitives'.  These types are useful in almost all code-generation contexts.
--
-- === Note on "primitives"
--
-- The set of primitives that can be translated to C or machine code ('Primitives') is fixed.
-- Changing the set implies rethinking and changing many other parts of the codebase.
--
-- __The machinery in this module is not to be modified without a lot of careful design!__
--
-- === Note on higher kinds
--
-- 'Prim' and 'Arrays' have kind @(Type -> Type) -> Type@ -- the @f@ type argument has kind @Type ->
-- Type@ like a normal list (@[]@) or 'Data.Vector.Vector'.  The @Kitty.HK1@ library exists to make
-- structures with this kind easy to work with safely, via analogues to the standard library classes
-- like 'Functor' and 'Traversable'.  Programs dealing with these higher-kinded types can cause
-- extra trouble when it comes to type inference and type checking.  If you're doing any work with
-- these types, you may want to read through the docs for @Kitty.HK1@ first, both to familiarize
-- yourself with the API (the analogy to standard classes is not perfect) and also to see various
-- notes about type inference and how to work around type errors or restrictions of the interface to
-- higher-kinded operations.
module Kitty.Prim.Base
  ( -- * Primitive types
    Primitives,
    Prim (..),
    Arrays (..),

    -- ** Optics
    AsPrim (..),
    HasArrays (..),

    -- ** Specialized views of primitive types

    -- *** Bare type information
    PrimType,

    -- *** Enhanced type information
    PrimGADT (..),

    -- *** Working with primitive types at the type level
    PrimAny,
    PrimitiveTypes,
    PrimNum,
    PrimitiveNumbers,
    PrimIntegral,
    PrimitiveIntegers,
    PrimSignedIntegral,
    PrimitiveSignedIntegers,
    PrimUnsignedIntegral,
    PrimitiveUnsignedIntegers,
    PrimFractional,
    PrimitiveFractionals,

    -- **** Helpers
    PrimitiveMembership (..),
    PrimitiveType,
    PrimitiveNumber,
    PrimitiveInteger,
    PrimitiveSignedInteger,
    PrimitiveUnsignedInteger,
    PrimitiveFractional,
    ApplyPrimTest,

    -- *** Single primitive values
    PrimVal,

    -- * The 'IsPrimitive' class
    IsPrimitive (..),

    -- ** Primitive optics
    -- $primitiveoptics
    PrimLens (..),
    PrimPrism (..),

    -- * Indexing
    polyIndexArrays,
    polyIndexPrim,

    -- ** Re-exports
    Tagged (..),
    Int8,
    Int16,
    Int32,
    Int64,
    Word8,
    Word16,
    Word32,
    Word64,
  )
where

import qualified Barbies
import qualified Barbies.Constraints as Barbies
import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData (..), NFData1 (..), rnf1)
import Control.Lens (Lens', Prism')
import Control.Lens.TH (makeClassyPrisms, makeLensesWith)
import Data.Data (Data, Typeable, typeRep)
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Show1 (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Data.Hashable (Hashable (..))
import Data.Hashable.Lifted (Hashable1 (..), hashWithSalt1)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Nat (Nat)
import Data.Proxy (Proxy (..))
import Data.Semigroup (All (..))
import Data.Tagged (Tagged (..))
import qualified Data.Type.Nat as Nat
import Data.Vector.Storable (Storable)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import qualified GHC.Generics as Generic
import GHC.TypeLits (KnownSymbol, Symbol)
import Generics.Deriving.Enum (GEnum (..))
import qualified Kitty.Barbies as Barbies
import Kitty.Lens.Rules (kittyLensRules)
import Kitty.Plugin.Client (deriveHasRep)
import qualified Kitty.Show as Show

-- | Primitive types for the 'Kitty.KTypes.K' DSL.  These should behave exactly like the
-- corresponding C type for all purposes.  This structure is dual to 'Arrays', and must contain
-- exactly the same fields.
--
-- The fields in this type must remain ordered identically to those in 'Arrays'.  You will get them
-- in this order if you use the 'GEnum' instance.
data Prim f
  = PrimBool !(f Bool)
  | PrimInt8 !(f Int8)
  | PrimInt16 !(f Int16)
  | PrimInt32 !(f Int32)
  | PrimInt64 !(f Int64)
  | PrimWord8 !(f Word8)
  | PrimWord16 !(f Word16)
  | PrimWord32 !(f Word32)
  | PrimWord64 !(f Word64)
  | PrimFloat !(f Float)
  | PrimDouble !(f Double)
  deriving (Generic)

instance Barbies.FunctorB Prim where
  bmap f = \case
    PrimBool x -> PrimBool (f x)
    PrimInt8 x -> PrimInt8 (f x)
    PrimInt16 x -> PrimInt16 (f x)
    PrimInt32 x -> PrimInt32 (f x)
    PrimInt64 x -> PrimInt64 (f x)
    PrimWord8 x -> PrimWord8 (f x)
    PrimWord16 x -> PrimWord16 (f x)
    PrimWord32 x -> PrimWord32 (f x)
    PrimWord64 x -> PrimWord64 (f x)
    PrimFloat x -> PrimFloat (f x)
    PrimDouble x -> PrimDouble (f x)

instance Barbies.ConstraintsB Prim where
  type
    AllB c Prim =
      ( (((c Bool, c Double), (c Float, c Int8)), ((c Int16, c Int32), (c Int64, c Word8))),
        ((c Word16, c Word32), c Word64)
      )

  baddDicts = \case
    PrimBool x -> PrimBool (Pair Barbies.Dict x)
    PrimInt8 x -> PrimInt8 (Pair Barbies.Dict x)
    PrimInt16 x -> PrimInt16 (Pair Barbies.Dict x)
    PrimInt32 x -> PrimInt32 (Pair Barbies.Dict x)
    PrimInt64 x -> PrimInt64 (Pair Barbies.Dict x)
    PrimWord8 x -> PrimWord8 (Pair Barbies.Dict x)
    PrimWord16 x -> PrimWord16 (Pair Barbies.Dict x)
    PrimWord32 x -> PrimWord32 (Pair Barbies.Dict x)
    PrimWord64 x -> PrimWord64 (Pair Barbies.Dict x)
    PrimFloat x -> PrimFloat (Pair Barbies.Dict x)
    PrimDouble x -> PrimDouble (Pair Barbies.Dict x)

instance Barbies.TraversableB Prim

-- | __TODO__: This instance is hand-written rather than using Generics, because SW-3114.
deriveHasRep ''Prim

makeClassyPrisms ''Prim

-- | Values of this type only represent information about which primitive type is present; there is
-- no primitive value associated.
type PrimType = Prim Proxy

deriving instance Eq PrimType

deriving instance Data PrimType

deriving instance Read PrimType

deriving instance Ord PrimType

instance Hashable PrimType

instance Serialise PrimType

instance GEnum PrimType

-- | A single value of some primitive type.
type PrimVal = Prim Identity

deriving instance Eq PrimVal

deriving instance Data PrimVal

deriving instance Read PrimVal

deriving instance Ord PrimVal

instance Hashable PrimVal

instance Serialise PrimVal

-- | This instance is done with @QuantifiedConstraints@ in order to allow showing values of type
-- @`Prim` `Kitty.KTypes.C.C`@ and so on.
deriving instance (forall a. Show a => Show (f a)) => Show (Prim f)

-- | A product of all primitive types in the 'Kitty.PrimTypes.K' DSL.
--
-- This structure is dual to 'Prim' and must contain exactly the same fields.  The fields in this
-- type must remain ordered identically to those in 'Prim'.
data Arrays f = Arrays
  { arrayBool :: !(f Bool),
    arrayInt8 :: !(f Int8),
    arrayInt16 :: !(f Int16),
    arrayInt32 :: !(f Int32),
    arrayInt64 :: !(f Int64),
    arrayWord8 :: !(f Word8),
    arrayWord16 :: !(f Word16),
    arrayWord32 :: !(f Word32),
    arrayWord64 :: !(f Word64),
    arrayFloat :: !(f Float),
    arrayDouble :: !(f Double)
  }
  deriving (Generic)

deriving instance (Typeable f, forall a. Data a => Data (f a)) => Data (Arrays f)

instance (forall a. Serialise a => Serialise (f a)) => Serialise (Arrays f)

instance Barbies.FunctorB Arrays where
  bmap f (Arrays b i8 i16 i32 i64 w8 w16 w32 w64 fl d) =
    Arrays (f b) (f i8) (f i16) (f i32) (f i64) (f w8) (f w16) (f w32) (f w64) (f fl) (f d)

-- | Manual instance for Kitty.Cat performance. This should be eliminated when we replace PolyVec
--   with a C struct-based calling convention.
instance Barbies.ConstraintsB Arrays where
  type
    AllB c Arrays =
      ( (((c Bool, c Double), (c Float, c Int8)), ((c Int16, c Int32), (c Int64, c Word8))),
        ((c Word16, c Word32), c Word64)
      )

  baddDicts (Arrays b i8 i16 i32 i64 w8 w16 w32 w64 f d) =
    Arrays
      (Pair Barbies.Dict b)
      (Pair Barbies.Dict i8)
      (Pair Barbies.Dict i16)
      (Pair Barbies.Dict i32)
      (Pair Barbies.Dict i64)
      (Pair Barbies.Dict w8)
      (Pair Barbies.Dict w16)
      (Pair Barbies.Dict w32)
      (Pair Barbies.Dict w64)
      (Pair Barbies.Dict f)
      (Pair Barbies.Dict d)

instance Barbies.TraversableB Arrays

instance Barbies.ApplicativeB Arrays

instance Barbies.DistributiveB Arrays

deriveHasRep ''Arrays

makeLensesWith kittyLensRules ''Arrays

-- | A boolean ring. `+` = xor and `*` = `&&`.
--
--   Unfortunately omitted from base.
instance Num Bool where
  x + y = (not x && y) || (x && not y)
  (*) = (&&)
  abs = id
  signum = id
  fromInteger = \case
    0 -> False
    _ -> True
  negate = not

-- | Instances of this class are code-generation primitives like numbers and booleans.
--
-- The class constraints serve to inform downstream code about the set of common operations
-- available for processing primitive values of all types (such as 'Show'ing, comparing for
-- 'Eq'uality or getting a 'Data.Typeable.TypeRep').
class
  ( Storable a,
    Show a,
    Read a,
    Eq a,
    Num a,
    Ord a,
    Data a,
    Typeable a,
    NFData a,
    Hashable a,
    Serialise a,
    Nat.SNatI (PrimSizeBytes a),
    KnownSymbol (PrimCName a),
    PrimPrism a,
    PrimLens a
    -- It's OK to add more constraints here as long as they apply to all the types given instances
    -- below.  This lets us do more things inside 'Barbies.bmap' and 'traverseHK1' and so on.
  ) =>
  IsPrimitive a
  where
  -- | Return the 'PrimType' corresponding to @a@.  This is probably best used with
  -- @-XTypeApplications@; for example, 'Kitty.Prim.Types.primTypeProxy' is defined as
  --
  -- > primTypeProxy :: forall a. IsPrimitive a => Proxy a -> PrimType
  -- > primTypeProxy _ = unTagged $ primType @a
  primType :: Tagged a PrimType

  -- | An enhanced version of 'PrimType', carrying more type information.
  primGADT :: PrimGADT a

  -- | Wrap the input in the appropriate 'Prim' constructor for the type @a@.
  toPrim :: f a -> Prim f

  -- | The size in memory of the primitive, in bytes.
  type PrimSizeBytes a :: Nat

  -- | The C name of a primitive type.  (To get the Haskell name, use the 'Data.Typeable.TypeRep' or
  -- use 'Kitty.Prim.Types.getPrimHaskellName'.)
  type PrimCName a :: Symbol

instance IsPrimitive Bool where
  primType = Tagged $ PrimBool Proxy

  primGADT = GBool

  toPrim = PrimBool

  type PrimSizeBytes Bool = Nat.FromGHC 1

  type PrimCName Bool = "bool"

  {-# INLINEABLE primType #-}

  {-# INLINEABLE toPrim #-}

instance IsPrimitive Int8 where
  primType = Tagged $ PrimInt8 Proxy

  primGADT = GInt8

  toPrim = PrimInt8

  type PrimSizeBytes Int8 = Nat.FromGHC 1

  type PrimCName Int8 = "int8_t"

  {-# INLINEABLE primType #-}

  {-# INLINEABLE toPrim #-}

instance IsPrimitive Int16 where
  primType = Tagged $ PrimInt16 Proxy

  primGADT = GInt16

  toPrim = PrimInt16

  type PrimSizeBytes Int16 = Nat.FromGHC 2

  type PrimCName Int16 = "int16_t"

  {-# INLINEABLE primType #-}

  {-# INLINEABLE toPrim #-}

instance IsPrimitive Int32 where
  primType = Tagged $ PrimInt32 Proxy

  primGADT = GInt32

  toPrim = PrimInt32

  type PrimSizeBytes Int32 = Nat.FromGHC 4

  type PrimCName Int32 = "int32_t"

  {-# INLINEABLE primType #-}

  {-# INLINEABLE toPrim #-}

instance IsPrimitive Int64 where
  primType = Tagged $ PrimInt64 Proxy

  primGADT = GInt64

  toPrim = PrimInt64

  type PrimSizeBytes Int64 = Nat.FromGHC 8

  type PrimCName Int64 = "int64_t"

  {-# INLINEABLE primType #-}

  {-# INLINEABLE toPrim #-}

instance IsPrimitive Word8 where
  primType = Tagged $ PrimWord8 Proxy

  primGADT = GWord8

  toPrim = PrimWord8

  type PrimSizeBytes Word8 = Nat.FromGHC 1

  type PrimCName Word8 = "uint8_t"

  {-# INLINEABLE primType #-}

  {-# INLINEABLE toPrim #-}

instance IsPrimitive Word16 where
  primType = Tagged $ PrimWord16 Proxy

  primGADT = GWord16

  toPrim = PrimWord16

  type PrimSizeBytes Word16 = Nat.FromGHC 2

  type PrimCName Word16 = "uint16_t"

  {-# INLINEABLE primType #-}

  {-# INLINEABLE toPrim #-}

instance IsPrimitive Word32 where
  primType = Tagged $ PrimWord32 Proxy

  primGADT = GWord32

  toPrim = PrimWord32

  type PrimSizeBytes Word32 = Nat.FromGHC 4

  type PrimCName Word32 = "uint32_t"

  {-# INLINEABLE primType #-}

  {-# INLINEABLE toPrim #-}

instance IsPrimitive Word64 where
  primType = Tagged $ PrimWord64 Proxy

  primGADT = GWord64

  toPrim = PrimWord64

  type PrimSizeBytes Word64 = Nat.FromGHC 8

  type PrimCName Word64 = "uint64_t"

  {-# INLINEABLE primType #-}

  {-# INLINEABLE toPrim #-}

instance IsPrimitive Float where
  primType = Tagged $ PrimFloat Proxy

  primGADT = GFloat

  toPrim = PrimFloat

  type PrimSizeBytes Float = Nat.FromGHC 4

  type PrimCName Float = "float"

  {-# INLINEABLE primType #-}

  {-# INLINEABLE toPrim #-}

instance IsPrimitive Double where
  primType = Tagged $ PrimDouble Proxy

  primGADT = GDouble

  toPrim = PrimDouble

  type PrimSizeBytes Double = Nat.FromGHC 8

  type PrimCName Double = "double"

  {-# INLINEABLE primType #-}

  {-# INLINEABLE toPrim #-}

-- $primitiveoptics
--
-- The 'PrimLens' and 'PrimPrism' classes provide optics for working with collections of primitive
-- types.
--
-- === Optics available
--
-- You cannot have a 'Lens'' for 'Prim'; 'Control.Lens.Type.Lens'es are for product types.
-- Similarly, you cannot have a 'Prism'' for 'Arrays', as 'Control.Lens.Type.Prism's are for sum
-- types.  If you find yourself wanting a different type of optic than what's provided, please have
-- a look at the 'Control.Lens.Type.Lens' laws and the 'Control.Lens.Type.Prism' laws and let me
-- know what law-abiding accessors you would like instead.
--
-- === Note on optic polymorphism and inference
--
-- These optics are extremely polymorphic, and GHC may have a hard time inferring all the right
-- types in some cases (e.g. when working inside a higher-kinded helper function).  When in doubt or
-- under attack by the type checker, try specifying the primitive base type @a@ with an explicit
-- type application; for example, if you are inside a polymorphic function operating on all base
-- types @a@, you can write things like the following:
--
-- > view (prims_ @a) foo
--
-- > review (_asPrims @a) val
--
-- See below for examples with specific types.
--
-- === Scope of provided optics
--
-- The provided optics only address the 'Arrays' or 'Prim' part of the problem.  If your value
-- contains vectors of primitive values, you will get a 'Lens'' or a 'Prism'' that accesses the
-- vector itself.  If you want to operate on all the primitive values inside the vector, you must
-- take an additional step.  For example, the following expression increments all the 'Int64'
-- primitive values in @v@ by composing 'prims_' with 'traverse':
--
-- > over (prims_ @Int64 . traverse) (+1) v
--
-- > :t over (prims_ @Int64 . traverse) (+1)
-- > over (prims_ @Int64 . traverse) (+1) :: (HasArrays t f, Traversable f) => t -> t
--
-- ==== 'Arrays' helpers
--
-- Most of the helper types like 'Kitty.Prim.ArrayCount.ArrayCount' or
-- 'Kitty.Prim.Compose.Compose' provide 'Control.Lens.Type.Iso's
-- ('Kitty.Prim.ArrayCount._ArrayCount'; 'Kitty.Prim.Compose._Compose') and
-- 'Control.Lens.Wrapped.Wrapped' instances for use in conjunction with 'prims_' or '_asPrims', so
-- you can do things like
--
-- > over (prims_ @Bool . _ArrayCount) (+1) :: Arrays ArrayCount -> Arrays ArrayCount
--
-- > view (prims_ @Double . _Wrapped) (v :: Arrays ArrayCount) :: Int
--
-- Similarly, 'Control.Lens.At.Ixed' support is provided for 'Kitty.Prim.Compose.Compose', so you
-- can form optics like
--
-- > view (prims_ @Bool . ix 22)

class PrimLens a where
  -- | Access the primitive elements of @c@ with base type @a@.  This is a 'Lens'' because instances
  -- of this are like 'Arrays' and always have some corresponding element.
  prims_ :: Lens' (Arrays f) (f a)

class PrimPrism a where
  -- | Access the primitive elements of @c@ with base type @a@.  This is a 'Prism'' because
  -- instances of this are like 'Prim' and don't necessarily contain values with base type @a@.
  _asPrims :: Prism' (Prim f) (f a)

instance (forall p. IsPrimitive p => NFData (f p)) => NFData (Prim f)

instance (forall a. (IsPrimitive a => Semigroup (v a))) => Semigroup (Arrays v) where
  (<>) = Barbies.bzipWithC @IsPrimitive (<>)
  {-# INLINEABLE (<>) #-}

instance (forall a. (IsPrimitive a => Monoid (v a))) => Monoid (Arrays v) where
  mempty = Barbies.bpureC @IsPrimitive mempty
  {-# INLINEABLE mempty #-}

instance PrimLens Bool where
  prims_ = arrayBool_
  {-# INLINEABLE prims_ #-}

instance PrimLens Int8 where
  prims_ = arrayInt8_
  {-# INLINEABLE prims_ #-}

instance PrimLens Int16 where
  prims_ = arrayInt16_
  {-# INLINEABLE prims_ #-}

instance PrimLens Int32 where
  prims_ = arrayInt32_
  {-# INLINEABLE prims_ #-}

instance PrimLens Int64 where
  prims_ = arrayInt64_
  {-# INLINEABLE prims_ #-}

instance PrimLens Word8 where
  prims_ = arrayWord8_
  {-# INLINEABLE prims_ #-}

instance PrimLens Word16 where
  prims_ = arrayWord16_
  {-# INLINEABLE prims_ #-}

instance PrimLens Word32 where
  prims_ = arrayWord32_
  {-# INLINEABLE prims_ #-}

instance PrimLens Word64 where
  prims_ = arrayWord64_
  {-# INLINEABLE prims_ #-}

instance PrimLens Float where
  prims_ = arrayFloat_
  {-# INLINEABLE prims_ #-}

instance PrimLens Double where
  prims_ = arrayDouble_
  {-# INLINEABLE prims_ #-}

instance PrimPrism Bool where
  _asPrims = _PrimBool
  {-# INLINEABLE _asPrims #-}

instance PrimPrism Int8 where
  _asPrims = _PrimInt8
  {-# INLINEABLE _asPrims #-}

instance PrimPrism Int16 where
  _asPrims = _PrimInt16
  {-# INLINEABLE _asPrims #-}

instance PrimPrism Int32 where
  _asPrims = _PrimInt32
  {-# INLINEABLE _asPrims #-}

instance PrimPrism Int64 where
  _asPrims = _PrimInt64
  {-# INLINEABLE _asPrims #-}

instance PrimPrism Word8 where
  _asPrims = _PrimWord8
  {-# INLINEABLE _asPrims #-}

instance PrimPrism Word16 where
  _asPrims = _PrimWord16
  {-# INLINEABLE _asPrims #-}

instance PrimPrism Word32 where
  _asPrims = _PrimWord32
  {-# INLINEABLE _asPrims #-}

instance PrimPrism Word64 where
  _asPrims = _PrimWord64
  {-# INLINEABLE _asPrims #-}

instance PrimPrism Float where
  _asPrims = _PrimFloat
  {-# INLINEABLE _asPrims #-}

instance PrimPrism Double where
  _asPrims = _PrimDouble
  {-# INLINEABLE _asPrims #-}

data PrimGADT a where
  GBool :: PrimGADT Bool
  GInt8 :: PrimGADT Int8
  GInt16 :: PrimGADT Int16
  GInt32 :: PrimGADT Int32
  GInt64 :: PrimGADT Int64
  GWord8 :: PrimGADT Word8
  GWord16 :: PrimGADT Word16
  GWord32 :: PrimGADT Word32
  GWord64 :: PrimGADT Word64
  GFloat :: PrimGADT Float
  GDouble :: PrimGADT Double

deriving instance Eq (PrimGADT a)

deriving instance Show (PrimGADT a)

data PrimitiveMembership a b = Expected a | Encountered b

-- | This is a customized version of `Kitty.TypeUtils.Lists.IsElem` which lets us give slightly more
-- helpful error messages.  We can't do the obvious thing due to
-- https://gitlab.haskell.org/ghc/ghc/-/issues/18766.
type family PrimTest' k (t :: k) (f :: k) (a :: Type) (xs :: [Type]) where
  PrimTest' _ t _ a (a ': moar) = t
  PrimTest' k t f a (_ ': moar) = PrimTest' k t f a moar
  PrimTest' _ _ f _ '[] = f

type ApplyPrimTest (e :: k) a xs =
  PrimTest' (PrimitiveMembership k Type) ('Expected e) ('Encountered a) a xs

type PrimTest (e :: k) a xs = 'Expected e ~ ApplyPrimTest e a xs

-- | The set of all primitive types.
type PrimitiveTypes =
  [ Bool,
    Int8,
    Int16,
    Int32,
    Int64,
    Word8,
    Word16,
    Word32,
    Word64,
    Float,
    Double
  ]

type Primitives = PrimitiveTypes

-- | Type-level token for membership in `PrimitiveTypes`.  Exists only so its name can be rendered
-- in error messages.
data PrimitiveType

-- | This constraint is like `IsPrimitive`, but it also limits @a@ to a closed set of types.
type PrimAny a = (IsPrimitive a, PrimTest PrimitiveType a Primitives)

-- | The set of all primitive number types.
type PrimitiveNumbers = '[Int8, Int16, Int32, Int64, Word8, Word16, Word32, Word64, Float, Double]

data PrimitiveNumber

type PrimNum a = (PrimAny a, Num a, PrimTest PrimitiveNumber a PrimitiveNumbers)

-- | The set of all primitive integral types.
type PrimitiveIntegers = '[Int8, Int16, Int32, Int64, Word8, Word16, Word32, Word64]

data PrimitiveInteger

-- | Need to include 'PrimNum' here, because GHC can't, for example, deduce
-- @IsElem a '[Int, Bool]@ from @IsElem a '[Int]@.
type PrimIntegral a =
  ( PrimAny a,
    PrimNum a,
    Integral a,
    PrimTest PrimitiveInteger a PrimitiveIntegers
  )

-- | The set of all signed primitive integer types.
type PrimitiveSignedIntegers = '[Int8, Int16, Int32, Int64]

data PrimitiveSignedInteger

type PrimSignedIntegral a =
  ( PrimAny a,
    PrimNum a,
    PrimIntegral a,
    Integral a,
    PrimTest PrimitiveSignedInteger a PrimitiveSignedIntegers
  )

-- | The set of all unsigned primitive integer types.
type PrimitiveUnsignedIntegers = '[Word8, Word16, Word32, Word64]

data PrimitiveUnsignedInteger

type PrimUnsignedIntegral a =
  ( PrimAny a,
    PrimNum a,
    PrimIntegral a,
    Integral a,
    PrimTest PrimitiveUnsignedInteger a PrimitiveUnsignedIntegers
  )

-- | The set of all fractional (floating-point) primitive types.
type PrimitiveFractionals = '[Float, Double]

data PrimitiveFractional

type PrimFractional a =
  ( PrimAny a,
    PrimNum a,
    Fractional a,
    PrimTest PrimitiveFractional a PrimitiveFractionals
  )

instance Eq1 f => Eq (Arrays f) where
  x == y = getAll . Barbies.bfoldMapC @IsPrimitive go $ Barbies.bzip x y
    where
      go (Pair a b) = All $ liftEq (==) a b
  {-# INLINEABLE (==) #-}

instance Ord1 f => Ord (Arrays f) where
  x `compare` y = Barbies.bfoldMapC @IsPrimitive go $ Barbies.bzip x y
    where
      go (Pair a b) = liftCompare compare a b
  {-# INLINEABLE compare #-}

instance Hashable1 f => Hashable (Arrays f) where
  hashWithSalt salt = Barbies.bfoldlC @Hashable hashWithSalt1 salt
  {-# INLINEABLE hashWithSalt #-}

instance Show1 f => Show (Arrays f) where
  showsPrec _ = Show.record "Arrays" . Barbies.bfoldMapC @IsPrimitive lsp
    where
      lsp :: forall a. (Show a, Typeable a) => f a -> [(String, ShowS)]
      lsp v =
        let typeName = show $ typeRep (Proxy @a)
         in [("array" <> typeName, liftShowsPrec showsPrec showList 11 v)]

instance NFData1 f => NFData (Arrays f) where
  rnf = Barbies.bfoldlC @NFData (\_ fa -> rnf1 fa) ()
  {-# INLINEABLE rnf #-}

-- | @'polyIndexArrays' f a p@ uses the function @f@ to turn the field of @a@ corresponding to the
-- given value of @p@ into a value of the uniform return type.  It allows you to index some 'Arrays'
-- by a 'Prim'; if you pass it 'PrimBool', you will get out the 'arrayBool' field of the 'Arrays'.
--
-- The twist is that each field of 'Arrays' has a different base type, and Haskell does not allow
-- the return type of the function to depend on the value of the 'Prim' constructor.  To work
-- around this, you must supply a reduction function @f@ which reduces any value you like into a
-- fixed return type.
--
-- === Math
--
-- This function is related to the adjunction between 'Prim' and 'Arrays'.  Making this adjunction
-- precise in the higher-kinded (@HK1@) setting is a bit of a headache in Haskell, so this limited
-- version is all you get for now.
polyIndexArrays ::
  forall c f g b.
  Barbies.AllB c Arrays =>
  -- | Reduction function @f@
  (forall a. c a => g a -> f a -> b) ->
  -- | 'Arrays' @a@ to be indexed
  Arrays f ->
  -- | Index value @p@
  Prim g ->
  -- | Return type
  b
polyIndexArrays f arrs (PrimBool x) = f x $ arrayBool arrs
polyIndexArrays f arrs (PrimInt8 x) = f x $ arrayInt8 arrs
polyIndexArrays f arrs (PrimInt16 x) = f x $ arrayInt16 arrs
polyIndexArrays f arrs (PrimInt32 x) = f x $ arrayInt32 arrs
polyIndexArrays f arrs (PrimInt64 x) = f x $ arrayInt64 arrs
polyIndexArrays f arrs (PrimWord8 x) = f x $ arrayWord8 arrs
polyIndexArrays f arrs (PrimWord16 x) = f x $ arrayWord16 arrs
polyIndexArrays f arrs (PrimWord32 x) = f x $ arrayWord32 arrs
polyIndexArrays f arrs (PrimWord64 x) = f x $ arrayWord64 arrs
polyIndexArrays f arrs (PrimFloat x) = f x $ arrayFloat arrs
polyIndexArrays f arrs (PrimDouble x) = f x $ arrayDouble arrs
{-# INLINEABLE polyIndexArrays #-}

-- | @'polyIndexPrim' f p@ uses the function @f@ to turn the value contained in the provided 'Prim'
-- (@p@) into a result of type @b@.  This function must necessarily be polymorphic and handle any
-- possible value of type @'Prim' f@.
polyIndexPrim :: forall c f b. Barbies.AllB c Arrays => (forall a. c a => f a -> b) -> Prim f -> b
polyIndexPrim f = polyIndexArrays @c @Proxy (const . f) (Barbies.bpureC @c Proxy)
{-# INLINEABLE polyIndexPrim #-}
