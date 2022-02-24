{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A toolkit like "GHC.Generics" that is aware of C language representation issues.
--
-- For an example of how to use this toolkit to operate generically on Haskell types while
-- respecting their C representation constraints, see "Categorifier.C.CTypes.CGeneric.CxxType".  For examples
-- of how to imbue your data types with a `CGeneric` representation, see
-- "Categorifier.C.CTypes.CGeneric.Test".
--
-- This module is intended to be imported qualified, e.g. as @CG@.
module Categorifier.C.CTypes.CGeneric.Class
  ( -- * Generic representation of types that can be generated to C code
    CGeneric (..),

    -- ** Primitive types
    Prim (..),

    -- ** Bitfields
    Bitfield (..),

    -- ** Arrays
    Array (..),
    ArraySize,
    GArrayType,

    -- ** Enumerations
    Enum (..),
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

import qualified Barbies
import Categorifier.C.CExpr.Cat.TargetOb (TargetOb, TargetObTC1)
import qualified Categorifier.C.KTypes.KBits as KBits
import Categorifier.C.Nat.Operators (type (+), type (-))
import Data.Bifunctor (second)
import qualified Data.Bits as Bits
import Data.Bool (bool)
import Data.Functor.Compose (Compose)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Type.Nat (Nat (..))
import qualified Data.Type.Nat as Nat
import Data.Vec.Lazy (Vec (..))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
  ( C1,
    D1,
    FixityI (PrefixI),
    Generic,
    K1 (..),
    M1 (..),
    Meta (..),
    Par1 (..),
    Rec0,
    Rec1 (..),
    S1,
    U1 (..),
    (:*:) (..),
    (:+:) (..),
  )
import qualified GHC.Generics as G
import GHC.TypeLits (Symbol)
import Prelude hiding (Enum)

-- | The "C-generics" representation of a data type.
--
-- `CGeneric` parallels `Generic` closely, but it differs according to how the types are represented
-- in C.  Special wrapper types can appear in a `Rep` so that the value-level representation of
-- whatever is being wrapped corresponds more closely to how it looks in the corresponding C code.
-- This means that when you write some class to process a `Rep`, in addition to the usual
-- `Generic` stuff, you can match on these wrapper types to handle these special situations.  The
-- wrappers are as follows:
--
--   [`Bitfield`] wraps a product of (potentially wrapped) `Bool`s, representing them as an
--   unsigned integer like in C
--
--   [`Enum`] wraps an enum (sum of nullary constructors), representing them as a `Word8` (C
--   @uint8_t@).
--
--   [`Array`] replaces a product where each field has the same type, representing them as a
--   length-indexed sequence, which corresponds to a C array rather than a struct.
--
--   [`Prim`] wraps a (potentially wrapped) C primitive value.  The primitive value is still
--   stored; this wrapper merely allows writing a `CGeneric` instance for such types.
--
-- == Graphical overview for instance-writers
--
-- Ignoring `GHC.Generics.V1`, which is not relevant for our purposes, the tree of possible types in
-- a GHC-produced `Rep` looks approximately like this:
--
-- @
--          `D1`
--           |
--       +---+---+
--       |       |
--       `C1`    X`:+:`Y
--       |
--   +---+--+-------+
--   |      |       |
--   `U1`     `S1`    W`:*:`Z
--          |
--          `K1`
-- @
--
-- where @W@, @X@, @Y@ and @Z@ stand for any types in the same row of the diagram (e.g. a `:+:` can
-- have a `C1` or another `:+:` on either side).  The tree of possible types in a `Rep` produced
-- by the system in this module, by contrast, looks approximately like
--
-- @
--          `D1`
--           |
--       +---+----+-------+---------+
--       |        |       |         |
--       `C1`    `Enum`     `Prim`      X`:+:`Y
--       |
--   +---+--+-------+-------+---------+
--   |      |       |       |         |
--   `U1`     `S1`    W`:*:`Z    `Array`    `Bitfield`
--          |
--          `K1`
-- @
--
-- where `Enum` always wraps a `:+:` and `Bitfield` always wraps an `S1` or a `:*:`.  `Array`
-- doesn't wrap anything but a size and the element type.  `Prim` always wraps some flavor of
-- primitive type.
--
-- == Generic deriving
--
-- For types which have a `Generic` instance and do not contain bitfields or C arrays (enums are
-- fine), deriving /should/ happen automatically; for example, for the type
--
-- > data MyType f = MyType {
-- >   fieldA :: f Bool,
-- >   fieldB :: f Double,
-- >   fieldC :: Switches f
-- >   } deriving (Generic)
--
-- you should just need
--
-- > instance CGeneric (MyType arg)
--
-- A more modern and concise way to write this is via the @DeriveAnyClass@ extension:
--
-- > {-# LANGUAGE DeriveAnyClass #-}
-- > data MyType f = {
-- >   fieldA :: f Bool,
-- >   fieldB :: f Double,
-- >   fieldC :: Switches f
-- >   } deriving (Generic, CGeneric)
--
-- See the documentation on the individual wrapper types to understand how to manually guide GHC to
-- produce the instance you want.
--
-- == Support for sum types
--
-- `CGeneric` has limited support for sum types.  `Either` and `Maybe` instances are provided by
-- this module.  Consumers of `Rep`s must attempt to handle such sums in a sensible way.  However,
-- sum type support in the code generation system is limited and ad-hoc, so you should beware of
-- breakage if you attempt to use other sum types with `CGeneric`.  We make no guarantees.
class CGeneric a where
  type Rep a :: Type -> Type
  type Rep a = GRep a

  -- | Convert from a Haskell type to the corresponding generic C-aware representation.
  from :: a -> Rep a ()
  default from :: (Generic a, CanGenericFrom a) => a -> Rep a ()
  from = gFrom . G.from
  {-# INLINEABLE from #-}

  -- | Convert from a generic C-aware representation to the corresponding Haskell type.
  to :: Rep a () -> a
  default to :: (Generic a, CanGenericTo a) => Rep a () -> a
  to = G.to . gTo
  {-# INLINEABLE to #-}

type CanGenericFrom a = GFrom (GIsEnum (G.Rep a)) (G.Rep a) (Rep a)

type CanGenericTo a = GTo (GIsEnum (G.Rep a)) (G.Rep a) (Rep a)

{- Generic `from` -}

-- | Note that in general, structurally similar parts of the `G.Rep` and the corresponding `Rep` may
-- differ at the type level.  One instance of this occurs when we have manually assigned field names
-- to a struct by using `WithFieldNames` -- only the selector metadata (`MetaSel`) is different at
-- the type level.
class
  ( enum ~ GIsEnum rep
  ) =>
  GFrom enum rep cgrep
  where
  gFrom :: rep x -> cgrep x

-- | This is the top-level pass-through instance; it works in all cases.
instance GFrom enum x cx => GFrom enum (D1 m x) (D1 m cx) where
  gFrom (M1 x) = M1 $ gFrom x
  {-# INLINEABLE gFrom #-}

-- | This instance is for normal products.  It has to match all the way down to the :*: constructor
-- in order to disambiguate from the wrapped types.
instance
  ( NotEnum (C1 m (a :*: b)),
    GFrom 'False (a :*: b) (ca :*: cb)
  ) =>
  GFrom 'False (C1 m (a :*: b)) (C1 m (ca :*: cb))
  where
  gFrom (M1 x) = M1 $ gFrom x
  {-# INLINEABLE gFrom #-}

instance
  ( (singleFieldRep :: Type -> Type) ~ S1 ms (Rec0 a),
    cFieldRep ~ S1 cms (Rec0 a),
    NotEnum singleFieldRep,
    GFrom 'False singleFieldRep (S1 cms (Rec0 a))
  ) =>
  GFrom 'False (C1 mc (S1 ms (Rec0 a) :: Type -> Type)) (C1 mc (S1 cms (Rec0 a)))
  where
  gFrom (M1 x) = M1 $ gFrom x
  {-# INLINEABLE gFrom #-}

instance
  ( NotEnum rep,
    rep ~ (sx :*: sy),
    GFrom (GIsEnum sx) sx csx,
    GFrom (GIsEnum sy) sy csy
  ) =>
  GFrom 'False (sx :*: sy) (csx :*: csy)
  where
  gFrom (sx :*: sy) = gFrom sx :*: gFrom sy
  {-# INLINEABLE gFrom #-}

instance
  ( NotEnum rep,
    rep ~ (sx :+: sy),
    GFrom (GIsEnum sx) sx csx,
    GFrom (GIsEnum sy) sy csy
  ) =>
  GFrom 'False (sx :+: sy) (csx :+: csy)
  where
  gFrom (L1 sx) = L1 $ gFrom sx
  gFrom (R1 sy) = R1 $ gFrom sy
  {-# INLINEABLE gFrom #-}

-- | This is the instance where the `G.Rep` provides the field name -- it must match.
instance
  ( rep ~ S1 ms (Rec0 a),
    ms ~ 'MetaSel ('Just name) u s d,
    NotEnum rep
  ) =>
  GFrom
    'False
    (S1 ('MetaSel ('Just name) u s d) (Rec0 a))
    (S1 ('MetaSel ('Just name) u s d) (Rec0 a))
  where
  gFrom (M1 (K1 a)) = M1 $ K1 a
  {-# INLINEABLE gFrom #-}

-- | This is the instance where we have manually provided a field name, e.g. `V3`.
instance
  ( rep ~ S1 ms (Rec0 a),
    ms ~ 'MetaSel 'Nothing u s d,
    NotEnum rep
  ) =>
  GFrom
    'False
    (S1 ('MetaSel 'Nothing u s d) (Rec0 a))
    (S1 ('MetaSel ('Just name) u s d) (Rec0 a))
  where
  gFrom (M1 (K1 a)) = M1 $ K1 a
  {-# INLINEABLE gFrom #-}

-- | This is the instance where we do not want a field name, e.g. a tuple.
instance
  ( rep ~ S1 ms (Rec0 a),
    ms ~ 'MetaSel 'Nothing u s d,
    NotEnum rep
  ) =>
  GFrom
    'False
    (S1 ('MetaSel 'Nothing u s d) (Rec0 a))
    (S1 ('MetaSel 'Nothing u s d) (Rec0 a))
  where
  gFrom (M1 (K1 a)) = M1 $ K1 a
  {-# INLINEABLE gFrom #-}

{- Generic `to` -}

class (enum ~ GIsEnum rep) => GTo enum rep cgrep where
  gTo :: forall (x :: Type). cgrep x -> rep x

-- | This is the top-level pass-through instance; it works in all cases.
instance GTo enum x cx => GTo enum (D1 m x) (D1 m cx) where
  gTo (M1 x) = M1 $ gTo x
  {-# INLINEABLE gTo #-}

instance
  ( NotEnum (C1 m (a :*: b)),
    GTo 'False (a :*: b) (ca :*: cb)
  ) =>
  GTo 'False (C1 m (a :*: b)) (C1 m (ca :*: cb))
  where
  gTo (M1 x) = M1 $ gTo x
  {-# INLINEABLE gTo #-}

instance
  ( singleFieldRep ~ S1 ms (Rec0 a),
    cFieldRep ~ S1 cms (Rec0 a),
    NotEnum singleFieldRep,
    GTo 'False singleFieldRep cFieldRep
  ) =>
  GTo 'False (C1 mc (S1 ms (Rec0 a))) (C1 mc (S1 cms (Rec0 a)))
  where
  gTo (M1 x) = M1 $ gTo x
  {-# INLINEABLE gTo #-}

instance
  ( NotEnum rep,
    rep ~ (sx :*: sy),
    GTo (GIsEnum sx) sx csx,
    GTo (GIsEnum sy) sy csy
  ) =>
  GTo 'False (sx :*: sy) (csx :*: csy)
  where
  gTo (csx :*: csy) = gTo csx :*: gTo csy
  {-# INLINEABLE gTo #-}

instance
  ( NotEnum rep,
    rep ~ (sx :+: sy),
    GTo 'False sx csx,
    GTo 'False sy csy
  ) =>
  GTo 'False (sx :+: sy) (csx :+: csy)
  where
  gTo (L1 sx) = L1 $ gTo sx
  gTo (R1 sy) = R1 $ gTo sy
  {-# INLINEABLE gTo #-}

-- | This is the instance where the `G.Rep` has provided a field name -- they must match.
instance
  ( rep ~ S1 ms (Rec0 a),
    ms ~ 'MetaSel ('Just name) u s d,
    NotEnum rep
  ) =>
  GTo
    'False
    (S1 ('MetaSel ('Just name) u s d) (Rec0 a))
    (S1 ('MetaSel ('Just name) u s d) (Rec0 a))
  where
  gTo (M1 (K1 a)) = M1 $ K1 a
  {-# INLINEABLE gTo #-}

-- | This is the instance where we have manually provided a field name
instance
  ( rep ~ S1 ms (Rec0 a),
    ms ~ 'MetaSel 'Nothing u s d,
    NotEnum rep
  ) =>
  GTo
    'False
    (S1 ('MetaSel 'Nothing u s d) (Rec0 a))
    (S1 ('MetaSel ('Just name) u s d) (Rec0 a))
  where
  gTo (M1 (K1 a)) = M1 $ K1 a
  {-# INLINEABLE gTo #-}

-- | This is the instance where we do not want a field name
instance
  ( rep ~ S1 ms (Rec0 a),
    ms ~ 'MetaSel 'Nothing u s d,
    NotEnum rep
  ) =>
  GTo
    'False
    (S1 ('MetaSel 'Nothing u s d) (Rec0 a))
    (S1 ('MetaSel 'Nothing u s d) (Rec0 a))
  where
  gTo (M1 (K1 a)) = M1 $ K1 a
  {-# INLINEABLE gTo #-}

-- | This type wraps a product of fields of type `Bool`.  See `Categorifier.C.CTypes.CGeneric.RepBitfield`
-- for information on how to write an instance which uses this representation for such products.
-- For the following example type:
--
-- > data Switches f = Switches
-- >   { switchA :: f Bool,
-- >     switchB :: f Bool,
-- >     switchC :: f Bool
-- >   } deriving (Generic)
--
-- the GHC-derived `Rep` is
--
-- > forall f. Rep (Switches f) :: * -> *
-- > = D1
-- >   ('MetaData "Switches" "Categorifier.C.CTypes.CGeneric" "main" 'False)
-- >   (C1
-- >     ('MetaCons "Switches" 'GHC.Generics.PrefixI 'True)
-- >     (S1
-- >       ('MetaSel
-- >         ('Just "switchA")
-- >         'GHC.Generics.NoSourceUnpackedness
-- >         'GHC.Generics.NoSourceStrictness
-- >         'GHC.Generics.DecidedLazy)
-- >       (Rec0 (f Bool))
-- >       :*: (S1
-- >             ('MetaSel
-- >               ('Just "switchB")
-- >               'GHC.Generics.NoSourceUnpackedness
-- >               'GHC.Generics.NoSourceStrictness
-- >               'GHC.Generics.DecidedLazy)
-- >             (Rec0 (f Bool))
-- >             :*: S1
-- >             ('MetaSel
-- >               ('Just "switchC")
-- >               'GHC.Generics.NoSourceUnpackedness
-- >               'GHC.Generics.NoSourceStrictness
-- >               'GHC.Generics.DecidedLazy)
-- >             (Rec0 (f Bool)))))
--
-- which should correspond to
--
-- > forall f. Rep (Switches f) :: * -> *
-- > = D1
-- >   ('MetaData "Switches" "Categorifier.C.CTypes.CGeneric" "main" 'False)
-- >   (C1
-- >     ('MetaCons "Switches" 'GHC.Generics.PrefixI 'True)
-- >     (Bitfield
-- >       (f Word8)
-- >       (M1
-- >         GHC.Generics.S
-- >         ('MetaSel
-- >           ('Just "switchA")
-- >           'GHC.Generics.NoSourceUnpackedness
-- >           'GHC.Generics.NoSourceStrictness
-- >           'GHC.Generics.DecidedLazy)
-- >         (Rec0 (f Bool))
-- >         :*: (S1
-- >               ('MetaSel
-- >                 ('Just "switchB")
-- >                 'GHC.Generics.NoSourceUnpackedness
-- >                 'GHC.Generics.NoSourceStrictness
-- >                 'GHC.Generics.DecidedLazy)
-- >               (Rec0 (f Bool))
-- >               :*: S1
-- >               ('MetaSel
-- >                 ('Just "switchC")
-- >                 'GHC.Generics.NoSourceUnpackedness
-- >                 'GHC.Generics.NoSourceStrictness
-- >                 'GHC.Generics.DecidedLazy)
-- >               (Rec0 (f Bool))))))
--
-- The appropriate unsigned integral type to represent the bitfield is computed automatically based
-- on number of elements in the product `G.Rep` of your type.
newtype Bitfield word (rep :: Type -> Type) p = Bitfield
  { getBitfield :: word
  }
  deriving (Eq, Ord, Show, Generic, Functor)

type instance TargetOb (Bitfield word rep p) = Bitfield (TargetOb word) (TargetObTC1 rep) p

type instance TargetObTC1 (Bitfield word rep) = Bitfield (TargetOb word) (TargetObTC1 rep)

-- | This type wraps a sum of nullary constructors (a nullary constructor looks like @`M1` `U1` ::
-- `C1` m `U1`@) inside a `Rep`.  It is safe to rely on the default implementation to generate
-- this type's `Rep` and `from` and `to` conversion methods if it doesn't have a custom `Enum`
-- instance.  The generically derived instance will simply number the fields consecutively starting
-- from 0.  The GHC-derived `Rep` for `Bool` is
--
-- > Rep Bool :: * -> *
-- > = D1
-- >  ('GHC.Generics.MetaData "Bool" "GHC.Types" "ghc-prim" 'False)
-- >  (C1
-- >     ('GHC.Generics.MetaCons "False" 'GHC.Generics.PrefixI 'False) U1
-- >   :+: C1
-- >         ('GHC.Generics.MetaCons "True" 'GHC.Generics.PrefixI 'False) U1)
--
-- which corresponds to
--
-- > Rep Bool :: * -> *
-- > = D1
-- >  ('GHC.Generics.MetaData "Bool" "GHC.Types" "ghc-prim" 'False)
-- >  (Enum
-- >  ((C1
-- >      ('GHC.Generics.MetaCons "False" 'GHC.Generics.PrefixI 'False) U1
-- >    :+: C1
-- >          ('GHC.Generics.MetaCons "True" 'GHC.Generics.PrefixI 'False) U1)))
--
-- In this way, the `Rep` preserves all the relevant type-level information of the `G.Rep`, but the
-- value-level representation is simply a `Word8` indexing the appropriate constructor.
--
-- If you are used to the `Categorifier.C.CTypes.Types.CxxType` way of representing an enum as an
-- `Categorifier.C.CTypes.Types.CEnum`, just keep in mind that a `Word8` with no wrapper type @f@ is
-- perfectly sufficient for representing the choice of nullary constructor in a sum.  A wrapper only
-- needs to be introduced in order to allow structure-only types like
-- @`Categorifier.C.CTypes.Types.CxxType` `Proxy`@; `CGeneric` represents this structural information at the
-- type level instead.
newtype Enum (rep :: Type -> Type) p = Enum
  { getEnum :: Word8
  }
  deriving (Eq, Ord, Show, Generic, Functor)

type instance TargetOb (Enum rep p) = Enum (TargetObTC1 rep) p

type instance TargetObTC1 (Enum rep) = Enum (TargetObTC1 rep)

-- | This type wraps a product of fields of the same type like `Bitfield`.  The difference is that
-- the value-level representation is simply a list of the declared element type.  We do not preserve
-- the original `G.Rep` of the structure's fields.  See `Categorifier.C.CTypes.CGeneric.RepArray` for
-- information on how to write an instance for a product that should use this representation.
--
-- == Flavors of C array
--
-- === Normal arrays
--
-- A type like
--
-- > data V3 a = V3 a a a
--
-- with a fixed number of fields of the argument type can be given a C representation as a normal
-- array using `Categorifier.C.CTypes.CGeneric.RepArray` or `Categorifier.C.CTypes.CGeneric.AsArray`.
--
-- Fixed-size vectors using the type `Vec` will also be represented using a `Array`.
--
-- === Flattened arrays
--
-- A type like
--
-- > data Irregular a = Irregular
-- >   { field1 :: a,
-- >     field2 :: V3 a,
-- >     field3 :: Product V3 Rotors a
-- >   }
--
-- composed of various nested functors, can be given a C representation as a "flattened" array using
-- `Categorifier.C.CTypes.CGeneric.RepFlattenedArray` or `Categorifier.C.CTypes.CGeneric.AsFlattenedArray`, where
-- the nesting structure is gone and the array contains one element per value of type @a@ in this
-- structure.
newtype Array (n :: Nat) elem p = Array
  { getArray :: Vec n elem
  }
  deriving (Eq, Ord, Show, Generic, Functor)

type instance TargetOb (Array n elem p) = Array n (TargetOb elem) p

type instance TargetObTC1 (Array n elem) = Array n (TargetOb elem)

-- | This type wraps a primitive C type.
--
-- GHC generics does not give such built-in (non-structural) types a `G.Rep`, because they cannot be
-- operated on generically.  However, we need to process primitive values all the time at the leaves
-- of all our `Rep`s; they're the only thing we can actually represent in C.  Without this
-- wrapper, we had to use a fancy type family in every module where we process `Rep`s to dispatch
-- between primitive and non-primitive types.  With this wrapper in hand, __consumers of `Rep`s
-- can safely assume that any @`Rec0` a@ they encounter when processing has a `CGeneric` instance.__
--
-- Primitive C types may also appear inside their own wrappers such as `Categorifier.C.KTypes.C.C`.
-- In this case, one sees e.g. @`Prim (`Categorifier.C.KTypes.C.C` `Bool`) p@.  To write
-- non-overlapping instances, just match on @`Prim` (f `Bool`)@ and @`Prim` `Bool`@ as needed --
-- no type families required.
newtype Prim prim p = Prim {getPrim :: prim}
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

type instance TargetOb (Prim prim p) = Prim (TargetOb prim) p

type instance TargetObTC1 (Prim prim) = Prim (TargetOb prim)

-- | This type computes the appropriate `Rep` for a primitive type @prim@.  You must fill in the
-- `Symbol` arguments with approximate GHC generics-like metadata (see examples below).
--
-- `PrimRep` is intended to be used with `primFrom` and `primTo` to define new primitive
-- instances of `CGeneric`.
--
-- == Examples
--
-- > instance CGeneric Bool where
-- >   type Rep Bool = PrimRep "Bool" "Data.Bool" "base" Bool
-- >   from = primFrom
-- >   to = primTo
--
-- > instance CGeneric (C Bool) where
-- >   type Rep (C Bool) =
-- >     PrimRep "CBool" "Categorifier.C.KTypes.C" "ktypes" (C Bool)
-- >   from = primFrom
-- >   to = primTo
type PrimRep (name :: Symbol) (modul :: Symbol) (package :: Symbol) prim =
  D1 ('MetaData name modul package 'False) (Prim prim)

-- | This function can be used to define primitive instances of `CGeneric`.  See `PrimRep` for
-- more details.
primFrom :: prim -> PrimRep name modul package prim p
primFrom = M1 . Prim

-- | This function can be used to define primitive instances of `CGeneric`.  See `PrimRep` for
-- more details.
primTo :: PrimRep name modul package prim p -> prim
primTo = getPrim . unM1

instance CGeneric Bool where
  type Rep Bool = PrimRep "Bool" "Data.Bool" "base" Bool
  from = primFrom
  to = primTo

instance CGeneric Int8 where
  type Rep Int8 = PrimRep "Int8" "Data.Int" "base" Int8
  from = primFrom
  to = primTo

instance CGeneric Int16 where
  type Rep Int16 = PrimRep "Int16" "Data.Int" "base" Int16
  from = primFrom
  to = primTo

instance CGeneric Int32 where
  type Rep Int32 = PrimRep "Int32" "Data.Int" "base" Int32
  from = primFrom
  to = primTo

instance CGeneric Int64 where
  type Rep Int64 = PrimRep "Int64" "Data.Int" "base" Int64
  from = primFrom
  to = primTo

instance CGeneric Word8 where
  type Rep Word8 = PrimRep "Word8" "Data.Word" "base" Word8
  from = primFrom
  to = primTo

instance CGeneric Word16 where
  type Rep Word16 = PrimRep "Word16" "Data.Word" "base" Word16
  from = primFrom
  to = primTo

instance CGeneric Word32 where
  type Rep Word32 = PrimRep "Word32" "Data.Word" "base" Word32
  from = primFrom
  to = primTo

instance CGeneric Word64 where
  type Rep Word64 = PrimRep "Word64" "Data.Word" "base" Word64
  from = primFrom
  to = primTo

instance CGeneric Float where
  type Rep Float = PrimRep "Float" "GHC.Float" "base" Float
  from = primFrom
  to = primTo

instance CGeneric Double where
  type Rep Double = PrimRep "Double" "GHC.Float" "base" Double
  from = primFrom
  to = primTo

{- Generic representation -}

-- | /G/enerically-derived /Rep/resentation.
type GRep a = GRep' (G.Rep a) (GIsEnum (G.Rep a))

type family GRep' rep isEnum where
-- Pass through the top-level data type
  GRep' (D1 md x) ep = D1 md (GRep' x ep)
-- If this can be an enum, apply the `Enum` constructor
  GRep' rep 'True = Enum rep
-- Pass through all constructors
  GRep' (C1 mc x) ep = C1 mc (GRep' x ep)
-- Pass through on products.  We don't derive bitfields generically, so there's nothing special to
-- do here.
  GRep' (a :*: b) ep = GRep' a ep :*: GRep' b ep
-- Pass through on sums.  We have already caught enums here.  We don't support sums in the
-- backend, but why not handle them here?
  GRep' (a :+: b) ep = GRep' a ep :+: GRep' b ep
-- If this is a field containing another type, that variable had better have a `Rep` itself.  We
-- just leave it in place.
  GRep' (S1 ms (Rec0 a)) _ep = S1 ms (Rec0 a)

-- `V1` and `U1` are out.

-- | Does this generic representation correspond to an enum?
type family GIsEnum rep where
  GIsEnum (D1 _m x) = GIsEnum x
  GIsEnum (a :+: b) = And (GIsEnum a) (GIsEnum b)
  GIsEnum (C1 _m U1) = 'True
  GIsEnum _ = 'False

type NotEnum rep = GIsEnum rep ~ 'False

type IsEnum rep = GIsEnum rep ~ 'True

type family And a b where
  And 'True 'True = 'True
  And _ _ = 'False

type family EqTy' a b where
  EqTy' ('Just ('Just a)) ('Just ('Just a)) = 'Just ('Just a)
  EqTy' ('Just ('Just a)) ('Just 'Nothing) = 'Just ('Just a)
  EqTy' ('Just 'Nothing) ('Just ('Just a)) = 'Just ('Just a)
  EqTy' _a _b = 'Nothing

type family Join a :: Maybe Type where
  Join ('Just ('Just a)) = 'Just a
  Join _ = 'Nothing

-- | If the type represented by @rep@ can be represented as a C array, return `'Just` the element
-- type.  If it cannot, return `'Nothing`.
type family GArrayType rep where
  GArrayType (D1 _m x) = GArrayType x
  GArrayType (C1 _m x) = GArrayType x
  GArrayType (a :*: b) = EqTy' (GArrayType a) (GArrayType b)
-- GArrayType (S1 ms (Rec0 (f x))) = FmapMaybe f (LookupTy x Primitives)
  GArrayType (S1 ms (Rec0 x)) = 'Just ('Just x)
-- This case is only to deal with things created by `RecurseRep`
  GArrayType (S1 ms (Rec1 x)) = GArrayType (G.Rep1 x)
  GArrayType (S1 ms Par1) = 'Just 'Nothing
  GArrayType _ = 'Nothing

{- Providing field names for types without them built in -}

type family CountFields (rep :: Type -> Type) :: Nat where
  CountFields (D1 _dm x) = CountFields x
  CountFields (C1 _cm x) = CountFields x
  CountFields (a :*: b) = CountFields a + CountFields b
  CountFields (S1 _sm x) = 'S 'Z

-- | Counts the number of constructors in a sum type.  The type may also have one constructor.
type family CountConstructors (rep :: Type -> Type) :: Nat where
  CountConstructors (D1 _dm x) = CountConstructors x
  CountConstructors (C1 _cm _x) = 'S 'Z
  CountConstructors (a :+: b) = CountConstructors a + CountConstructors b

type family Length (list :: [k]) :: Nat where
  Length '[] = 'Z
  Length (x ': xs) = 'S (Length xs)

type family Drop n (list :: [k]) :: [k] where
  Drop 'Z xs = xs
  Drop ('S n) (x ': xs) = Drop n xs

type family Take n (list :: [k]) :: [k] where
  Take 'Z _ = '[]
  Take ('S n) (x ': xs) = x ': Take n xs

-- This is a bit tortured, but it allows us to fill in a TypeError case to catch mismatches between
-- the number of fields provided by the user and the number of fields actually available
type family Split m n length (list :: [k]) acc where
  Split 'Z 'Z 'Z '[] acc = acc
  Split 'Z n n rights '(lefts, '[]) = '(lefts, rights)
  Split m n length list '( '[], '[]) = Split 'Z n (length - m) (Drop m list) '(Take m list, '[])

type Split' list a b = Split (CountFields a) (CountFields b) (Length list) list '( '[], '[])

type family ApplyToTuple tup a b where
  ApplyToTuple '(lefts, rights) a b = AssignFieldNames lefts a :*: AssignFieldNames rights b

type family AssignFieldNames names rep where
  AssignFieldNames names (D1 dm x) = D1 dm (AssignFieldNames names x)
  AssignFieldNames names (C1 cm x) = C1 cm (AssignFieldNames names x)
  AssignFieldNames names (a :*: b) = ApplyToTuple (Split' names a b) a b
  AssignFieldNames (name ': '[]) (S1 ('MetaSel 'Nothing unp str dec) x) =
    S1 ('MetaSel ('Just name) unp str dec) x

-- | This synonym can be used to manually fill in the names of a product type's fields.  For
-- instance, the `V3` type is defined as
--
-- > data V3 a = V3 a a a
--
-- with no record field selectors to give the fields names.  By convention, we use @x@, @y@ and @z@
-- to refer to these fields, and we require the C representation of 3-vectors to refer to them by
-- these names, so the `CGeneric` instance for `V3` is as follows:
--
-- > instance CGeneric (V3 a) where
-- >   type Rep (V3 a) = WithFieldNames '["x", "y", "z"] (V3 a)
--
-- The type-level `Symbol`s (in this instance @"x"@, @"y"@ and @"z"@) are applied to the fields in
-- the order they occur in the definition.
--
-- See also `ExplicitFieldNames` to understand how to do this more concisely.
type WithFieldNames names a =
  GRep' (AssignFieldNames names (G.Rep a)) (GIsEnum (AssignFieldNames names (G.Rep a)))

-- | This newtype wrapper is analogous to `Categorifier.C.CTypes.CGeneric.AsBitfield` or
-- `Categorifier.C.CTypes.CGeneric.AsArray`: it can be used with @deriving via@ to provide field names in C
-- for Haskell structures without them.  The following definition is equivalent to the instance for
-- @`V3` a@ given in the docs for `WithFieldNames`:
--
-- > deriving via ExplicitFieldNames '["x", "y", "z"] (V3 a) instance CGeneric (V3 a)
--
-- There are few situations where one would write a Haskell type definition without giving it field
-- names and then immediately provide C ones using `ExplicitFieldNames` and @deriving via@; it is
-- intended for use writing instances for third-party data types.
newtype ExplicitFieldNames (names :: [Symbol]) a = ExplicitFieldNames {getExplicitFieldNames :: a}
  deriving (Eq, Show, Ord, Generic)

instance
  ( Generic a,
    GFrom (GIsEnum (G.Rep a)) (G.Rep a) (WithFieldNames names a),
    GTo (GIsEnum (G.Rep a)) (G.Rep a) (WithFieldNames names a)
  ) =>
  CGeneric (ExplicitFieldNames names a)
  where
  type Rep (ExplicitFieldNames names a) = WithFieldNames names a
  from = gFrom . G.from . getExplicitFieldNames
  to = ExplicitFieldNames . G.to . gTo

{- Bitfield handling -}

-- | Like `KBits.KBits`, but it can be written for types without an @f@.  This is used internally by
-- `BlastBitfield`.
class Bits a where
  type Bit a :: Type
  testBit :: a -> Int -> Bit a
  setBitTo :: a -> Int -> Bit a -> a
  zeroBits :: a

bitsSetBitTo :: Bits.Bits w => w -> Int -> Bool -> w
bitsSetBitTo w idx b = w `f` idx
  where
    f = bool Bits.clearBit Bits.setBit b

instance Bits Word8 where
  type Bit Word8 = Bool
  testBit = Bits.testBit
  setBitTo = bitsSetBitTo
  zeroBits = Bits.zeroBits
  {-# INLINEABLE testBit #-}
  {-# INLINEABLE setBitTo #-}
  {-# INLINEABLE zeroBits #-}

instance Bits Word16 where
  type Bit Word16 = Bool
  testBit = Bits.testBit
  setBitTo = bitsSetBitTo
  zeroBits = Bits.zeroBits
  {-# INLINEABLE testBit #-}
  {-# INLINEABLE setBitTo #-}
  {-# INLINEABLE zeroBits #-}

instance Bits Word32 where
  type Bit Word32 = Bool
  testBit = Bits.testBit
  setBitTo = bitsSetBitTo
  zeroBits = Bits.zeroBits
  {-# INLINEABLE testBit #-}
  {-# INLINEABLE setBitTo #-}
  {-# INLINEABLE zeroBits #-}

instance Bits Word64 where
  type Bit Word64 = Bool
  testBit = Bits.testBit
  setBitTo = bitsSetBitTo
  zeroBits = Bits.zeroBits
  {-# INLINEABLE testBit #-}
  {-# INLINEABLE setBitTo #-}
  {-# INLINEABLE zeroBits #-}

instance (KBits.KBits f w, Bit w ~ Bool) => Bits (f w) where
  type Bit (f w) = f Bool
  testBit = KBits.testBit
  setBitTo = KBits.setBitTo
  zeroBits = KBits.zeroBits
  {-# INLINEABLE testBit #-}
  {-# INLINEABLE setBitTo #-}
  {-# INLINEABLE zeroBits #-}

instance
  ( BlastBitfield word rep,
    Bits word,
    NotEnum (C1 mc rep),
    GFrom 'False rep crep
  ) =>
  GFrom 'False (C1 mc rep) (C1 mc (Bitfield word crep))
  where
  gFrom (M1 rep) = M1 . Bitfield $ compactBitfield rep 0 zeroBits
  {-# INLINEABLE gFrom #-}

instance
  ( BlastBitfield word rep,
    NotEnum (C1 mc rep),
    GTo 'False rep crep
  ) =>
  GTo 'False (C1 mc rep) (C1 mc (Bitfield word crep))
  where
  gTo (M1 (Bitfield w)) = M1 $ expandBitfield w 0
  {-# INLINEABLE gTo #-}

-- | How many bits of a bitfield are needed to represent this generic product of booleans?
type family BitSize e where
  BitSize (S1 _m (Rec0 (f Bool))) = 'S 'Z
  BitSize (S1 _m (Rec0 Bool)) = 'S 'Z
  BitSize (a :*: b) = BitSize a + BitSize b

-- | Internal handling of translation of products of (possibly wrapped) booleans into integral
-- bitfields and back.
class Bits word => BlastBitfield word rep where
  compactBitfield :: rep x -> Int -> word -> word

  expandBitfield :: word -> Int -> rep x

instance (Bits word, Bit word ~ f Bool) => BlastBitfield word (S1 _m (Rec0 (f Bool))) where
  compactBitfield (M1 (K1 fb)) idx w = setBitTo w idx fb
  {-# INLINEABLE compactBitfield #-}

  expandBitfield w idx = M1 . K1 $ testBit w idx
  {-# INLINEABLE expandBitfield #-}

instance (Bits word, Bit word ~ Bool) => BlastBitfield word (S1 _m (Rec0 Bool)) where
  compactBitfield (M1 (K1 fb)) idx w = setBitTo w idx fb
  {-# INLINEABLE compactBitfield #-}

  expandBitfield w idx = M1 . K1 $ testBit w idx
  {-# INLINEABLE expandBitfield #-}

instance
  ( Bits word,
    lsize ~ BitSize a,
    Nat.SNatI lsize,
    BlastBitfield word a,
    BlastBitfield word b
  ) =>
  BlastBitfield word (a :*: b)
  where
  compactBitfield (a :*: b) idx = compactBitfield b (idx + leftCount) . compactBitfield a idx
    where
      leftCount :: Int
      leftCount = Nat.reflectToNum (Proxy @lsize)
  {-# INLINEABLE compactBitfield #-}

  expandBitfield w idx = expandBitfield w idx :*: expandBitfield w (idx + leftCount)
    where
      leftCount :: Int
      leftCount = Nat.reflectToNum (Proxy @lsize)
  {-# INLINEABLE expandBitfield #-}

{- Enum handling -}

instance
  ( IsEnum a,
    IsEnum b,
    CountEnum (a :+: b)
  ) =>
  GFrom 'True (a :+: b) (Enum (a :+: b))
  where
  gFrom = Enum . countEnum
  {-# INLINEABLE gFrom #-}

instance
  ( IsEnum a,
    IsEnum b,
    CountEnum (a :+: b)
  ) =>
  GTo 'True (a :+: b) (Enum (a :+: b))
  where
  gTo (Enum idx) = indexEnum @(a :+: b) idx
  {-# INLINEABLE gTo #-}

instance GFrom 'True (C1 m U1) (Enum (C1 m U1)) where
  gFrom = Enum . countEnum
  {-# INLINEABLE gFrom #-}

instance GTo 'True (C1 m U1) (Enum (C1 m U1)) where
  gTo (Enum idx) = indexEnum @(C1 m U1) idx
  {-# INLINEABLE gTo #-}

-- | We compute this at the type level, because we if we have an `R1` value, we need to then work
-- out how many constructors precede it in the `L1` we /don't/ have.  This could also be done by
-- traversing the `Rep` with a dedicated class, but this is shorter.
type family EnumSize e where
  EnumSize (C1 _m U1) = 'S 'Z
  EnumSize (D1 _m x) = EnumSize x
  EnumSize (a :+: b) = EnumSize a + EnumSize b

-- | Internal handling of translation between `Word8` and nullary sums.
class IsEnum a => CountEnum a where
  countEnum :: a x -> Word8

  indexEnum :: Word8 -> a x

instance
  ( CountEnum a,
    CountEnum b,
    Nat.SNatI lsize,
    lsize ~ EnumSize a
  ) =>
  CountEnum (a :+: b)
  where
  countEnum (L1 x) = countEnum x
  countEnum (R1 x) = leftCount + countEnum x
    where
      leftCount :: Word8
      leftCount = Nat.reflectToNum (Proxy @lsize)
  {-# INLINEABLE countEnum #-}

  indexEnum idx =
    if idx < leftCount
      then L1 $ indexEnum @a idx
      else R1 $ indexEnum @b (idx - leftCount)
    where
      leftCount :: Word8
      leftCount = Nat.reflectToNum (Proxy @lsize)
  {-# INLINEABLE indexEnum #-}

instance CountEnum (C1 _m U1) where
  countEnum (M1 U1) = 0
  {-# INLINEABLE countEnum #-}

  indexEnum _betterBeZero = M1 U1
  {-# INLINEABLE indexEnum #-}

{- Array handling -}

-- TODO(MP): This could in principle let us avoid any value-level indexing until the last possible
-- moment.
type family ArraySize a where
  ArraySize (D1 _m x) = ArraySize x
  ArraySize (C1 _m x) = ArraySize x
  ArraySize (S1 _m (Rec0 a)) = 'S 'Z
  ArraySize (a :*: b) = ArraySize a + ArraySize b

-- | Class for getting array fields when there's no nesting going on
class
  ( size ~ ArraySize rep
  ) =>
  ArrayFields (offset :: Nat) (size :: Nat) elem rep
  where
  fieldsToArray :: rep x -> Vec offset elem -> Vec (size + offset) elem

  arrayToFields :: Vec (size + offset) elem -> (rep x, Vec offset elem)

instance
  ( size ~ ArraySize (a :*: b),
    ArrayFields (rsize + offset) lsize elem a,
    ArrayFields offset rsize elem b,
    lsize ~ ArraySize a,
    rsize ~ ArraySize b,
    lsize + (rsize + offset) ~ (size + offset)
  ) =>
  ArrayFields offset size elem (a :*: b)
  where
  fieldsToArray (a :*: b) = fieldsToArray a . fieldsToArray b
  {-# INLINEABLE fieldsToArray #-}

  arrayToFields = (\(v1, (v2, s)) -> (v1 :*: v2, s)) . second arrayToFields . arrayToFields
  {-# INLINEABLE arrayToFields #-}

instance ArrayFields offset ('S 'Z) elem (S1 m (Rec0 elem)) where
  fieldsToArray (M1 (K1 x)) = (x :::)
  {-# INLINEABLE fieldsToArray #-}

  arrayToFields (h ::: t) = (M1 $ K1 h, t)
  {-# INLINEABLE arrayToFields #-}

instance
  ( ArraySize a ~ na,
    ArraySize b ~ nb,
    ArrayFields 'Z size elem (a :*: b),
    'Just elem ~ Join (GArrayType (C1 m (a :*: b))),
    size ~ (na + nb),
    size + 'Z ~ size
  ) =>
  GFrom 'False (C1 m (a :*: b)) (C1 m (Array size elem))
  where
  gFrom =
    M1 . Array . flip (fieldsToArray @'Z @size @elem @_) VNil . unM1
  {-# INLINEABLE gFrom #-}

instance
  ( ArraySize a ~ na,
    ArraySize b ~ nb,
    ArrayFields 'Z size elem (a :*: b),
    'Just elem ~ Join (GArrayType (C1 m (a :*: b))),
    size ~ (na + nb),
    size + 'Z ~ size
  ) =>
  GTo 'False (C1 m (a :*: b)) (C1 m (Array size elem))
  where
  gTo = M1 . fst . arrayToFields @'Z @size @elem @_ . getArray . unM1
  {-# INLINEABLE gTo #-}

-- Provided instances for base types

instance (Generic (Identity a)) => CGeneric (Identity a)

instance (Generic (Const a b)) => CGeneric (Const a (b :: k))

instance CGeneric (Product f g a) where
  type Rep (Product f g a) = WithFieldNames '["first", "second"] (Product f g a)

instance CGeneric (Barbies.Unit f) where
  type Rep (Barbies.Unit f) = G.Rep (Barbies.Unit f)
  from = const (M1 (M1 U1))
  to = const Barbies.Unit
  {-# INLINEABLE from #-}
  {-# INLINEABLE to #-}

instance CGeneric () where
  type Rep () = G.Rep ()
  from = const (M1 (M1 U1))
  to = const ()
  {-# INLINEABLE from #-}
  {-# INLINEABLE to #-}

instance CGeneric (a, b) where
  type
    Rep (a, b) =
      WithFieldNames '["tuple_element_0", "tuple_element_1"] (a, b)

instance CGeneric (a, b, c) where
  type
    Rep (a, b, c) =
      WithFieldNames '["tuple_element_0", "tuple_element_1", "tuple_element_2"] (a, b, c)

instance CGeneric (a, b, c, d) where
  type
    Rep (a, b, c, d) =
      WithFieldNames
        '[ "tuple_element_0",
           "tuple_element_1",
           "tuple_element_2",
           "tuple_element_3"
         ]
        (a, b, c, d)

instance CGeneric (a, b, c, d, e) where
  type
    Rep (a, b, c, d, e) =
      WithFieldNames
        '[ "tuple_element_0",
           "tuple_element_1",
           "tuple_element_2",
           "tuple_element_3",
           "tuple_element_4"
         ]
        (a, b, c, d, e)

instance CGeneric (a, b, c, d, e, f) where
  type
    Rep (a, b, c, d, e, f) =
      WithFieldNames
        '[ "tuple_element_0",
           "tuple_element_1",
           "tuple_element_2",
           "tuple_element_3",
           "tuple_element_4",
           "tuple_element_5"
         ]
        (a, b, c, d, e, f)

instance CGeneric (a, b, c, d, e, f, g) where
  type
    Rep (a, b, c, d, e, f, g) =
      WithFieldNames
        '[ "tuple_element_0",
           "tuple_element_1",
           "tuple_element_2",
           "tuple_element_3",
           "tuple_element_4",
           "tuple_element_5",
           "tuple_element_6"
         ]
        (a, b, c, d, e, f, g)

instance Generic (Compose f g a) => CGeneric (Compose f g a)

-- | This is a one-off instance designed to support generic traversals of fixed-size vectors.
instance CGeneric (Vec n elem) where
  type
    Rep (Vec n elem) =
      D1
        ('MetaData "Vec" "Categorifier.Indexed.Vec" "commonZSindexedZSvec" 'False)
        (C1 ('MetaCons "FakeConstructor" 'PrefixI 'False) (Array n elem))
  from = M1 . M1 . Array
  {-# INLINEABLE from #-}
  to = getArray . unM1 . unM1
  {-# INLINEABLE to #-}

-- | See note in the `CGeneric` docs about sum type support.
instance CGeneric (Either a b)

-- | See note in the `CGeneric` docs about sum type support.
instance CGeneric (Maybe a) where
  type Rep (Maybe a) = G.Rep (Maybe a)
  from = G.from
  {-# INLINEABLE from #-}
  to = G.to
  {-# INLINEABLE to #-}
