{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Categorifier.C.CTypes.GArrays
  ( GArrays (..),
    FromArraysError (..),
    ToArraysError (..),
  )
where

import qualified Barbies
import Categorifier.C.CTypes.CGeneric.Class (CGeneric)
import qualified Categorifier.C.CTypes.CGeneric.Class as CG
import Categorifier.C.KTypes.KLiteral (KLiteral (..))
import Categorifier.C.Prim
  ( ArrayCount (..),
    ArrayMVec (..),
    Arrays (..),
    IsPrimitive (..),
    Prim (..),
    PrimLens (..),
    PrimType,
    Tagged (..),
    _ArrayCount,
  )
import Categorifier.Common.IO.Exception (Exception)
import Control.Applicative (liftA2)
import Control.DeepSeq (force)
import Control.Lens (over, set, view)
import Control.Monad (when)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import Data.STRef (STRef, readSTRef, writeSTRef)
import qualified Data.Type.Nat as Nat
import Data.Vec.Lazy (Vec)
import qualified Data.Vec.Lazy as Vec
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (K1 (..), M1 (..), U1 (..), (:*:) (..), (:+:) (..))

newtype FromArraysError = RanOutOfPrim (Prim Proxy) deriving (Show)

instance Exception FromArraysError

data ToArraysError = RanOutOf PrimType Int deriving (Show)

instance Exception ToArraysError

class GArrays f a where
  arraysCount :: Proxy f -> Proxy a -> Arrays ArrayCount
  default arraysCount ::
    (CGeneric a, GArrays f (CG.Rep a ())) => Proxy f -> Proxy a -> Arrays ArrayCount
  arraysCount pf = arraysCount @_ @(CG.Rep a ()) pf . fmap CG.from

  fromArrays ::
    forall v.
    (forall b. IsPrimitive b => GV.Vector v (f b)) =>
    Arrays (Compose v f) ->
    Either FromArraysError (a, Arrays (Compose v f))
  default fromArrays ::
    (CGeneric a, GArrays f (CG.Rep a ()), forall b. IsPrimitive b => GV.Vector v (f b)) =>
    Arrays (Compose v f) ->
    Either FromArraysError (a, Arrays (Compose v f))
  fromArrays = runStateT $ fmap CG.to . StateT $ fromArrays @_ @(CG.Rep a ())

  -- | Takes a structure of mutable arrays to write into, as well as the current indices into those
  --   arrays and adds the @a@ to the arrays.
  toArrays ::
    forall mv v s.
    (mv ~ GV.Mutable v, forall b. IsPrimitive b => MV.MVector mv (f b)) =>
    Arrays (ArrayMVec v s f) ->
    STRef s (Arrays ArrayCount) ->
    a ->
    ST s (Either ToArraysError ())
  default toArrays ::
    ( CGeneric a,
      GArrays f (CG.Rep a ()),
      mv ~ GV.Mutable v,
      forall b. IsPrimitive b => MV.MVector mv (f b)
    ) =>
    Arrays (ArrayMVec v s f) ->
    STRef s (Arrays ArrayCount) ->
    a ->
    ST s (Either ToArraysError ())
  toArrays arrays indices = toArrays @_ @(CG.Rep a ()) arrays indices . CG.from

-- | __FIXME__: This instance isn't good, but we shouldn't be using it yet anyway. It only exists to
--              allow more instances to be boiled easily.
instance (GArrays f (a p), GArrays f (b p)) => GArrays f ((a :+: b) p) where
  arraysCount pf Proxy =
    arraysCount pf (Proxy @(a p)) `max` arraysCount pf (Proxy @(b p))
  fromArrays = runStateT $ L1 <$> StateT fromArrays
  toArrays arrays indices (L1 f) = toArrays arrays indices f
  toArrays arrays indices (R1 g) = toArrays arrays indices g

instance (GArrays f (a p), GArrays f (b p)) => GArrays f ((a :*: b) p) where
  arraysCount pf Proxy =
    arraysCount pf (Proxy @(a p)) <> arraysCount pf (Proxy @(b p))
  fromArrays = runStateT $ liftA2 (:*:) (StateT fromArrays) (StateT fromArrays)
  toArrays arrays indices (a :*: b) =
    runExceptT $ ExceptT (toArrays arrays indices a) *> ExceptT (toArrays arrays indices b)

instance GArrays f b => GArrays f (K1 a b p) where
  arraysCount pf Proxy = arraysCount pf (Proxy @b)
  fromArrays = runStateT $ K1 <$> StateT fromArrays
  toArrays arrays indices (K1 c) = toArrays arrays indices c

instance GArrays f (g p) => GArrays f (M1 i c g p) where
  arraysCount pf Proxy = arraysCount pf (Proxy @(g p))
  fromArrays = runStateT $ M1 <$> StateT fromArrays
  toArrays arrays indices (M1 g) = toArrays arrays indices g

instance GArrays f (U1 x) where
  arraysCount Proxy Proxy = mempty
  fromArrays = runStateT $ pure U1
  toArrays _ _ U1 = pure $ pure ()

instance (Nat.SNatI n, GArrays f e) => GArrays f (CG.Array n e p) where
  arraysCount pf Proxy =
    mconcat . replicate (Nat.reflectToNum (Proxy @n)) $
      arraysCount pf (Proxy @e)
  fromArrays = runStateT $ fmap CG.Array . sequenceA . Vec.repeat $ StateT fromArrays
  toArrays arrays indices (CG.Array e) =
    runExceptT $ traverse_ (ExceptT . toArrays arrays indices) e

instance GArrays f w => GArrays f (CG.Bitfield w r p) where
  arraysCount pf Proxy = arraysCount pf (Proxy @w)
  fromArrays = runStateT $ CG.Bitfield <$> StateT fromArrays
  toArrays arrays indices (CG.Bitfield w) = toArrays arrays indices w

instance (Foldable f, KLiteral f Word8) => GArrays f (CG.Enum r p) where
  arraysCount pf Proxy = arraysCount pf (Proxy @(f Word8))
  fromArrays = runStateT $ CG.Enum . sum <$> StateT (fromArrays @_ @(f Word8))
  toArrays arrays indices (CG.Enum w) = toArrays @_ @(f Word8) arrays indices $ kliteral w

primArrayCount :: Prim f -> Arrays ArrayCount
primArrayCount (PrimBool _) = mempty {arrayBool = ArrayCount 1}
primArrayCount (PrimInt8 _) = mempty {arrayInt8 = ArrayCount 1}
primArrayCount (PrimInt16 _) = mempty {arrayInt16 = ArrayCount 1}
primArrayCount (PrimInt32 _) = mempty {arrayInt32 = ArrayCount 1}
primArrayCount (PrimInt64 _) = mempty {arrayInt64 = ArrayCount 1}
primArrayCount (PrimWord8 _) = mempty {arrayWord8 = ArrayCount 1}
primArrayCount (PrimWord16 _) = mempty {arrayWord16 = ArrayCount 1}
primArrayCount (PrimWord32 _) = mempty {arrayWord32 = ArrayCount 1}
primArrayCount (PrimWord64 _) = mempty {arrayWord64 = ArrayCount 1}
primArrayCount (PrimFloat _) = mempty {arrayFloat = ArrayCount 1}
primArrayCount (PrimDouble _) = mempty {arrayDouble = ArrayCount 1}

primArraysCount :: forall f a. IsPrimitive a => Proxy f -> Proxy (f a) -> Arrays ArrayCount
primArraysCount Proxy Proxy = primArrayCount . toPrim $ Proxy @a

primFromArrays ::
  forall v f a.
  (IsPrimitive a, GV.Vector v (f a)) =>
  Arrays (Compose v f) ->
  Either FromArraysError (f a, Arrays (Compose v f))
primFromArrays arrays
  | GV.null arr = Left . RanOutOfPrim . unTagged $ primType @a
  | otherwise = Right (GV.head arr, set (prims_ @a) (Compose (GV.tail arr)) arrays)
  where
    Compose arr = view (prims_ @a) arrays :: Compose v f a

primToArrays ::
  forall v mv s f a.
  (mv ~ GV.Mutable v, IsPrimitive a, MV.MVector mv (f a)) =>
  Arrays (ArrayMVec v s f) ->
  STRef s (Arrays ArrayCount) ->
  f a ->
  ST s (Either ToArraysError ())
primToArrays mutableArrays arraysCountRef cprim = runExceptT $ do
  counts0 <- lift (readSTRef arraysCountRef)
  counts1 <- f counts0 cprim (view (prims_ @a) mutableArrays)
  lift (writeSTRef arraysCountRef $ force counts1)
  where
    f ::
      Arrays ArrayCount ->
      f a ->
      ArrayMVec v s f a ->
      ExceptT ToArraysError (ST s) (Arrays ArrayCount)
    f countarr val (ArrayMVec marray) =
      let count0 = view (prims_ @a . _ArrayCount) countarr
          n = MV.length marray
       in do
            when (count0 > n) . throwE $ RanOutOf (unTagged $ primType @a) n
            MV.write marray count0 val
            pure $ over (prims_ @a . _ArrayCount) (+ 1) countarr

-- instance {-# OVERLAPPABLE #-} IsPrimitive a => GArrays f (f a) where
--   arraysCount = primArraysCount
--   fromArrays = primFromArrays
--   toArrays = primToArrays

instance GArrays f (f Bool) where
  arraysCount = primArraysCount
  fromArrays = primFromArrays
  toArrays = primToArrays

instance GArrays f (f Double) where
  arraysCount = primArraysCount
  fromArrays = primFromArrays
  toArrays = primToArrays

instance GArrays f (f Float) where
  arraysCount = primArraysCount
  fromArrays = primFromArrays
  toArrays = primToArrays

instance GArrays f (f Int8) where
  arraysCount = primArraysCount
  fromArrays = primFromArrays
  toArrays = primToArrays

instance GArrays f (f Int16) where
  arraysCount = primArraysCount
  fromArrays = primFromArrays
  toArrays = primToArrays

instance GArrays f (f Int32) where
  arraysCount = primArraysCount
  fromArrays = primFromArrays
  toArrays = primToArrays

instance GArrays f (f Int64) where
  arraysCount = primArraysCount
  fromArrays = primFromArrays
  toArrays = primToArrays

instance GArrays f (f Word8) where
  arraysCount = primArraysCount
  fromArrays = primFromArrays
  toArrays = primToArrays

instance GArrays f (f Word16) where
  arraysCount = primArraysCount
  fromArrays = primFromArrays
  toArrays = primToArrays

instance GArrays f (f Word32) where
  arraysCount = primArraysCount
  fromArrays = primFromArrays
  toArrays = primToArrays

instance GArrays f (f Word64) where
  arraysCount = primArraysCount
  fromArrays = primFromArrays
  toArrays = primToArrays

-- Most instances should look like this, unless they're bitfields or arrays.

instance GArrays f ()

instance (GArrays f a, GArrays f b) => GArrays f (a, b)

instance (GArrays f a, GArrays f b, GArrays f c) => GArrays f (a, b, c)

instance (GArrays f a, GArrays f b, GArrays f c, GArrays f d) => GArrays f (a, b, c, d)

instance
  (GArrays f a, GArrays f b, GArrays f c, GArrays f d, GArrays f e) =>
  GArrays f (a, b, c, d, e)

instance GArrays f (g (h a)) => GArrays f (Compose g h a)

instance GArrays f a => GArrays f (Identity a)

instance GArrays f a => GArrays f (Maybe a)

instance GArrays f (Barbies.Unit f)

instance (GArrays f a, Nat.SNatI k) => GArrays f (Vec k a)
