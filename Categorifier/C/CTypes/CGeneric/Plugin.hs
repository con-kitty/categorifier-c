{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Handles mapping betwen our C representation and the plugin's sum-of-products
--   representation.
--
--   Since `CG.CGeneric` is an extension of `G.Generic`, these same operations should work for
--   converting between `Client.Rep` and `G.Rep` as well.
--
--   The following equivalences should hold:
--
-- - @`from` ~~ `Client.repr` . `CG.to` :: `CG.Rep` a x -> `Client.Rep` a@
-- - @`to` ~~ `CG.from` . `Client.abst` :: `Client.Rep` a -> `CG.Rep` a x@
--
--   However, we can't use that in the case where we're dealing with `Categorifier.C.CExpr.Types.Core.CExpr`
--   instead of `Categorifier.C.KTypes.C.C`, because the underlying @a@ hardcodes
--  `Categorifier.C.KTypes.C.C`, so we use this as a bypass.
module Categorifier.C.CTypes.CGeneric.Plugin
  ( RepRep (..),
    repRep,
    repRep',
    Vectorizable (..),
  )
where

import qualified Categorifier.C.CTypes.CGeneric.Class as CG
import Categorifier.C.Nat.Operators (type (+))
import qualified Categorifier.Client as Client
import Control.Lens.Iso (Iso', iso)
import Data.Bifunctor (second)
import Data.Proxy (Proxy (..))
import Data.Type.Nat (Nat (..))
import qualified Data.Type.Nat as Nat
import Data.Vec.Lazy (Vec (..))
import GHC.Generics (D1, K1 (..), M1 (..), U1 (..), (:*:) (..))
import qualified GHC.Generics as G

-- | Every `RepRep` instance forms a valid isomorphism.
repRep :: RepRep a (aRep x) => Iso' a (aRep x)
repRep = iso to from

-- | A shorthand for the case where you want to work with an @a@.
repRep' :: (RepRep (Client.Rep a) (CG.Rep a x)) => Proxy a -> Iso' (Client.Rep a) (CG.Rep a x)
repRep' Proxy = repRep

class RepRep hasRep cgRep where
  to :: hasRep -> cgRep
  from :: cgRep -> hasRep

instance RepRep a (K1 i a x) where
  to = K1
  from = unK1

instance RepRep a (aRep x) => RepRep a (M1 i m aRep x) where
  to = M1 . to
  from = from . unM1

instance RepRep () (U1 x) where
  to () = U1
  from U1 = ()

instance (RepRep a (aRep x), RepRep b (bRep x)) => RepRep (a, b) ((aRep :*: bRep) x) where
  to (a, b) = to a :*: to b
  from (aRep :*: bRep) = (from aRep, from bRep)

instance (n ~ (n + 'Z), VecRep a 'Z n e) => RepRep a (CG.Array n e x) where
  to = CG.Array . flip toVec VNil
  from = fst . fromVec @_ @'Z . CG.getArray

-- | Converts to/from a `Vec` for `CG.Array` reps.
class (size ~ ArraySize a) => VecRep a offset size e where
  toVec :: a -> Vec offset e -> Vec (size + offset) e
  fromVec :: Vec (size + offset) e -> (a, Vec offset e)

instance (ArraySize e ~ 'S 'Z) => VecRep e offset ('S 'Z) e where
  toVec = (:::)
  fromVec (h ::: t) = (h, t)

type family ArraySize a where
  ArraySize (a, b) = ArraySize a + ArraySize b
  ArraySize a = 'S 'Z

instance
  ( size ~ (lsize + rsize),
    VecRep a (rsize + offset) lsize e,
    VecRep b offset rsize e,
    lsize ~ ArraySize a,
    rsize ~ ArraySize b,
    lsize + (rsize + offset) ~ (size + offset)
  ) =>
  VecRep (a, b) offset size e
  where
  toVec (a, b) = toVec @_ @_ @lsize a . toVec @_ @offset @rsize b
  fromVec =
    (\(v1, (v2, s)) -> ((v1, v2), s)) . second (fromVec @_ @offset @rsize) . fromVec @_ @_ @lsize

instance (CG.Bits word, WordRep a word) => RepRep a (CG.Bitfield word rep x) where
  to a = CG.Bitfield $ toWord a 0 CG.zeroBits
  from = fromWord 0 . CG.getBitfield

class WordRep a word where
  toWord :: a -> Int -> word -> word
  fromWord :: Int -> word -> a

instance (CG.Bits word, CG.Bit word ~ f Bool) => WordRep (f Bool) word where
  toWord fb idx w = CG.setBitTo w idx fb
  fromWord idx w = CG.testBit w idx

instance (CG.Bits word, CG.Bit word ~ Bool) => WordRep Bool word where
  toWord b idx w = CG.setBitTo w idx b
  fromWord idx w = CG.testBit w idx

instance
  (lsize ~ ArraySize a, Nat.SNatI lsize, WordRep a word, WordRep b word) =>
  WordRep (a, b) word
  where
  toWord (a, b) idx = toWord b (idx + Nat.reflectToNum (Proxy @lsize)) . toWord a idx
  fromWord idx w = (fromWord idx w, fromWord (idx + Nat.reflectToNum (Proxy @lsize)) w)

instance (G.Generic a, CG.CountEnum (G.Rep a)) => RepRep a (CG.Enum rep x) where
  to = CG.Enum . CG.countEnum . G.from
  from = G.to . CG.indexEnum . CG.getEnum

instance RepRep a (CG.Prim a x) where
  to = CG.Prim
  from = CG.getPrim

-- | Converts a type into (and out of) a form that vectorizes the same as the underlying type.
class Vectorizable a v where
  makeVectorizable :: a -> v
  unmakeVectorizable :: v -> a

-- | The default, when possible is just to go back to the underlying type. However, this doesn't
--   work if there's a `Categorifier.C.CExpr.Types.Core.CExpr` / `Categorifier.C.KTypes.C.C` mismatch.
instance {-# OVERLAPPABLE #-} (Client.HasRep b, a ~ Client.Rep b) => Vectorizable a b where
  makeVectorizable = Client.abst
  unmakeVectorizable = Client.repr

-- | This handles the `Categorifier.C.CExpr.Types.Core.CExpr` / `Categorifier.C.KTypes.C.C` mismatch case by
--   going directly to `CG.Rep`, which is guaranteed to have the same vectorization as the
--   underlying type.
instance RepRep a (D1 m b x) => Vectorizable a (D1 m b x) where
  makeVectorizable = to
  unmakeVectorizable = from
