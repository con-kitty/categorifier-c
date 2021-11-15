{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Higher kinded fixed-point operators currently unavailable in @yaya@.
module Kitty.Recursion
  ( HFix,
    BFix,
    HMu (..),
    NaturalTransformation (..),
    NaturalTransformationC (..),
    Projectable (..),
    hcata,
    hcataC,
    hembed,
    hproject,
  )
where

import qualified Barbies
import Control.Category (Category (..))
import Control.DeepSeq (NFData)
import Data.Data (Data, Typeable)
import Data.Functor.Classes (Show1)
import Data.Hashable (Hashable (..))
import Data.Hashable.Lifted (Hashable1 (..))
import GHC.Generics (Generic, Generic1)
import Kitty.Higher (HFunctorC (..))
import Prelude hiding ((.), id)
import Yaya.Fold (Projectable (..), Recursive (..), Steppable (..))
import Yaya.Functor (HFunctor (..))

newtype NaturalTransformation arr f g = NT {runNT :: forall a. f a `arr` g a}

instance Category arr => Category (NaturalTransformation arr) where
  id = NT id
  NT f . NT g = NT $ f . g

newtype NaturalTransformationC c arr f g = NTC {runNTC :: forall a. c a => f a `arr` g a}

newtype HFix f i = HFix {hunFix :: f (HFix f) i}

instance Barbies.ConstraintsB (f (HFix f)) => Barbies.ConstraintsB (HFix f)

instance Barbies.FunctorB (f (HFix f)) => Barbies.FunctorB (HFix f)

instance Barbies.TraversableB (f (HFix f)) => Barbies.TraversableB (HFix f)

deriving newtype instance Eq (f (HFix f) i) => Eq (HFix f i)

deriving newtype instance Ord (f (HFix f) i) => Ord (HFix f i)

deriving newtype instance NFData (f (HFix f) i) => NFData (HFix f i)

deriving newtype instance Show (f (HFix f) i) => Show (HFix f i)

deriving newtype instance Show1 (f (HFix f)) => Show1 (HFix f)

deriving stock instance Generic (HFix f a)

deriving stock instance Generic1 (HFix f)

deriving stock instance
  (Typeable k, Typeable f, Typeable a, Data (f (HFix f) a)) =>
  Data (HFix f (a :: k))

instance Hashable1 (f (HFix f)) => Hashable1 (HFix f) where
  liftHashWithSalt hashA s (HFix x) = liftHashWithSalt hashA s x

instance (Hashable1 (f (HFix f)), Hashable a) => Hashable (HFix f a) where
  hashWithSalt s (HFix x) = liftHashWithSalt hashWithSalt s x

instance Projectable (NaturalTransformation (->)) (HFix f) f where
  project = NT hunFix

instance Steppable (NaturalTransformation (->)) (HFix f) f where
  embed = NT HFix

instance HFunctor f => Recursive (NaturalTransformation (->)) (HFix f) f where
  cata f = NT (runNT f . hmap (runNT (cata f)) . hunFix)

instance HFunctorC c f => Recursive (NaturalTransformationC c (->)) (HFix f) f where
  cata f = NTC (runNTC f . hmapC @c (runNTC (cata f)) . hunFix)

-- | A duplicate of `HFix`, but using "Barbies" constraints for the instances.
newtype BFix f i = BFix {bunFix :: f (BFix f) i}

instance Barbies.ConstraintsB (f (BFix f)) => Barbies.ConstraintsB (BFix f)

instance Barbies.FunctorB (f (BFix f)) => Barbies.FunctorB (BFix f)

instance Barbies.TraversableB (f (BFix f)) => Barbies.TraversableB (BFix f)

deriving newtype instance Eq (f (BFix f) i) => Eq (BFix f i)

deriving newtype instance Ord (f (BFix f) i) => Ord (BFix f i)

deriving newtype instance NFData (f (BFix f) i) => NFData (BFix f i)

deriving newtype instance Show (f (BFix f) i) => Show (BFix f i)

deriving newtype instance Show1 (f (BFix f)) => Show1 (BFix f)

deriving stock instance Generic (BFix f a)

deriving stock instance Generic1 (BFix f)

deriving stock instance
  (Typeable k, Typeable f, Typeable a, Data (f (BFix f) a)) =>
  Data (BFix f (a :: k))

instance Hashable1 (f (BFix f)) => Hashable1 (BFix f) where
  liftHashWithSalt hashA s (BFix x) = liftHashWithSalt hashA s x

instance (Hashable1 (f (BFix f)), Hashable a) => Hashable (BFix f a) where
  hashWithSalt s (BFix x) = liftHashWithSalt hashWithSalt s x

instance Projectable (NaturalTransformation (->)) (BFix f) f where
  project = NT bunFix

instance Steppable (NaturalTransformation (->)) (BFix f) f where
  embed = NT BFix

instance Barbies.FunctorT f => Recursive (NaturalTransformation (->)) (BFix f) f where
  cata f = NT (runNT f . Barbies.tmap (runNT (cata f)) . hproject)

instance
  (Barbies.FunctorT f, Barbies.ConstraintsT f, Barbies.AllT c f) =>
  Recursive (NaturalTransformationC c (->)) (BFix f) f
  where
  cata f = NTC (runNTC f . Barbies.tmapC @c (runNTC (cata f)) . hproject)

newtype HMu f i = HMu {hunMu :: forall a. (forall x. f a x -> a x) -> a i}

instance HFunctor f => Projectable (NaturalTransformation (->)) (HMu f) f where
  project = cata (NT (hmap hembed))

instance HFunctor f => Steppable (NaturalTransformation (->)) (HMu f) f where
  embed = NT (\m -> HMu (\f -> f (hmap (runNT (cata (NT f))) m)))

instance Recursive (NaturalTransformation (->)) (HMu f) f where
  cata (NT f) = NT (\(HMu unMu) -> unMu f)

-- | Lowers a fold over `NaturalTransformation`s to one over the underlying category.
hcata :: Recursive (NaturalTransformation arr) t f => (forall x. f a x `arr` a x) -> t i `arr` a i
hcata f = runNT (cata (NT f))

hcataC ::
  forall c arr nt t f a i.
  (c i, nt ~ NaturalTransformationC c arr, Recursive nt t f) =>
  (forall x. c x => f a x `arr` a x) ->
  t i `arr` a i
hcataC f = runNTC (cata @nt (NTC f))

hembed :: Steppable (NaturalTransformation c) t f => f t i `c` t i
hembed = runNT embed

hproject :: Projectable (NaturalTransformation c) t f => t i `c` f t i
hproject = runNT project
