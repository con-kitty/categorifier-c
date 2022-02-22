{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Convert a 'CType' to 'Arrays' of primitives.
module Categorifier.C.CTypes.ToArrays
  ( ToArraysError,
    ctypesToArrays,
    ctypesToArrays',
  )
where

import qualified Barbies
import qualified Categorifier.C.Barbies as Barbies
import Categorifier.C.CTypes.ArrayLengths (arrayLengthsCType, maxUnionConLengths)
import Categorifier.C.CTypes.Traverse (CTraversalFuns_ (..), traverseCCon_, traverseCType_)
import Categorifier.C.CTypes.Types (CEnum (..), CType, CUnion, CUnionF (..))
import Categorifier.C.Prim
  ( ArrayCount (..),
    ArrayMVec (..),
    Arrays (..),
    IsPrimitive (..),
    PrimLens (..),
    PrimType,
    unTagged,
    _ArrayCount,
  )
import Control.DeepSeq (force)
import Control.Lens (over, view)
import Control.Monad (when)
import Control.Monad.Primitive (PrimState)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import qualified Data.Foldable as F
import Data.Functor.Compose (Compose (..))
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Generic.Mutable as MV

data ToArraysError = RanOutOf PrimType Int deriving (Show)

{-# INLINEABLE ctypesToArrays #-}
ctypesToArrays ::
  forall mv v f g.
  ( mv ~ V.Mutable v,
    forall a. IsPrimitive a => V.Vector v (f a),
    forall a. IsPrimitive a => MV.MVector mv (f a),
    Functor g,
    Foldable g
  ) =>
  g (CType f) ->
  Either ToArraysError (Arrays (Compose v f))
ctypesToArrays ctypes = runST action
  where
    arrayLengths :: Arrays ArrayCount
    arrayLengths = F.fold (fmap arrayLengthsCType ctypes)
    allocate ::
      forall a s. MV.MVector (V.Mutable v) (f a) => ArrayCount a -> ST s (ArrayMVec v s f a)
    allocate = fmap ArrayMVec . MG.new . unArrayCount
    freeze :: V.Vector v (f a) => ArrayMVec v (PrimState (ST s)) f a -> ST s (Compose v f a)
    freeze = fmap Compose . G.freeze . unArrayMVec
    action :: ST s (Either ToArraysError (Arrays (Compose v f)))
    action = do
      mutableArrays :: Arrays (ArrayMVec v s f) <-
        Barbies.btraverseC @IsPrimitive allocate arrayLengths
      maybeErrors <- ctypesToArrays' mutableArrays ctypes
      case maybeErrors of
        Just errors -> pure (Left errors)
        Nothing -> Right <$> Barbies.btraverseC @IsPrimitive freeze mutableArrays

{-# INLINEABLE ctypesToArrays' #-}
ctypesToArrays' ::
  forall v s f g mv.
  (mv ~ V.Mutable v, forall a. IsPrimitive a => MV.MVector mv (f a), Foldable g) =>
  Arrays (ArrayMVec v s f) ->
  g (CType f) ->
  ST s (Maybe ToArraysError)
ctypesToArrays' mutableArrays ctypes = do
  arrayCountRef <- newSTRef mempty
  let arraysFuns = toArraysFuns mutableArrays arrayCountRef
  result <- runExceptT (F.traverse_ (traverseCType_ arraysFuns) ctypes)
  pure $ case result of
    Left err -> Just err
    Right () -> Nothing

-- NB: This ConstraintHK1 constraint _must_ be written this way, I think due to limitations of how
-- quantified constraints are solved.
-- See https://gitlab.haskell.org/ghc/ghc/issues/14860 for more info.
toArraysFuns ::
  forall v s f mv.
  (mv ~ V.Mutable v, forall a. IsPrimitive a => MV.MVector mv (f a)) =>
  Arrays (ArrayMVec v s f) ->
  STRef s (Arrays ArrayCount) ->
  CTraversalFuns_ (ExceptT ToArraysError (ST s)) f
toArraysFuns mutableArrays arraysCountRef = arraysFuns
  where
    arraysFuns =
      CTraversalFuns_
        { ctfHandlePrim_ = Barbies.btraverseC_ @IsPrimitive primFun,
          ctfHandleEnum_ = enumFun,
          ctfHandleUnionCon_ = unionConFun
        }
    enumFun (CEnum _ _ cprim) = do
      counts0 <- lift (readSTRef arraysCountRef)
      counts1 <- f counts0 cprim (view prims_ mutableArrays)
      lift (writeSTRef arraysCountRef $ force counts1)
    f ::
      forall a.
      IsPrimitive a =>
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
    primFun :: forall a. IsPrimitive a => f a -> ExceptT ToArraysError (ST s) ()
    primFun cprim = do
      counts0 <- lift (readSTRef arraysCountRef)
      counts1 <- f counts0 cprim (view (prims_ @a) mutableArrays)
      lift (writeSTRef arraysCountRef $ force counts1)
    unionConFun :: CUnion f -> ExceptT ToArraysError (ST s) ()
    unionConFun cunion@(CUnion _ _ tag ccon) = do
      -- write the tag
      primFun tag
      -- get the counts before writing the con
      counts0 <- lift (readSTRef arraysCountRef)
      -- write the con
      traverseCCon_ arraysFuns ccon
      -- write the padding
      let maxConLengths = maxUnionConLengths cunion
          counts1 = counts0 <> maxConLengths
      counts1 `seq` lift (writeSTRef arraysCountRef counts1)
{-# INLINEABLE toArraysFuns #-}
