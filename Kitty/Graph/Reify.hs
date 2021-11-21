{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | This version of observable sharing recovery is generalized to the case of an AST with embedded
-- strong typing.  Everything is built around the @Arrays@ type, which abstracts over the set of
-- primitive types allowed.  'reifyGraph' converts these expression values to a graph
-- representation, recovering sharing along the way.  The expression functor @f@ has type
-- @(Type -> Type) -> Type -> Type@.  The conversion changes expressions from @f t a@
-- to @f ('Const' 'Int') a@ for all @IsPrimitive a@.  @f@ must be an instance of 'HTraversableC'
-- so that we know how to do this swap in a type-safe and effectful way on each node.
--
-- Other changes relative to the original flavor:
--
--  - 'IORef' instead of 'Control.Concurrent.MVar.MVar', as we are not doing any concurrency
--    and have no need for locking.
--
--  - TODO(MP): The 'IntMap'-plus-list structure is used because 'StableName's have no 'Ord'
--    instance and can't be in a 'Data.Map.Map'.  Is there anything cleverer to be done here?
--
--  - TODO(MP): Maybe Greg Pfeil can provide guidance on how this could be a recursion scheme
--    for some kind of higher effectful algebra
module Kitty.Graph.Reify
  ( -- * Graph type
    Graph (..),

    -- * Operations on graphs
    reifyGraph,
    nullGraphNodes,
    nullGraphIdArrays,
  )
where

import qualified Barbies
import qualified Control.Lens as Lens (view)
import Data.Bifunctor (first)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import Data.Monoid (Sum (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Kitty.CExpr.Types (CExprTypeLens (..), CExprTypeProduct (..))
import Kitty.Graph (Graph (..))
import Kitty.Higher (HTraversableC (..))
import Kitty.Prim (Arrays)
import Kitty.Recursion (NaturalTransformation, Projectable, hproject)
import System.Mem.StableName (StableName)
import qualified System.Mem.StableName as StableName

type ProductOfRefsTo v = CExprTypeProduct (Compose IORef v)

newtype StableNameMap (t :: Type -> Type) f a = StableNameMap (IntMap [(StableName (f t a), Int)])

emptyStableNameMap :: forall t f. IO (ProductOfRefsTo (StableNameMap t f))
emptyStableNameMap = Barbies.bsequence $ Barbies.bpure go
  where
    go :: forall x. Compose IO (Compose IORef (StableNameMap t f)) x
    go = Compose $ Compose <$> newIORef (StableNameMap IntMap.empty)

type ResultMap f = Compose IntMap (f (Const Int))

emptyResultMap :: forall t. IO (ProductOfRefsTo (ResultMap t))
emptyResultMap = Barbies.bsequence $ Barbies.bpure go
  where
    go :: forall x. Compose IO (Compose IORef (ResultMap t)) x
    go = Compose $ Compose <$> newIORef (Compose mempty)

findNodes ::
  forall a t f.
  (CExprTypeLens a, Projectable (NaturalTransformation (->)) t f, HTraversableC CExprTypeLens f) =>
  ProductOfRefsTo (StableNameMap t f) ->
  ProductOfRefsTo (ResultMap f) ->
  IORef Int ->
  ( forall c.
    CExprTypeLens c =>
    (forall b. t b -> t b -> IO Bool) ->
    f t c ->
    IO (Either (t c) (f t c))
  ) ->
  t a ->
  IO Int
findNodes stableNameMapVars resultMapVars counterVar normalize node = do
  -- Must evaluate 'normalized' to observe the sharing.
  normalized <- either hproject id <$> normalize normalizeEq (hproject node)
  st <- makeStableNameStrict normalized
  stableNameMap :: StableNameMap t f a <- readIORef stableNameMapVar
  case lookupStableName st stableNameMap of
    -- NB: without MVars we do not need to update anything here; the IORef hasn't changed.
    Just var -> pure var
    Nothing -> do
      result <- htraverseC @CExprTypeLens (fmap Const . onChild) normalized
      var <- newVarNumber counterVar
      modifyIORef' stableNameMapVar $
        insertStableName (StableName.hashStableName st) [(st, var)]
      modifyIORef' resultMapVar $ Compose . IntMap.insert var result . getCompose
      pure var
  where
    normalizeEq :: t b -> t b -> IO Bool
    normalizeEq a b = (==) <$> makeStableNameStrict a <*> makeStableNameStrict b
    -- Extract the component of a product corresponding to the current base type @a@
    indexThisType :: ProductOfRefsTo g -> IORef (g a)
    indexThisType = getCompose . Lens.view (cexprType_ @a)

    stableNameMapVar = indexThisType stableNameMapVars
    resultMapVar = indexThisType resultMapVars

    onChild :: forall x. CExprTypeLens x => t x -> IO Int
    onChild = findNodes stableNameMapVars resultMapVars counterVar normalize

    insertStableName key val (StableNameMap snMap) =
      StableNameMap $ IntMap.insertWith (<>) key val snMap

    lookupStableName sn (StableNameMap snMap) =
      case IntMap.lookup (StableName.hashStableName sn) snMap of
        Just pairs -> lookup sn pairs
        Nothing -> Nothing

    newVarNumber ref = do
      v <- readIORef ref
      let v' = succ v
      writeIORef ref v'
      pure v'

-- | Given @Arrays@ full of 'Vector's of AST values built using fixed points of these strongly-typed
-- Naperian functors, this function will recover the underlying 'Graph', with sharing recovered.
reifyGraph ::
  forall t f.
  (Projectable (NaturalTransformation (->)) t f, HTraversableC CExprTypeLens f) =>
  ( forall m c.
    (Monad m, CExprTypeLens c) =>
    (forall b. t b -> t b -> m Bool) ->
    f t c ->
    m (Either (t c) (f t c))
  ) ->
  Arrays (Compose Vector t) ->
  IO (Graph f)
reifyGraph normalize asts = do
  stableNameMapVars :: ProductOfRefsTo (StableNameMap t f) <- emptyStableNameMap
  resultMapVars :: ProductOfRefsTo (ResultMap f) <- emptyResultMap
  counterVar <- newIORef 0
  let onArray :: forall a. CExprTypeLens a => Vector (t a) -> IO (Vector (Const Int a))
      onArray =
        traverse
          (fmap Const . findNodes stableNameMapVars resultMapVars counterVar normalize)
  roots <- Barbies.btraverseC @CExprTypeLens (fmap Compose . onArray . getCompose) asts
  nodes <- Barbies.btraverseC @CExprTypeLens (readIORef . getCompose) resultMapVars
  pure $ Graph nodes roots

nullGraphIdArrays ::
  (Barbies.FunctorB x, Monoid (x (Const (Sum Int))), Eq (x (Const Int))) =>
  x (Compose IntMap (t (Const Int))) ->
  Bool
nullGraphIdArrays arrs =
  Barbies.bmap (Const . IntMap.size . getCompose) arrs
    == Barbies.bmap (first getSum) mempty

-- | Are there any values in any of the given input vectors?
nullGraphNodes ::
  (Barbies.FunctorB x, Monoid (x (Const (Sum Int))), Eq (x (Const Int))) =>
  x (Compose Vector (t (Const Int))) ->
  Bool
nullGraphNodes arrs =
  Barbies.bmap (Const . Vector.length . getCompose) arrs
    == Barbies.bmap (first getSum) mempty

makeStableNameStrict :: a -> IO (StableName a)
makeStableNameStrict !a = StableName.makeStableName a
{-# ANN makeStableNameStrict "HLint: ignore Avoid restricted function" #-}
