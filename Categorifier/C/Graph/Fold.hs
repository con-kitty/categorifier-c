{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Categorifier.C.Graph.Fold
  ( NaturalTransformationMC,
    HAlgebraMC,
    HElgotAlgebraMC,
    nodeHcataM,
    nodeHezygoM,
  )
where

import qualified Barbies
import Categorifier.C.CExpr.Types (CExprTypeLens (..), CExprTypeProduct (..))
import Categorifier.C.Graph (Graph (..), GraphFailure (..))
import Categorifier.C.Higher (HFunctorC (hmapC), HTraversableC (htraverseC))
import Categorifier.C.Prim (Arrays)
import Categorifier.Duoidal (Parallel (..))
import Categorifier.Duoidal.Either (noteAccum)
import Control.Arrow ((&&&))
import Control.Lens (view, (%~))
import Control.Monad ((<=<))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.State (StateT (..), execStateT)
import Data.Bifunctor (bimap, first)
import Data.Bitraversable (bisequenceA)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Product (Product (Pair))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Vector (Vector)

type NaturalTransformationMC (c :: Type -> Constraint) m f g = forall a. c a => f a -> m (g a)

type HAlgebraMC (c :: Type -> Constraint) m h f = NaturalTransformationMC c m (h f) f

type HElgotAlgebraMC (c :: Type -> Constraint) m h (w :: (Type -> Type) -> Type -> Type) f =
  NaturalTransformationMC c m (w (h f)) f

-- | Fold a 'Graph' using an 'HAlgebraMC'. Returns a result, @IntMap (r a)@ for some @a@,
-- for each node in the graph, but does not return the results for the root nodes.
--
-- The monadic effect is only performed once per node.
nodeHcataM' ::
  forall
    (m :: Type -> Type)
    (e :: (Type -> Type) -> Type -> Type)
    (r :: Type -> Type).
  (HTraversableC CExprTypeLens e, Monad m) =>
  HAlgebraMC CExprTypeLens m e r ->
  Graph e ->
  m (Either GraphFailure (CExprTypeProduct (Compose IntMap r)))
nodeHcataM' alg =
  either
    (pure . Left . DuplicateKeys)
    ( runExceptT . flip execStateT (Barbies.bpure (Compose mempty))
        . traverse_ (StateT . applyAlgebra)
        -- Since a node's ID is bigger than the ID of any of its children, we
        -- convert the map to an asc list so that we process each node after
        -- all its children have been processed.
        . IntMap.toAscList
    )
    . getParallel
    . IntMap.traverseWithKey
      ( \k vs ->
          -- Check that we don't have keys that appear in multiple IntMaps.
          Parallel (case vs of (v :| []) -> Right v; _ -> Left (pure k))
      )
    . IntMap.unionsWith (<>)
    . Barbies.bfoldMapC @CExprTypeLens (pure @[] . IntMap.map (pure . Aux) . getCompose)
    . graphNodes
  where
    applyAlgebra ::
      (Int, Aux (e (Const Int))) ->
      CExprTypeProduct (Compose IntMap r) ->
      ExceptT
        GraphFailure
        m
        ((), CExprTypeProduct (Compose IntMap r))
    applyAlgebra (i, Aux (node :: e (Const Int) a)) maps =
      let modifyState :: r a -> CExprTypeProduct (Compose IntMap r)
          modifyState res = maps & cexprType_ %~ Compose . IntMap.insert i res . getCompose
       in fmap ((),) . swapMonad . fmap (fmap modifyState . alg) $ findChildren maps node

    findChildren ::
      forall a.
      CExprTypeLens a =>
      CExprTypeProduct (Compose IntMap r) ->
      e (Const Int) a ->
      Parallel (Either (NonEmpty Int)) (e r a)
    findChildren maps =
      htraverseC @CExprTypeLens $
        noteAccum (flip IntMap.lookup . getCompose $ view cexprType_ maps) . getConst

    swapMonad :: forall a. Parallel (Either (NonEmpty Int)) (m a) -> ExceptT GraphFailure m a
    swapMonad = ExceptT . sequenceA . first MissingReferences . getParallel

-- | Fold a 'Graph' using an 'HAlgebraMC'. Returns a result for each root
-- (@Vector (r a)@ where @a@ is the type of the root node), as well as the results for
-- each vertex in the graph (@IntMap (r a)@).
--
-- The monadic effect is only performed once per node.
nodeHcataM ::
  forall m e r nodes roots.
  ( HTraversableC CExprTypeLens e,
    Monad m,
    nodes ~ CExprTypeProduct (Compose IntMap r),
    roots ~ Arrays (Compose Vector r)
  ) =>
  HAlgebraMC CExprTypeLens m e r ->
  Graph e ->
  m (Either GraphFailure (nodes, roots))
nodeHcataM alg graph@(Graph _ roots) =
  either Left f <$> nodeHcataM' alg graph
  where
    -- Obtain the results for roots from the results for all nodes
    f :: nodes -> Either GraphFailure (nodes, roots)
    f nodes =
      bimap MissingRoots (nodes,) . getParallel $
        Barbies.btraverseC @CExprTypeLens (g nodes) roots

    -- For a given CExpr type 'a', lookup the results of the root nodes
    -- from the IntMaps.
    g ::
      forall a.
      CExprTypeLens a =>
      nodes ->
      Compose Vector (Const Int) a ->
      Parallel (Either (NonEmpty Int)) (Compose Vector r a)
    g nodes =
      fmap Compose
        . traverse (noteAccum (`IntMap.lookup` getCompose (view cexprType_ nodes)))
        . fmap getConst
        . getCompose

-- | Like 'nodeHcataM' but uses a helper algebra which feeds a monadic value at each step
-- to the main algebra.
nodeHezygoM ::
  forall m e r s nodes roots.
  ( HTraversableC CExprTypeLens e,
    Monad m,
    nodes ~ CExprTypeProduct (Compose IntMap r),
    roots ~ Arrays (Compose Vector r)
  ) =>
  HAlgebraMC CExprTypeLens m e s ->
  HElgotAlgebraMC CExprTypeLens m e (Product s) r ->
  Graph e ->
  m (Either GraphFailure (nodes, roots))
nodeHezygoM help alg = fmap (fmap extractResult) . nodeHcataM (evalInContext <=< pairWithContext)
  where
    pairWithContext :: forall a. CExprTypeLens a => e (Product s r) a -> m (s a, e r a)
    pairWithContext =
      bisequenceA . (help . hmapC @CExprTypeLens fstP &&& pure . hmapC @CExprTypeLens sndP)

    evalInContext :: forall a. CExprTypeLens a => (s a, e r a) -> m (Product s r a)
    evalInContext (sa, era) = Pair sa <$> alg (Pair sa era)

    extractResult =
      bimap
        (Barbies.bmap (Compose . fmap sndP . getCompose))
        (Barbies.bmap (Compose . fmap sndP . getCompose))

-- | The purpose of @Aux@ is to existentialize @a@ away, so that we can union all 'IntMap's,
-- one for each possible choice of @a@, into a single 'IntMap'.
data Aux f = forall a. CExprTypeLens a => Aux (f a)

fstP :: Product f g a -> f a
fstP (Pair x _) = x

sndP :: Product f g a -> g a
sndP (Pair _ y) = y
