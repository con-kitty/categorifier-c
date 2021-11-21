{-# LANGUAGE KindSignatures #-}

module Kitty.Graph (Graph (..), GraphFailure (..)) where

import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Vector (Vector)
import Kitty.CExpr.Types (CExprTypeProduct (..))
import Kitty.Prim (Arrays)

-- | A @Graph e@ is like an @Arrays (Compose Vector (HFix e))@, except that it is a graph
-- structure rather than a tree structure, in which sharings are observed. Each node
-- in the graph has a unique @Int@ ID, and may refer to other node IDs.
--
-- Type parameter @e@ is usually instantiated to 'Kitty.CExpr.Types.Core.CExprF'.
--
-- == Invariants
--
--    * A node only refers to IDs that are smaller than its own.
--
--    * Each 'IntMap' key appears exactly once, i.e., no two 'IntMap's in 'graphNodes' have
--      any common key.
data Graph (e :: (Type -> Type) -> Type -> Type) = Graph
  { -- | The nodes: for each primitive type, 'Kitty.CExpr.Types.Core.CExprFunctionCall' and
    -- 'Kitty.CExpr.Types.Core.CExprSelectExpression', there is a map from the unique node ID
    -- to that node.
    graphNodes :: CExprTypeProduct (Compose IntMap (e (Const Int))),
    -- | The roots: for each primitive type, there is a vector of unique node IDs, each the ID of a
    -- result value of the C function this graph represents.
    graphRoots :: Arrays (Compose Vector (Const Int))
  }

-- | Cases where Graphs break the interface of recursion schemes.
data GraphFailure
  = -- | Some node IDs in 'graphRoots' are not found in 'graphNodes'.
    MissingRoots (NonEmpty Int)
  | -- | Some nodes refer to node IDs that do not exist in 'graphNodes'.
    MissingReferences (NonEmpty Int)
  | -- | Some keys appear in multiple 'IntMap's in 'graphNodes'.
    DuplicateKeys (NonEmpty Int)
  deriving (Show)
