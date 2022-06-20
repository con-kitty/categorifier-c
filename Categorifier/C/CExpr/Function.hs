{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Top-level interface to generating code with statically typed expression functors
module Categorifier.C.CExpr.Function
  ( generateToplevelFunction,
    makeFunctionInputs,
    reifyFunction,
    reifyFunctionWithChildren',
    FunctionGenError (..),
    FunctionGenErrorInfo (..),
    FunctionGenMode (..),
    FunctionSet (..),
  )
where

import qualified Barbies
import Categorifier.C.CExpr.C (wrapWithExternC)
import Categorifier.C.CExpr.File
  ( FunctionGenErrorInfo (..),
    FunctionText (..),
    ReadyToGenerate (..),
    cExprHeaders,
    generateFunctionText,
    includeUserHeader,
  )
import Categorifier.C.CExpr.FunctionCall (computeSpec, mkInputIndices, mkInputs)
import Categorifier.C.CExpr.Normalize (normalize)
import Categorifier.C.CExpr.Types (CExprFunctionCall, CExprTypeLens (..), CExprTypeProduct (..))
import Categorifier.C.CExpr.Types.Core
  ( CExpr,
    CExprF (..),
    CExprHaskellFunction,
    Callee (..),
    FunctionCall (..),
  )
import Categorifier.C.Graph.Reify
  ( Graph (..),
    nullGraphIdArrays,
    reifyGraph,
  )
import Categorifier.C.Prim (Arrays, IsPrimitive)
import Categorifier.C.Recursion (hembed)
import Categorifier.Common.IO.Exception (Exception)
import Control.Monad (unless, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Data.Bifunctor (first)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List.Extra (nubOrd)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Prettyprinter as Doc
import PyF (fmt)

data FunctionGenMode
  = -- | separate header code and C source code
    Standard
  | -- | generated code is piled into a single source
    AllInOne

data FunctionGenError = FunctionGenError
  { functionGenErrorName :: Text,
    functionGenErrorInfo :: FunctionGenErrorInfo
  }
  deriving (Show)

instance Exception FunctionGenError

generateTopLevelFunction' ::
  FunctionGenMode ->
  Text ->
  Set ReadyToGenerate ->
  Either FunctionGenError (FunctionText ann)
generateTopLevelFunction' mode functionName rtg = do
  FunctionText topLevelHeader topLevelSource <-
    fmap mconcat . traverse (first (FunctionGenError functionName) . generateFunctionText) $
      Set.toList rtg
  let funcallHeaders =
        fmap Doc.pretty . nubOrd
          . foldMap
            ( \case
                ReadyToGenerate _ _ _ (Imported _ hdr) -> [hdr]
                ReadyToGenerate _ _ _ (HandWritten _ hdrs _ _) -> hdrs
                _ -> []
            )
          $ Set.toList rtg
      userHeaders =
        let headerList =
              case mode of
                Standard -> Doc.pretty (functionName <> ".h") : funcallHeaders
                AllInOne -> funcallHeaders
         in foldr
              (\inc acc -> acc <> Doc.line <> includeUserHeader inc)
              mempty
              headerList
      fullHdr =
        case mode of
          Standard -> Doc.vcat [spam, cExprHeaders, wrapWithExternC topLevelHeader]
          AllInOne -> Doc.emptyDoc
      fullSrc = Doc.vcat [spam, cExprHeaders, userHeaders, topLevelSource]
  pure $ FunctionText fullHdr fullSrc
  where
    spam =
      [fmt|
/* This file was AUTOMATICALLY GENERATED from a collection of Haskell expressions. */
/* Any modification you make here WILL BE OVERWRITTEN in short order. */
/* This function was generated using the `CExpr` DSL defined in `code_generation/ktypes`. */
/* See `generateCExprFunction` for the top-level entry point. */
/* Questions or bugs?  Please open a Jira ticket against the Tools team. */
          |]

generateToplevelFunction ::
  forall ann.
  FunctionGenMode ->
  Text ->
  Arrays (Const Int) ->
  CExprHaskellFunction IO ->
  IO (Either FunctionGenError (FunctionText ann))
generateToplevelFunction mode functionName inputSpec f =
  runExceptT $
    except . generateTopLevelFunction' mode functionName
      =<< reifyFunctionWithChildren functionName inputSpec f

makeFunctionInputs :: Arrays (Const Int) -> Arrays (Compose Vector CExpr)
makeFunctionInputs = Barbies.bmapC @IsPrimitive gen
  where
    gen :: forall a. IsPrimitive a => Const Int a -> Compose Vector CExpr a
    gen (Const n) = Compose $ Vector.generate n (hembed . InputF)

-- This is somewhat confusing because we should never be able to find
findInvalidInputs ::
  Arrays (Const Int) ->
  CExprTypeProduct (Compose IntMap (CExprF (Const Int))) ->
  Arrays (Compose IntMap (CExprF (Const Int)))
findInvalidInputs spec (CExprTypeProduct prims _funs _selects) =
  Barbies.bzipWithC @CExprTypeLens go spec prims
  where
    go ::
      Const Int a ->
      Compose IntMap (CExprF (Const Int)) a ->
      Compose IntMap (CExprF (Const Int)) a
    go (Const thisInputCount) = Compose . IntMap.filter outOfRange . getCompose
      where
        outOfRange (InputF idx) = idx < 0 || idx >= thisInputCount
        outOfRange _ = False

reifyFunction ::
  -- | name
  Text ->
  -- | input specification
  Arrays (Const Int) ->
  -- | graph
  Arrays (Compose Vector CExpr) ->
  ExceptT FunctionGenError IO (Arrays (Const Int), Graph CExprF)
reifyFunction name inputSpec outputs = do
  graph <- lift $ reifyGraph normalize outputs
  let invalidInputNodes = findInvalidInputs inputSpec $ graphNodes graph
  -- TODO(MP): Should we perhaps also look through the other nodes and see whether they look OK?
  unless (nullGraphIdArrays invalidInputNodes) . failToReify $ BogusInputNodes invalidInputNodes
  pure (computeSpec outputs, graph)
  where
    failToReify = except . Left . FunctionGenError name

reifyFunctionWithChildren' ::
  Text ->
  Arrays (Const Int) ->
  Arrays (Compose Vector CExpr) ->
  ExceptT FunctionGenError IO (DList ReadyToGenerate)
reifyFunctionWithChildren' name inputSpec outputs = do
  (outputSpec, exprGraph@(Graph nodes _)) <- reifyFunction name inputSpec outputs
  let readyFunction =
        ReadyToGenerate inputSpec outputSpec name (Generated exprGraph)
      calledFunctions :: [FunctionCall (Const Int)]
      calledFunctions =
        IntMap.elems . IntMap.mapMaybe getFunctionCall
          . getCompose
          $ cexprTypeProductFunction nodes

      getFunctionCall :: forall r. CExprF r CExprFunctionCall -> Maybe (FunctionCall r)
      getFunctionCall = \case
        FunctionCallF call -> Just call
        -- This case should be unreachable, but it's difficult to convince GHC that this is
        -- the case.
        _ -> Nothing

      genChildren ::
        FunctionCall (Const Int) ->
        ExceptT FunctionGenError IO (DList ReadyToGenerate)
      genChildren (FunctionCall name' inputs callee) = case callee of
        Generated body -> reifyFunctionWithChildren' name' (computeSpec inputs) body
        Imported outspec header ->
          pure . pure $ ReadyToGenerate (computeSpec inputs) outspec name' (Imported outspec header)
        HandWritten outspec hdrs decl defn ->
          pure . pure $
            ReadyToGenerate (computeSpec inputs) outspec name' (HandWritten outspec hdrs decl defn)

  children :: DList ReadyToGenerate <- mconcat <$> traverse genChildren calledFunctions
  pure $ children `DList.append` pure readyFunction

reifyFunctionWithChildren ::
  Text ->
  Arrays (Const Int) ->
  CExprHaskellFunction IO ->
  ExceptT FunctionGenError IO (Set ReadyToGenerate)
reifyFunctionWithChildren functionName inputSpec f =
  fmap (Set.fromList . DList.toList) . reifyFunctionWithChildren' functionName inputSpec <=< lift $
    f symbolicInputs
  where
    symbolicInputs = mkInputs $ mkInputIndices inputSpec

data FunctionSet = FunctionSet
  { -- | This set is a fast cache which lets us avoid actually computing any hashes in most cases.
    stableNameSet :: IntSet,
    -- | This stores the actual set of functions we need to generate later.
    hashSet :: HashSet (FunctionCall (CExprF (Const Int)))
  }

instance Semigroup FunctionSet where
  FunctionSet snsa hsa <> FunctionSet snsb hsb = FunctionSet (snsa <> snsb) (hsa <> hsb)

instance Monoid FunctionSet where
  mempty = FunctionSet IntSet.empty HashSet.empty
