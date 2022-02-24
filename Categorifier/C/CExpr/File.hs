{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- Data.Text.Prettyprint.Doc is deprecated in prettyprinter-1.7
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | File I/O with CExpr-generated code.
module Categorifier.C.CExpr.File
  ( cExprHeaders,
    includeSystemHeader,
    includeUserHeader,
    generateFunctionText,
    FunctionGenErrorInfo (..),
    FunctionText (..),
    ReadyToGenerate (..),
  )
where

import qualified Barbies
import Categorifier.C.CExpr.C
  ( genAssignment,
    genFunctionDeclaration,
    genFunctionDefinition,
    genOutputAssignment,
  )
import Categorifier.C.CExpr.C.Assignment (ArrayIndexOffset, AssignState (..), AssignVar)
import Categorifier.C.CExpr.Types (CExprTypeLens, CExprTypeProduct (..))
import Categorifier.C.CExpr.Types.Core (CExprF (..), Callee (..))
import Categorifier.C.Graph (Graph, GraphFailure (..))
import Categorifier.C.Graph.Fold (nodeHezygoM)
import Categorifier.C.Higher (HFunctorC (hmapC))
import Categorifier.C.Prim
  ( Arrays,
    IsPrimitive,
  )
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Bifunctor (bimap, first, second)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Product (Product (Pair))
import qualified Data.IntMap as IntMap
import Data.IntMap.Strict (IntMap)
import Data.List (sortOn)
import Data.Ord (comparing)
import Data.Semigroup (All (..))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, (<+>))
import qualified Data.Text.Prettyprint.Doc as Doc (angles, dquotes, line, pretty)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

data ReadyToGenerate = ReadyToGenerate
  { readyToGenerateInputSpec :: Arrays (Const Int),
    readyToGenerateOutputSpec :: Arrays (Const Int),
    readyToGenerateFilename :: !Text,
    readyToGenerateCallee :: Callee (Graph CExprF)
  }

instance Eq ReadyToGenerate where
  a == b =
    getAll $
      fold
        [ eqOn readyToGenerateFilename,
          eqOn readyToGenerateInputSpec,
          eqOn readyToGenerateOutputSpec
        ]
    where
      eqOn :: Eq t => (ReadyToGenerate -> t) -> All
      eqOn f = All $ ((==) `on` f) a b

instance Ord ReadyToGenerate where
  compare =
    mconcat
      [ comparing readyToGenerateFilename,
        comparing readyToGenerateInputSpec,
        comparing readyToGenerateOutputSpec
      ]

data FunctionGenErrorInfo
  = BogusInputNodes (Arrays (Compose IntMap (CExprF (Const Int))))
  | InvalidGraph GraphFailure

--  | AssignmentError GenError

data FunctionText ann = FunctionText
  { -- | Header text
    functionTextDeclaration :: Doc ann,
    -- | Source text
    functionTextDefinition :: Doc ann
  }
  deriving (Functor)

instance Semigroup (FunctionText ann) where
  FunctionText h1 s1 <> FunctionText h2 s2 =
    FunctionText (h1 <> Doc.line <> h2) (s1 <> Doc.line <> s2)

instance Monoid (FunctionText ann) where
  mempty = FunctionText mempty mempty

  mconcat texts = FunctionText decls defs
    where
      decls = mconcat $ fmap functionTextDeclaration texts
      defs = mconcat $ fmap functionTextDefinition texts

deriving instance Show (FunctionText ann)

type R ann = Const (AssignState, Doc ann)

-- | This generates the body text implementing a function.
generateFunctionBody ::
  forall ann.
  Graph CExprF ->
  -- | pretty-printed function definition
  Either FunctionGenErrorInfo (Doc ann)
generateFunctionBody =
  bimap InvalidGraph buildFunctionBody
    . flip State.evalState 0
    . collectAssignStatements
  where
    buildFunctionBody ::
      ( CExprTypeProduct (Compose IntMap (R ann)),
        Arrays (Compose Vector (R ann))
      ) ->
      Doc ann
    buildFunctionBody (nodes, roots) =
      let nodeDocs :: [(Int, Doc ann)]
          nodeDocs =
            Barbies.bfoldMap (fmap (second (snd . getConst)) . IntMap.toList . getCompose) nodes
          rootDocs =
            buildOutputArray
              (Barbies.bmap (Compose . fmap (first (assignVar . fst)) . getCompose) roots)
       in -- node docs must be concatenated in ascending order of node ID.
          mconcat (fmap snd (sortOn fst nodeDocs) <> rootDocs)

    -- Generate assignments for the output arrays, e.g., `output_double[7] = v42`.
    buildOutputArray ::
      Arrays (Compose Vector (Const AssignVar)) ->
      [Doc ann]
    buildOutputArray =
      Barbies.bfoldMapC @IsPrimitive
        (\vec -> uncurry (genOutputAssignment vec) <$> createIndex (getCompose vec))

    assignVariable = const (State.get <* State.modify succ)

    collectAssignStatements ::
      Graph CExprF ->
      State
        AssignVar
        ( Either
            GraphFailure
            ( CExprTypeProduct (Compose IntMap (R ann)),
              Arrays (Compose Vector (R ann))
            )
        )
    collectAssignStatements =
      let -- `helperAlg` increments the variable subscript, so that each assigned variable
          -- has a unique name.
          helperAlg ::
            forall a.
            CExprF (Const AssignVar) a ->
            State AssignVar (Const AssignVar a)
          helperAlg = fmap Const . assignVariable

          -- `mainAlg` generates an assignment statement. Given the subscript of the variable
          -- (e.g., 42) and the subscripts of its children (e.g., 40 and 41), it generates
          -- a statement like `const double v42 = v40 + v41`.
          mainAlg ::
            forall a.
            CExprTypeLens a =>
            Product (Const AssignVar) (CExprF (R ann)) a ->
            State AssignVar (Const (AssignState, Doc ann) a)
          mainAlg (Pair x y) = pure . Const $ genAssignStatement (getConst x) y
       in -- `nodeHezygoM` uses the value from its first argument as an input to its second argument
          nodeHezygoM helperAlg mainAlg

    -- Massages `genAssignment` into an algebra suitable for folding.
    genAssignStatement ::
      forall a.
      CExprTypeLens a =>
      AssignVar ->
      CExprF (R ann) a ->
      (AssignState, Doc ann)
    genAssignStatement v = (AssignState v,) . genAssignment v . hmapC @CExprTypeLens (first fst)

    -- Creating the output map (from graph ID to array index)
    createIndex :: forall a. Vector (Const AssignVar a) -> [(AssignVar, ArrayIndexOffset)]
    createIndex = Vector.ifoldl' indexOutput mempty

    indexOutput ::
      [(AssignVar, ArrayIndexOffset)] ->
      Int ->
      Const AssignVar a ->
      [(AssignVar, ArrayIndexOffset)]
    indexOutput acc idx (Const var) = (var, idx) : acc

generateFunctionText :: ReadyToGenerate -> Either FunctionGenErrorInfo (FunctionText ann)
generateFunctionText (ReadyToGenerate inputSpec outputSpec funName callee) = case callee of
  Imported _ _ -> Right mempty
  Generated graph ->
    let define =
          genFunctionDefinition inputSpec outputSpec (Doc.pretty funName)
            <$> generateFunctionBody graph
        decl = genFunctionDeclaration inputSpec outputSpec (Doc.pretty funName)
     in FunctionText decl <$> define
  HandWritten _ _ decl defn ->
    Right $ FunctionText (maybe mempty Doc.pretty decl) (Doc.pretty defn)

includeHeader :: Doc ann -> Doc ann
includeHeader hdr = "#include" <+> hdr

includeSystemHeader :: Doc ann -> Doc ann
includeSystemHeader = includeHeader . Doc.angles

includeUserHeader :: Doc ann -> Doc ann
includeUserHeader = includeHeader . Doc.dquotes

cExprSystemHeaders :: [Doc ann]
cExprSystemHeaders = ["stdint.h", "stdbool.h", "stdlib.h", "stdio.h", "math.h"]

cExprHeaders :: Doc ann
cExprHeaders =
  foldr (\inc acc -> acc <> Doc.line <> includeSystemHeader inc) mempty cExprSystemHeaders
