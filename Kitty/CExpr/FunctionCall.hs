{-# LANGUAGE TypeFamilies #-}
-- There are @`KFunCall` `CExpr`@ and @`KForeignFunctionCall` `CExpr`@ orphans here.
-- __TODO__: Make them non-orphans and get rid of @import Kitty.CExpr.FunctionCall ()@
{-# OPTIONS_GHC -Wno-orphans #-}

module Kitty.CExpr.FunctionCall
  ( computeSpec,
    mkInputIndices,
    mkInputs,
  )
where

import qualified Barbies
import Data.Coerce (coerce)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Kitty.CExpr.Cat.TargetOb (TargetOb)
import Kitty.CExpr.Types.Core
  ( CExpr,
    CExprF (..),
    FunctionCall (..),
    PrimTypeLens,
  )
import qualified Kitty.CExpr.Types.Core as CExpr
import Kitty.KTypes.Function
  ( ArraysVec,
    Callee (..),
    IsFunCall,
    KForeignFunctionCall (..),
    KFunCall (..),
    KVariadicVectorizeFunctionInputs (..),
    accumulateInputsAndCallArraysFunction,
  )
import Kitty.Prim (ArrayCount (..), Arrays, IsPrimitive)
import Kitty.Recursion (hembed)

instance KFunCall CExpr where
  kCallFunctionWithSpec name _inspec outspec fun input =
    Barbies.bmapC @PrimTypeLens fillArray outspec
    where
      mkSymbolicInputs = mkInputs . mkInputIndices . computeSpec
      fillArray :: forall a. PrimTypeLens a => ArrayCount a -> Compose Vector CExpr a
      fillArray (ArrayCount n) = Compose . Vector.generate n $ hembed . FunctionCallOutputF call
      symbolicGraph = fun $ mkSymbolicInputs input
      call =
        hembed @_ @CExpr . FunctionCallF $
          FunctionCall (Text.pack name) input (CExpr.Generated symbolicGraph)

instance KForeignFunctionCall CExpr where
  type KFFCall CExpr a = TargetOb a

  kffcall ::
    forall isFunCall a b.
    ( KVariadicVectorizeFunctionInputs isFunCall CExpr (TargetOb a -> TargetOb b),
      isFunCall ~ IsFunCall (TargetOb a -> TargetOb b)
    ) =>
    Proxy CExpr ->
    Text ->
    Callee ->
    Maybe (a -> b) ->
    (TargetOb a -> TargetOb b)
  kffcall Proxy name callee _f =
    vectorizeFunctionInputs
      (Proxy @isFunCall)
      (accumulateInputsAndCallArraysFunction functionNode)
      mempty
      (const $ pure ())
    where
      functionNode :: Arrays ArrayCount -> Arrays ArrayCount -> ArraysVec CExpr -> ArraysVec CExpr
      functionNode _inspec = mkFunctionCallF name callee

-- | Create a function argument specification using the lengths of the given vectors.
computeSpec :: forall a b. Barbies.FunctorB b => b (Compose Vector a) -> b (Const Int)
computeSpec = Barbies.bmap (Const . Vector.length . getCompose)

-- | This function is only suitable for a single input 'Arrays (Const Int)'.
mkInputIndices :: Arrays (Const Int) -> Arrays (Compose Vector (Const Int))
mkInputIndices = Barbies.bmapC @IsPrimitive gen
  where
    gen :: forall a. Const Int a -> Compose Vector (Const Int) a
    gen (Const n) = Compose (Vector.generate n Const)

-- | Convert an input spec into an 'Arrays' of 'CExpr' nodes that know where in an
-- 'Arrays' to get their input from.  Once you have this 'Arrays', you can turn a Haskell function
-- of type 'Inputs -> Outputs' into a full graph.
mkInputs ::
  Arrays (Compose Vector (Const Int)) ->
  Arrays (Compose Vector CExpr)
mkInputs = Barbies.bmapC @IsPrimitive (Compose . fmap (hembed . InputF . getConst) . getCompose)

mkFunctionCallF ::
  Text ->
  Callee ->
  Arrays ArrayCount ->
  ArraysVec CExpr ->
  ArraysVec CExpr
mkFunctionCallF name callee outspec input = Barbies.bmapC @PrimTypeLens fillArray outspec
  where
    fillArray :: forall a. PrimTypeLens a => ArrayCount a -> Compose Vector CExpr a
    fillArray (ArrayCount n) = Compose . Vector.generate n $ hembed . FunctionCallOutputF call
    call =
      hembed @_ @CExpr . FunctionCallF
        . FunctionCall name input
        $ case callee of
          Imported header -> CExpr.Imported outspec' header
          HandWritten headers decl defn -> CExpr.HandWritten outspec' headers decl defn
    outspec' = Barbies.bmap coerce outspec
