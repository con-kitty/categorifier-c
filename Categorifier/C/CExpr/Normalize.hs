{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Categorifier.C.CExpr.Normalize
  ( normalize,
  )
where

import qualified Barbies
import Categorifier.C.CExpr.Types (CExprTypeLens (..), CExprTypeProduct (..), CmpOp (..))
import Categorifier.C.CExpr.Types.Core (CExprF (..))
import Categorifier.C.Prim (Arrays (..))
import qualified Control.Lens as Lens (view)
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Bool (bool)
import Data.Functor.Compose (Compose (..))

type IntermediateNormalization m r a = CExprF r a -> m (CExprF r a)

type TerminalNormalization m r a = CExprF r a -> m (Either (r a) (CExprF r a))

-- These newtype wrappers are only needed internally to `normalize`, so that we can store these
-- individual normalization passes inside a list within a higher-kinded functor (we can't partially
-- apply a type synonym as an argument of `Compose`).
newtype IntermediateWrapper m r a = IntermediateWrapper
  { unwrapIntermediate :: IntermediateNormalization m r a
  }

newtype TerminalWrapper m r a = TerminalWrapper
  { unwrapTerminal :: TerminalNormalization m r a
  }

-- | Apply all normalizations to a single `CExprF`. See the source of this module for the individual
--   normalizations that get applied.
normalize ::
  forall m r a.
  (Monad m, CExprTypeLens a) =>
  (forall b. r b -> r b -> m Bool) ->
  CExprF r a ->
  m (Either (r a) (CExprF r a))
normalize eq = runExceptT . (terminalNorms <=< lift . intermediateNorms)
  where
    -- Intermediate normalization passes
    intermediateNorms =
      foldr (<=<) pure
        . fmap unwrapIntermediate
        . (polyIntermediateNorms <>)
        . getCompose
        $ Lens.view (cexprType_ @a) monoIntermediateNorms
    -- Monomorphic intermediate normalization passes, which can only operate on terms of a single
    -- base type
    monoIntermediateNorms =
      emptyNormalizations
        { cexprTypeProductPrimitives =
            emptyNormalizations
              { arrayBool =
                  Compose $
                    fmap
                      IntermediateWrapper
                      [ identityCmp eq
                      ]
              }
        }
    -- Polymorphic intermediate normalization passes, which can operate on any base type.
    polyIntermediateNorms = fmap IntermediateWrapper mempty
    -- Terminal normalization passes
    terminalNorms =
      foldr (<=<) pure
        . fmap (fmap ExceptT . unwrapTerminal)
        . (polyTerminalNorms <>)
        . getCompose
        $ Lens.view (cexprType_ @a) monoTerminalNorms
    -- Monomorphic terminal normalization passes, which can only operate on terms of a single base
    -- type
    monoTerminalNorms = emptyNormalizations
    -- Polymorphic terminal normalization passes, which can operate on any base type.
    polyTerminalNorms =
      fmap
        TerminalWrapper
        [ equivalentBranch eq
        ]

    emptyNormalizations :: Barbies.ApplicativeB x => x (Compose [] (n m r))
    emptyNormalizations = Barbies.bpure (Compose mempty)

-- | If we're comparing something to itself, it's always a static `Bool`.
identityCmp ::
  Applicative f =>
  (forall a. r a -> r a -> f Bool) ->
  IntermediateNormalization f r Bool
identityCmp eq c = case c of
  CmpOpF op l r ->
    bool
      c
      ( LitF $ case op of
          CmpEq -> True
          CmpNeq -> False
          CmpGT -> False
          CmpLT -> False
          CmpGE -> True
          CmpLE -> True
      )
      <$> l `eq` r
  _ -> pure c

-- | If both sides of the branch are the same, elide the branch.
equivalentBranch :: Applicative f => (r a -> r a -> f Bool) -> TerminalNormalization f r a
equivalentBranch eq c = case c of
  BranchF _ l r -> bool (pure c) (Left l) <$> l `eq` r
  _ -> pure $ pure c
