{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
-- Data.Text.Prettyprint.Doc is deprecated in prettyprinter-1.7
{-# OPTIONS_GHC -Wno-deprecations #-}

-- The assignment monad
module Categorifier.C.CExpr.C.Assignment
  ( AssignState (..),
    AssignTypeError (..),
    AssignVar,
    ArrayIndexOffset,
    prettyAssignVar,
  )
where

import Categorifier.C.CExpr.Types (CExprType)
import Categorifier.C.CExpr.Types.Core (CExprF, FunctionCall)
import Data.Functor.Classes (Show1)
import Data.Text.Prettyprint.Doc (Doc)
import qualified Data.Text.Prettyprint.Doc as Doc
import GHC.Generics (Generic)

-- | This is a counter for generating unique variable names.
type AssignVar = Int

type ArrayIndexOffset = Int

prettyAssignVar :: AssignVar -> Doc ann
prettyAssignVar x = "v" <> Doc.pretty x

newtype AssignState = AssignState {assignVar :: AssignVar}
  deriving (Show)

data AssignTypeError r = AssignTypeError
  { -- | __TODO__: Make this @`Data.List.NonEmpty.NonEmpty` `CExprType`@.
    _assignTypeErrorExpected :: [CExprType],
    _assignTypeErrorGot :: CExprType,
    _assignTypeErrorTerm :: Either AssignVar (CExprF r AssignState)
  }
  deriving (Generic)

instance
  (Show1 r, forall b. Show b => Show (r b), Show (FunctionCall r)) =>
  Doc.Pretty (AssignTypeError r)
  where
  pretty (AssignTypeError expected got t) =
    Doc.nest 8
      . (<> "'")
      $ "Expected type: "
        <> Doc.pretty (show expected)
        <> Doc.line
        <> "   Found type: "
        <> Doc.pretty (show got)
        <> Doc.line
        <> either
          (("In assigned variable '" <>) . Doc.pretty)
          (("In the graph node '" <>) . Doc.pretty . show)
          t
