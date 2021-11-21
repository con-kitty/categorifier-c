{-# LANGUAGE DerivingVia #-}

module Kitty.KTypes.CExpr.Generate
  ( generateCExprFunction,
    CExpr,
  )
where

import qualified Barbies
import Data.Coerce (coerce)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Text (Text)
import Data.Vector (Vector)
import Kitty.CExpr.File (FunctionText)
import Kitty.CExpr.Function (FunctionGenError, generateToplevelFunction)
import Kitty.CExpr.Types.Core (CExpr)
import Kitty.Prim
  ( ArrayCount (..),
    Arrays,
  )

generateCExprFunction ::
  Text ->
  Arrays ArrayCount ->
  (Arrays (Compose Vector CExpr) -> IO (Arrays (Compose Vector CExpr))) ->
  IO (Either FunctionGenError (FunctionText ann))
generateCExprFunction funName inputCounts f =
  generateToplevelFunction funName (Barbies.bmap coerce inputCounts) $
    fmap (Barbies.bmap coerce) . f . Barbies.bmap coerce
