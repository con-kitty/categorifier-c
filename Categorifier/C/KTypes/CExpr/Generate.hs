{-# LANGUAGE DerivingVia #-}

module Categorifier.C.KTypes.CExpr.Generate
  ( generateCExprFunction,
    CExpr,
  )
where

import qualified Barbies
import Categorifier.C.CExpr.File (FunctionText)
import Categorifier.C.CExpr.Function (FunctionGenError, generateToplevelFunction)
import Categorifier.C.CExpr.Types.Core (CExpr)
import Categorifier.C.Prim
  ( ArrayCount (..),
    Arrays,
  )
import Data.Coerce (coerce)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Const (Const (..))
import Data.Text (Text)
import Data.Vector (Vector)

generateCExprFunction ::
  -- | with self header file included or not
  Bool ->
  Text ->
  Arrays ArrayCount ->
  (Arrays (Compose Vector CExpr) -> IO (Arrays (Compose Vector CExpr))) ->
  IO (Either FunctionGenError (FunctionText ann))
generateCExprFunction withSelfHeader funName inputCounts f =
  generateToplevelFunction withSelfHeader funName (Barbies.bmap coerce inputCounts) $
    fmap (Barbies.bmap coerce) . f . Barbies.bmap coerce
