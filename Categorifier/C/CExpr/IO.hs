{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Categorifier.C.CExpr.IO
  ( emitCFunction,
    layoutOptions,
    prettyFunctionGenError,
  )
where

import Categorifier.C.CExpr.File (FunctionGenErrorInfo (..), FunctionText (..))
import Categorifier.C.CExpr.Function (FunctionGenError (..))
import Categorifier.C.Graph (GraphFailure (..))
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import Prettyprinter (Doc)
import qualified Prettyprinter as Doc
import qualified Prettyprinter.Render.Text as Doc
import PyF (fmt)
import System.IO (IOMode (WriteMode), withFile)

-- | Display a 'FunctionGenError' in a human-readable format.
prettyFunctionGenError :: FunctionGenError -> Text
prettyFunctionGenError (FunctionGenError name info) =
  name <> ": " <> case info of
    InvalidGraph gf -> case gf of
      MissingRoots xs ->
        [fmt|
Missing roots: {show (NonEmpty.toList xs)}
        |]
      MissingReferences xs ->
        [fmt|
Missing references: {show (NonEmpty.toList xs)}
        |]
      DuplicateKeys xs ->
        [fmt|
Duplicate keys: {show (NonEmpty.toList xs)}
        |]
    BogusInputNodes nodes ->
      [fmt|
Internal error ('{name}'):
    Expression graph contained bogus input nodes!
    {Text.pack (show nodes)}

This can happen if you call `kVariadicFunction "{name}"` on a closure rather
than a standalone function.  You can capture Haskell values that you don't care
about code-generating, but you cannot capture DSL values; these must all be
explicitly passed as inputs to the function.
    |]

layoutOptions :: Doc ann -> Doc.SimpleDocStream ann
layoutOptions = Doc.layoutSmart (Doc.LayoutOptions $ Doc.AvailablePerLine 100 1.0)

prepareCFunctionFiles ::
  Text ->
  FunctionText ann ->
  ((FilePath, Doc.SimpleDocStream ann), (FilePath, Doc.SimpleDocStream ann))
prepareCFunctionFiles (Text.unpack -> funName) (FunctionText headerBody sourceBody) =
  bimap
    applyLayout
    applyLayout
    ((headerName, headerBody), (sourceName, sourceBody))
  where
    applyLayout = fmap layoutOptions
    headerName = funName <> ".h"
    sourceName = funName <> ".c"

emitCFunction :: FilePath -> Text -> FunctionText ann -> IO (FilePath, FilePath)
emitCFunction directory funName =
  bitraverse emit emit . prepareCFunctionFiles funName
  where
    emit (path, stream) =
      let p = directory <> "/" <> path
       in withFile p WriteMode $ \hdl -> Doc.renderIO hdl stream $> p
