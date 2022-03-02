{-# LANGUAGE OverloadedStrings #-}

module Categorifier.C.Generate
  ( generateCFunction,
    generateCFunction',
    writeCFiles,
  )
where

import qualified Categorifier.C.CExpr.Cat as C
import Categorifier.C.CExpr.Cat.TargetOb (TargetOb)
import qualified Categorifier.C.CExpr.File as CExpr (FunctionText (..))
import qualified Categorifier.C.CExpr.IO as CExpr (layoutOptions, prettyFunctionGenError)
import Categorifier.C.CExpr.Types.Core (CExpr)
import Categorifier.C.KTypes.C (C)
import Categorifier.C.KTypes.CExpr.Generate (generateCExprFunction)
import Categorifier.C.PolyVec (PolyVec, pdevectorize, pvectorize, pvlengths)
import Categorifier.C.Prim (ArrayCount, Arrays)
import qualified Categorifier.Common.IO.Exception as Exception
import Control.Monad ((<=<))
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Prettyprint.Doc.Render.Text as Prettyprint
import Data.Vector (Vector)
import System.FilePath ((</>))

generateCFunction ::
  forall i o.
  (PolyVec CExpr (TargetOb i), PolyVec CExpr (TargetOb o), PolyVec C i) =>
  Text ->
  (i `C.Cat` o) ->
  IO [(Text, Text)]
generateCFunction name = generateCFunction' name (inputDims $ Proxy @i) . arraysFun

generateCFunction' ::
  Text ->
  Arrays ArrayCount ->
  (Arrays (Compose Vector CExpr) -> IO (Arrays (Compose Vector CExpr))) ->
  IO [(Text, Text)]
generateCFunction' funName inputSizes =
  either
    (Exception.throwIOAsExceptionWithCallStack (Text.unpack . CExpr.prettyFunctionGenError))
    ( \(CExpr.FunctionText headerText srcText) ->
        pure [(funName <> ".h", render headerText), (funName <> ".c", render srcText)]
    )
    <=< generateCExprFunction funName inputSizes
  where
    render = Prettyprint.renderStrict . CExpr.layoutOptions

arraysFun ::
  forall i o.
  (PolyVec CExpr (TargetOb i), PolyVec CExpr (TargetOb o)) =>
  (i `C.Cat` o) ->
  Arrays (Compose Vector CExpr) ->
  IO (Arrays (Compose Vector CExpr))
arraysFun f =
  Exception.throwIOLeft . pvectorize . C.lowerCat f <=< Exception.throwIOLeft . pdevectorize

inputDims :: forall a. PolyVec C a => Proxy a -> Arrays ArrayCount
inputDims = pvlengths (Proxy @C)

writeFiles :: Text -> [(Text, Text)] -> IO ()
writeFiles dir =
  traverse_ $ \(path, contents) -> do
    Text.writeFile (Text.unpack dir </> Text.unpack path) contents

writeCFiles ::
  (PolyVec CExpr (TargetOb i), PolyVec CExpr (TargetOb o), PolyVec C i) =>
  Text ->
  Text ->
  (i `C.Cat` o) ->
  IO ()
writeCFiles dir filename morphism =
  writeFiles dir =<< generateCFunction filename morphism
