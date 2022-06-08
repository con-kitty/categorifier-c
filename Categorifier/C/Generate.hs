{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Categorifier.C.Generate
  ( generateCFunction,
    generateCFunction',
    writeCFiles,
    writeCWrapperFiles,
    writeCWrapperFilesWithConfig,
  )
where

import qualified Categorifier.C.CExpr.Cat as C
import Categorifier.C.CExpr.Cat.TargetOb (TargetOb)
import qualified Categorifier.C.CExpr.File as CExpr (FunctionText (..))
import Categorifier.C.CExpr.Function (FunctionGenMode (Standard))
import qualified Categorifier.C.CExpr.IO as CExpr (layoutOptions, prettyFunctionGenError)
import Categorifier.C.CExpr.Types.Core (CExpr)
import Categorifier.C.CTypes.ArrayLengths (showMismatches)
import Categorifier.C.CTypes.Codegen.Render.Render (RenderedFile (..))
import Categorifier.C.CTypes.Codegen.Run (renderCTypesModules)
import Categorifier.C.CTypes.Types (CxxType (..))
import Categorifier.C.Codegen.Cxx.WrapKGenCFunction
  ( ArrayConversionFailure (..),
    CheckFinite (..),
    FunctionExporterConfig (..),
    toKGenWrapper,
  )
import Categorifier.C.Codegen.ToKioTypes (KioType (..), ToKioTypes (toKioTypes))
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
import PyF (fmt)
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
    <=< generateCExprFunction Standard funName inputSizes
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

writeCWrapperFiles ::
  forall i o.
  (PolyVec CExpr (TargetOb i), PolyVec CExpr (TargetOb o), ToKioTypes i, ToKioTypes o) =>
  -- | dir
  Text ->
  -- | wrapper C file name
  Text ->
  -- | low-level C file name
  Text ->
  (i `C.Cat` o) ->
  IO ()
writeCWrapperFiles =
  writeCWrapperFilesWithConfig
    FunctionExporterConfig
      { fecGenerateTimingInfo = False,
        fecCheckFinite = Don'tCheckFinite
      }

writeCWrapperFilesWithConfig ::
  forall i o.
  (PolyVec CExpr (TargetOb i), PolyVec CExpr (TargetOb o), ToKioTypes i, ToKioTypes o) =>
  FunctionExporterConfig ->
  -- | dir
  Text ->
  -- | wrapper C file name
  Text ->
  -- | low-level C file name
  Text ->
  (i `C.Cat` o) ->
  IO ()
writeCWrapperFilesWithConfig config dir wrapperName lowLevelName _morphism = do
  let inputIOTypes = toKioTypes (Proxy @i)
      outputIOTypes = toKioTypes (Proxy @o)
      cxxTypes :: [CxxType Proxy]
      cxxTypes = fmap (CxxTypeCType . kiotCType) (inputIOTypes <> outputIOTypes)
  ctypesModules <-
    either
      ( Exception.throwIOAsException $ \err ->
          "Error from renderCTypesModules: " <> show err
      )
      pure
      $ renderCTypesModules cxxTypes
  (wrapperSrc, wrapperHeader) <-
    either
      ( Exception.throwIOAsException $ \case
          InputArraysMismatch types mismatches ->
            "internal error: total input array lengths for ("
              <> Text.unpack (Text.intercalate ", " (Text.pack . show . kiotRep <$> types))
              <> ") computed in Haskell (left) and C (right) didn't match\n"
              <> Text.unpack (showMismatches mismatches)
          OutputArraysMismatch types mismatches ->
            "internal error: total output array lengths for ("
              <> Text.unpack (Text.intercalate ", " (Text.pack . show . kiotRep <$> types))
              <> ") computed in Haskell (left) and C (right) didn't match\n"
              <> Text.unpack (showMismatches mismatches)
          InternalConstructorMismatches mismatches ->
            foldMap (Text.unpack . uncurry (<>) . fmap showMismatches) mismatches
      )
      pure
      $ toKGenWrapper config inputIOTypes outputIOTypes wrapperName lowLevelName
  writeFiles dir $
    fmap (\(RenderedFile fileName contents) -> (fileName, contents)) ctypesModules
  writeFiles
    dir
    [ ([fmt|{wrapperName}.c|], wrapperSrc),
      ([fmt|{wrapperName}.h|], wrapperHeader)
    ]
