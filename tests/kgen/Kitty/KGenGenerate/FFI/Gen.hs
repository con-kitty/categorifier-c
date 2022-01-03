{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | The core code-generation functions for 'KGen'.
module Kitty.KGenGenerate.FFI.Gen
  ( -- * Code generation of functions
    mkCgFromArraysFun,
    mkCgFromPolyVecFun,
    codeGenWithBindings,
    SBVSpec,
  )
where

import qualified Barbies
import Control.Monad (unless, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (StateT (..))
import Data.Functor.Compose (Compose (..))
import qualified Data.IORef as IORef
import Data.Proxy (Proxy (..))
import Data.SBV.Internals (SBV, SBVCodeGen (..))
import qualified Data.SBV.Internals as SBV
import qualified Data.SBV.Tools.CodeGen as SBV
import qualified Data.Text as Text (unpack)
import qualified Data.Vector as V
import qualified Kitty.Barbies as Barbies
import Kitty.Codegen.FFI.Spec (SBVSpec, Spec (..))
import qualified Kitty.Common.IO.Exception as Exception
import Kitty.KGen.KGen (KGen (..))
import Kitty.KGenGenerate.FFI.Bindings (mkFFIBindingModule)
import Kitty.KGenGenerate.FFI.Spec (cgStateToSpec, genFunSpec, specFileName)
import Kitty.KTypes.C (C (..))
import Kitty.KTypes.KLiteral (false)
import Kitty.PolyVec (PolyVec, pdevectorize, pvectorize, pvlengths)
import Kitty.Prim (ArrayCount (..), ArrayName (..), Arrays (..), arrayNames)
import qualified Text.Casing as Casing

-- | Convenience function to wrap `mkCgFromArraysFun` with PolyVec types.
mkCgFromPolyVecFun :: forall a b. (PolyVec KGen a, PolyVec KGen b) => (a -> b) -> SBVCodeGen SBVSpec
mkCgFromPolyVecFun polyVecFun = mkCgFromArraysFun arraysFun inputCounts
  where
    arraysFun =
      Exception.throwIOLeft . pvectorize @V.Vector @KGen @b . polyVecFun
        <=< Exception.throwIOLeft . pdevectorize @V.Vector @KGen @a
    inputCounts = pvlengths (Proxy @KGen) (Proxy @a)

-- Note to implementors: keep in mind that SBV cannot deal with empty input or output arrays, so
-- even if this function has arrays of zero size in the 'counts' argument we pass it, our resulting
-- 'SBVCodeGen' action (and the 'CgState' accumulated therein) will generate all arrays with a
-- minimum size of one. In other words, the generated code will _not_ exactly match the 'Arrays
-- ArrayCount' you get from polyvectorization. See 'arrayCountsToSpec' and 'FFIArrayCount' for some
-- more info.

-- | This function takes a polyvectorized function and its input specification and gives you an
-- 'SBVCodeGen' action you can pass to 'codeGenWithBindings' to generate the corresponding C code
-- and FFI tools.
mkCgFromArraysFun ::
  (Arrays (Compose V.Vector KGen) -> IO (Arrays (Compose V.Vector KGen))) ->
  Arrays ArrayCount ->
  SBVCodeGen SBVSpec
mkCgFromArraysFun f counts = do
  inputArrays <-
    Barbies.bzipWithMC @SBV.SymVal genInArr' arrayNames counts ::
      SBVCodeGen (Arrays (Compose V.Vector KGen))
  outputArrays <- liftIO $ f inputArrays
  let outcounts = Barbies.bmap (ArrayCount . V.length . getCompose) outputArrays
  Barbies.bzipWith3MC_ @SBV.SymVal
    genOutArr'
    arrayNames
    outputArrays
    outputsIfZeroLength
  SBV.cgOverwriteFiles True
  SBV.cgGenerateDriver False
  SBV.cgGenerateMakefile False
  pure $ Spec counts outcounts
  where
    genInArr' ::
      forall a. SBV.SymVal a => ArrayName a -> ArrayCount a -> SBVCodeGen (Compose V.Vector KGen a)
    genInArr' (ArrayName name) (ArrayCount count) =
      Compose <$> genInArr ("in_" <> Text.unpack name) count
    genInArr :: forall a. SBV.SymVal a => String -> Int -> SBVCodeGen (V.Vector (KGen a))
    genInArr name 0 = do
      _ <- SBV.cgInputArr 1 name :: SBVCodeGen [SBV a]
      pure V.empty
    genInArr name count =
      V.fromList . fmap KGen <$> SBV.cgInputArr (fromIntegral count) name
    genOutArr' ::
      forall a. SBV.SymVal a => ArrayName a -> Compose V.Vector KGen a -> C a -> SBVCodeGen ()
    genOutArr' (ArrayName name) (Compose out) outputIfZeroLength =
      genOutArr ("out_" <> Text.unpack name) out outputIfZeroLength
    genOutArr :: forall a. SBV.SymVal a => String -> V.Vector (KGen a) -> C a -> SBVCodeGen ()
    genOutArr name outArr outputIfZeroLength
      | V.null outArr = SBV.cgOutputArr name (pure . SBV.literal $ unsafeC outputIfZeroLength)
      | otherwise = SBV.cgOutputArr name (V.toList (fmap getKGen outArr))
    outputsIfZeroLength :: Arrays C
    outputsIfZeroLength =
      Arrays
        { arrayBool = false,
          arrayInt8 = 0,
          arrayInt16 = 0,
          arrayInt32 = 0,
          arrayInt64 = 0,
          arrayWord8 = 0,
          arrayWord16 = 0,
          arrayWord32 = 0,
          arrayWord64 = 0,
          arrayFloat = 0,
          arrayDouble = 0
        }

-- | 'codeGenWithBindings quiet path funName gen' generates C code and various associated FFI helper
-- tools:
--
--   * an FFI binding that allows calling the function directly from Haskell TODO(MP): Not yet
--     implemented
--   * a set of C functions that can be called to get the input and output specifications
--
-- @quiet@ determines whether stdout receives a lot of spam. @path@ determines where the code is
-- generated. @funName@ is the name of the function to be generated. This module attempts to deal
-- with the casing conventions in Haskell and C, so passing the Haskell-style @myFunctionName@ will
-- correspond to @my_function_name@ in the generated C code. @gen@ is the code-generation result,
-- which you can compute using 'mkCgFromArraysFun'.
--
-- The specification functions include one function to find the number of pointer arguments (which
-- should always be the number of primitive types for which we support code-generation; enums are
-- mapped to a 'Word' of some size) and one function to find the expected array sizes corresponding
-- to each of these pointers. There are two sets of such function pairs: one for the input
-- arguments and one for the output arguments.
codeGenWithBindings ::
  -- | @quiet@
  Bool ->
  -- | @path@
  Maybe FilePath ->
  -- | @funName@
  String ->
  -- | @gen@
  SBVCodeGen SBVSpec ->
  IO SBVSpec
codeGenWithBindings quiet path funName' fun = do
  stateStore <-
    IORef.newIORef $ error "Uninitialized stateStore in Kitty.KGenGenerate.FFI.codeGenWithBindings!"
  specStore <-
    IORef.newIORef $ error "Uninitialized specStore in Kitty.KGenGenerate.FFI.codeGenWithBindings!"
  codeGenWithState funName fun path stateStore specStore
  cgState <- IORef.readIORef stateStore
  cgSpec <- IORef.readIORef specStore
  let moduleText = mkFFIBindingModule funName cgSpec
  case path of
    Nothing -> pure ()
    Just pth -> do
      let fullPath = pth <> "/" <> moduleName <> ".hs"
          specSrcPath = pth <> "/" <> specFileName funName <> ".c"
          specHeaderPath = pth <> "/" <> specFileName funName <> ".h"
      unless quiet . putStrLn $
        "Haskell binding generation: \"" <> fullPath <> "\".."
      writeFile fullPath moduleText
      unless quiet . putStrLn $
        "SBV (KGen) spec helpers: \"" <> specSrcPath <> "\".."
      let (specSrc, specHeader) =
            genFunSpec funName $ cgStateToSpec cgState
      writeFile specSrcPath specSrc
      writeFile specHeaderPath specHeader
  pure cgSpec
  where
    funName = funName' <> "SBV"
    moduleName = Casing.pascal funName

extractState :: SBVCodeGen SBV.CgState
extractState = SBVCodeGen . StateT $ \st -> pure (st, st)

codeGenWithState ::
  String ->
  SBVCodeGen SBVSpec ->
  Maybe FilePath ->
  IORef.IORef SBV.CgState ->
  IORef.IORef SBVSpec ->
  IO ()
codeGenWithState funName fun path finalState specvar =
  SBV.compileToC path funName f
  where
    f = do
      spec <- fun
      liftIO $ IORef.writeIORef specvar spec
      extractState >>= liftIO . IORef.writeIORef finalState
