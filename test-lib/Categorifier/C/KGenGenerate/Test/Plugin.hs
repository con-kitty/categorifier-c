{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Head-to-head dynamically-loaded codegen testing.
--
-- In addition to catching many bugs in our own systems, this tool is
-- responsible for identifying the following bugs in external projects we
-- use:
--
--     * In the GCC compiler:
--
--         * <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=85811
--            Invalid optimization with @fmax@, @fabs@ and @NaN@>
--
--     * In the clang compiler (LLVM):
--
--        * <https://bugs.llvm.org/show_bug.cgi?id=37776
--           Invalid optimization with @fmax@ and @-inf@>
--
--        * <https://bugs.llvm.org/show_bug.cgi?id=46627
--           Invalid optimization with math library @fmax@ and signed zeros>
--
--     * In the GHC compiler's standard library:
--
--         * <https://mail.haskell.org/pipermail/ghc-devs/2018-June/015822.html
--            Severe inaccuracy of the 'asinh' and 'atanh' functions>
--
--     * In the SBV library:
--
--         * <https://github.com/LeventErkok/sbv/issues/382
--            Floating-point @Infinity@ sign handling>
--
--         * <https://github.com/LeventErkok/sbv/issues/381
--            Invalid code generation with 'abs' and unsigned integers>
--
--         * <https://github.com/LeventErkok/sbv/issues/379
--            Code generation misses some opportunities to eliminate constants>
--
--         * <https://github.com/LeventErkok/sbv/issues/375
--            Code generation and integer literals>
--
--         * <https://github.com/LeventErkok/sbv/issues/389
--            Generated @Makefile@ does not contain @-lm@ when needed>
--
-- Please continue adding to the list!
--
-- == Note about SBV interpretation
--
-- A pure SBV expression graph can often be evaluated either via Haskell code or by handing it off
-- to an SMT solver.  Unfortunately external function calls, like those to glibc trigonometric
-- functions, can't be evaluted in the SMT solver.  SBV versions after 8.0 represent all
-- floating-point values symbolically to avoid discrepancies between Haskell and C semantics; this
-- means on these versions, only an SMT solver can evaluate expressions involving floating-point.
-- But trigonometric functions are out of bounds, so we simply don't have a way to interpret SBV
-- graphs other than to code-generate them.
--
-- This program used to compare head-to-head with the SBV interpretation as well, but since SBV 8.1,
-- this isn't feasible.
--
-- == Note about robustness
--
-- This code can be configured to run the generated C code in a separate subprocess.  This mode
-- imposes significant overhead on testing, but it means that errors such as the generated C code
-- being terminated with @SIGFPE@ can be caught and shrunk.
module Categorifier.C.KGenGenerate.Test.Plugin
  ( pluginTest,
    PluginTestConfig (..),
    QuietConfig (..),
    SubprocessConfig (..),
  )
where

import qualified Barbies
import Categorifier.C.CExpr.Function (FunctionGenMode (Standard))
import qualified Categorifier.C.CExpr.IO as CExpr
import Categorifier.C.CExpr.Types.Core (CExpr)
import Categorifier.C.Codegen.FFI.Call (callSBVCFunction)
import Categorifier.C.Codegen.FFI.Spec
  ( CallError (..),
    FFIArrayCount,
    SBVSpec,
    Spec (..),
    SpecMismatch,
    checkSBVCFunctionSizeInputs,
    prettyCallError,
    specToFFISpec,
  )
import Categorifier.C.KGen.KGen (KGen)
import Categorifier.C.KGenGenerate.FFI.Gen (codeGenWithBindings, mkCgFromArraysFun)
import Categorifier.C.KGenGenerate.FFI.Plugin
  ( BuildError,
    BuildOptions (buildOptOutputDirectory),
    buildObj,
    defaultBuildOptions,
    getTempDirectory,
    prettyBuildError,
    withSBVCFunctionPlugin,
  )
import Categorifier.C.KGenGenerate.FFI.Spec (genFunSpec, specFileName)
import Categorifier.C.KGenGenerate.Test.GenerateTyped
  ( ArrayCount (..),
    Arrays (..),
    arraysEq,
    fromCInputs,
    genCounts,
    genInputValues,
    genMIMOFunction,
  )
import Categorifier.C.KGenGenerate.Test.IProduct (splitIProduct)
import Categorifier.C.KGenGenerate.Test.Pretty (Pretty (..), specToPrettyArray)
import Categorifier.C.KGenGenerate.Test.Subprocess (Channel, SubprocessError (..), Uniplex (..))
import qualified Categorifier.C.KGenGenerate.Test.Subprocess as Subprocess
import Categorifier.C.KTypes.C (C (..), (!!?))
import Categorifier.C.KTypes.CExpr.Generate (generateCExprFunction)
import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData, force)
import Control.Lens (over, _Left)
import Control.Monad (join, replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (void)
import Data.Functor.Compose (Compose (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (zipWith4)
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Vector as V (Vector)
import GHC.Generics (Generic)
import qualified Hedgehog as H
import qualified System.Exit as Exit
import qualified System.Posix.Process as Process
import qualified System.Posix.Signals as Signals (floatingPointException, killProcess)

data QuietConfig = Quiet | Noisy deriving (Eq, Ord, Show)

data SubprocessConfig = OneProcess | Subprocess deriving (Eq, Ord, Show)

data PluginTestConfig = PluginTestConfig
  { pluginTestQuiet :: QuietConfig,
    pluginTestSubprocess :: SubprocessConfig,
    pluginTestTopDir :: FilePath
  }
  deriving (Eq, Ord, Show)

getNextFunctionNumber :: (Num a, MonadIO m) => IORef a -> m a
getNextFunctionNumber counter =
  -- This is done atomically so we can parallelize the plugin testing
  liftIO $ atomicModifyIORef' counter (\x -> (x, x + 1))

getPrintFunction ::
  Arrays ArrayCount ->
  (Arrays (Compose V.Vector Pretty) -> Arrays (Compose V.Vector Pretty), a) ->
  String
getPrintFunction inputCounts (pretty, _) =
  show . pretty $ specToPrettyArray inputCounts

-- | This data type is used to send messages back and forth between the child and parent processes
-- if running with a subprocess for C code.
data ChildRunnerMessage a
  = ChildRunnerInputIndex Int
  | ChildRunnerResult a
  deriving (Generic)

instance Serialise a => Serialise (ChildRunnerMessage a)

parentAction ::
  Serialise a =>
  IORef Int ->
  Channel 'Rx (ChildRunnerMessage a) ->
  IO (Either SubprocessError a)
parentAction ref rxChannel = go 0
  where
    go lastInput = do
      writeIORef ref lastInput
      Subprocess.receiveFromPipe rxChannel >>= \case
        -- Got an error; this could mean the child died.  Report the last successfully received
        -- input along with the error.
        Left subprocessErr -> pure $ Left subprocessErr
        -- Got an update on the last input run; increment the counter and wait for another message.
        Right (ChildRunnerInputIndex k) -> go k
        -- Got a final result from running the calculation; return it
        Right (ChildRunnerResult v) -> pure $ pure v

handleSubprocessResult ::
  forall m a.
  MonadIO m =>
  Arrays V.Vector ->
  Either SubprocessError a ->
  H.PropertyT m a
handleSubprocessResult _ (Right a) = pure a
handleSubprocessResult lastInput (Left (SubprocessExit (Process.Exited (Exit.ExitFailure code)))) =
  H.annotateShow lastInput *> H.annotateShow code *> H.failure
-- This should be impossible.
handleSubprocessResult _lastInput (Left (SubprocessExit (Process.Exited Exit.ExitSuccess))) =
  error "We should never get 'Left' with a successful exit here!"
handleSubprocessResult lastInput (Left (SubprocessExit (Process.Terminated signal _dumped))) =
  H.annotateShow lastInput *> H.annotateShow signal *> handleSignal
  where
    handleSignal
      -- We got a floating-point exception.  This counts as a code-generation failure, since we
      -- shouldn't be able to generate code that fails in this way; thus we should continue
      -- shrinking.
      | signal == Signals.floatingPointException = H.failure
      -- This could happen for any reason, but the most likely is that it happened due to the OOM
      -- killer, which doesn't tell us anything about the expression we're testing.  Just discard
      -- this test and try again -- either the child will sneak past the OOM killer next time, or
      -- eventually we will fail to shrink any farther.
      --
      -- TODO(MP): I'm not sure this is how discard is necessarily meant to be used.
      | signal == Signals.killProcess = H.discard
      -- Received some other error -- let's conservatively assume it's a code-generation problem and
      -- try to shrink it.
      | otherwise = H.failure
handleSubprocessResult _ (Left err) =
  error $ "Some terrible problem happened: " <> show err

evaluateInSeparateProcess ::
  Serialise a =>
  (Channel 'Tx (ChildRunnerMessage a) -> IO ()) ->
  IO (Either SubprocessError a, Int)
evaluateInSeparateProcess childAction = do
  ref <- newIORef 0
  subprocessResult <- Subprocess.runSubprocessWithChannel (parentAction ref) childAction
  refResult <- readIORef ref
  pure (subprocessResult, refResult)

generateAndCallC ::
  MonadIO m =>
  PluginTestConfig ->
  [FilePath] ->
  String ->
  SBVSpec ->
  [Arrays V.Vector] ->
  String ->
  H.PropertyT m a ->
  H.PropertyT
    m
    ( Either
        (SpecMismatch FFIArrayCount)
        (Either CallError [Arrays V.Vector])
    )
generateAndCallC config@PluginTestConfig {..} sources funName funSpec inputs cgSystemName mkCgF = do
  defaultOpts <- liftIO $ defaultBuildOptions Nothing
  let ourBuildOptions = defaultOpts {buildOptOutputDirectory = Just pluginTestTopDir}
  -- Emit the function code
  spam "Generating function code."
  _ <- mkCgF
  -- Compile the code into a shared object
  spam $ "Function generated; building " <> cgSystemName <> " shared object."
  (objName, command) <-
    checkBuildResult config funName
      =<< H.evalIO (buildObj ourBuildOptions sources soName)
  H.annotateShow $ String.unwords command
  spam "Random inputs generated; calling functions."
  -- Load in the SBV function and call it
  case pluginTestSubprocess of
    OneProcess ->
      liftIO $
        withSBVCFunctionPlugin
          (pluginTestQuiet /= Quiet)
          funSpec
          objName
          (funName <> cgSystemName)
          runTestsInProcess
    Subprocess -> do
      (res, lastInputIndex) <- liftIO . evaluateInSeparateProcess $ \channel -> do
        res <-
          withSBVCFunctionPlugin
            (pluginTestQuiet /= Quiet)
            funSpec
            objName
            (funName <> cgSystemName)
            (runTestsWithLogging channel)
        Subprocess.sendToPipe (ChildRunnerResult res) channel
      maybe H.failure (`handleSubprocessResult` res) (inputs !!? lastInputIndex)
  where
    spam = if pluginTestQuiet == Quiet then const $ pure () else liftIO . putStrLn
    soName = pluginTestTopDir <> "/" <> funName <> "_" <> cgSystemName <> ".so"
    checkSizes =
      over _Left SpecMismatchInputs
        . flip checkSBVCFunctionSizeInputs (specInput funSpec)
    runTests cfun runSBVFunction =
      (checkSizes cfun *>) . Right
        <$> traverse runSBVFunction (zip [0 ..] inputs)
    runTestsInProcess cfun =
      runTests cfun $ callSBVCFunction cfun . snd
    runTestsWithLogging pipe cfun =
      runTests cfun runSBVFunction
      where
        runSBVFunction (idx, value) = do
          Subprocess.sendToPipe (ChildRunnerInputIndex idx) pipe
          callSBVCFunction cfun value

data Mismatch = Mismatch
  { mismatchSBVC :: Arrays V.Vector, -- SBV C value
    mismatchCExprC :: Arrays V.Vector, -- CExpr C value
    mismatchC :: Arrays V.Vector, -- C value
    mismatchInputs :: Arrays V.Vector -- Input values
  }
  deriving (Show)

makeSbv ::
  PluginTestConfig ->
  String ->
  Arrays ArrayCount ->
  (Arrays (Compose V.Vector KGen) -> IO (Arrays (Compose V.Vector KGen))) ->
  IO ()
makeSbv PluginTestConfig {..} funName inputSizes f =
  void $ codeGenWithBindings (pluginTestQuiet == Quiet) (Just path) funName doCg
  where
    path = pluginTestTopDir <> "/" <> funName
    doCg = mkCgFromArraysFun f inputSizes

makeCExpr ::
  PluginTestConfig ->
  String ->
  SBVSpec ->
  (Arrays (Compose V.Vector CExpr) -> Arrays (Compose V.Vector CExpr)) ->
  H.PropertyT IO [FilePath]
makeCExpr PluginTestConfig {..} funName' (Spec inputSizes outputSizes) f = do
  bodyResult <-
    H.evalIO $
      generateCExprFunction Standard (Text.pack funName) inputSizes (pure . f)
  functionBodies <- case bodyResult of
    Right bod -> pure bod
    Left err -> H.annotate (Text.unpack $ CExpr.prettyFunctionGenError err) *> H.failure
  (hdrFile, srcFile) <- H.evalIO $ do
    writeFile specSrcPath specSrc
    writeFile specHeaderPath specHeader
    CExpr.emitCFunction pluginTestTopDir (Text.pack funName) functionBodies
  pure [specSrcPath, specHeaderPath, srcFile, hdrFile]
  where
    funName = funName' <> "CExpr"
    specRoot = pluginTestTopDir <> "/" <> specFileName funName
    specSrcPath = specRoot <> ".c"
    specHeaderPath = specRoot <> ".h"
    ffiSpec = specToFFISpec $ Spec inputSizes outputSizes
    (specSrc, specHeader) = genFunSpec funName ffiSpec

-- | This function generates C code corresponding to a 'KGen' function, compiles and dynamically
-- loads it and compares it head-to-head against the original 'KGen' expression to ensure the
-- results match exactly.
pluginTest :: PluginTestConfig -> IORef Int -> Int -> Int -> IO () -> H.Property
pluginTest oldConfig@PluginTestConfig {pluginTestTopDir = pth} counter numInputs exprSize after =
  H.property $ do
    defaultOpts <- liftIO $ defaultBuildOptions Nothing
    let ourBuildOptions = defaultOpts {buildOptOutputDirectory = Just pth}
    -- Generate a random function expression
    inputCounts <- H.forAll $ genCounts exprSize
    outputCounts <- H.forAll $ genCounts exprSize
    functions@(prettyF, (sbvF, (egenF, nativeF))) <-
      H.forAllWith (getPrintFunction inputCounts)
        . fmap (fmap (fmap splitIProduct . splitIProduct) . splitIProduct)
        $! genMIMOFunction inputCounts outputCounts
    spam "Generating random inputs."
    -- Here we mask the input values using a custom display function so Hedgehog doesn't try to show
    -- all of them (including the majority of perfectly fine ones) on failure. Each test failure
    -- situation below comes with its own set of annotations which deliberately show only the
    -- problematic values, which allows scaling to >1e6 input values per test.
    inputList <-
      H.forAllWith (const $ "<" <> show numInputs <> " inputs>")
        . replicateM numInputs
        $ genInputValues inputCounts ::
        H.PropertyT IO [Arrays (Compose V.Vector C)]
    -- Grab a unique number we can use to name this function
    nextNumber <- getNextFunctionNumber counter
    -- Create a new temp dir for this test
    topDir <- liftIO $ getTempDirectory ourBuildOptions
    H.annotateShow topDir
    H.annotateShow nextNumber
    let _ = egenF :: Arrays (Compose V.Vector CExpr) -> Arrays (Compose V.Vector CExpr)
        -- Naming things
        funSpec =
          Spec
            { specInput = inputCounts,
              specOutput = outputCounts
            }
        functionName = "test_function" <> show nextNumber
        cInputList = fmap fromCInputs inputList
        sbvSources =
          fmap
            ((topDir <> "/" <> functionName <> "/") <>)
            [ functionName <> "SBV.c", -- SBV source
              functionName <> "SBV_spec.c" -- SBV calling convention spec
            ]
        egenSources =
          fmap
            ((topDir <> "/") <>)
            [ functionName <> "CExpr.c", -- SBV source
              functionName <> "CExpr_spec.c" -- SBV calling convention spec
            ]
        newConfig = oldConfig {pluginTestTopDir = topDir}
    -- TODO(MP): these can be parallelized, but beware the consequences of having a 'H.PropertyT'
    -- bind call interleaved with async stuff!
    egenCResultList' <-
      generateAndCallC
        newConfig
        egenSources
        functionName
        funSpec
        cInputList
        "CExpr"
        $ makeCExpr newConfig functionName funSpec egenF
    sbvCResultList' <-
      generateAndCallC
        newConfig
        sbvSources
        functionName
        funSpec
        cInputList
        "SBV"
        $ liftIO (makeSbv newConfig functionName inputCounts (pure . sbvF))
    let nativeOutputs = fmap nativeF inputList :: [Arrays (Compose V.Vector C)]
    nativeResult <-
      H.eval
        $! fmap
          (Barbies.bmapC @NFData (force . fmap (force . unsafeC) . getCompose))
          nativeOutputs
    let sbvCResultList :: [Arrays V.Vector]
        sbvCResultList = checkResultList sbvCResultList'
        egenCResultList = checkResultList egenCResultList'
        matchF (Mismatch k e n _) = matchTuple (k, e, n)
        matchTuple (k, e, n) = arraysEq k e && arraysEq k n
        -- Rather than just directly compare with Hedgehog, we filter down the outputs to the sets
        -- that don't match between the generated code and our Haskell code before asserting. This
        -- means that our error reports will only show the errors, rather than all the inputs
        -- generated.
        mismatchDescriptions =
          filter (not . matchF) $
            zipWith4
              Mismatch
              sbvCResultList
              egenCResultList
              nativeResult
              (fmap fromCInputs inputList)
    H.annotateShow topDir
    H.annotateShow . prettyF $ specToPrettyArray inputCounts
    H.annotateShow inputCounts
    H.annotateShow outputCounts
    case mismatchDescriptions of
      [] -> pure ()
      -- Our goal at this point is to display enough to be able to reproduce the problem manually
      -- (e.g. in ghci or with a small C program) if we find a bug, so we don't have any need to
      -- hang on to more than one failing input point.
      --
      -- Due to the way Hedgehog handles shrinks and rendering (i.e. with excessive strictness),
      -- keeping only the first failure here instead of displaying every failing input point we find
      -- yields enormous improvements in memory performance.
      (m@(Mismatch k e n _) : _) -> do
        H.annotateShow m
        -- Check equal outputs
        H.annotate $ getPrintFunction inputCounts functions
        H.assert $ matchTuple (k, e, n)
    liftIO after
  where
    checkResultList list = case join . over _Left SpecMismatchFFI $ list of
      Left mismatch -> error $ prettyCallError mismatch
      Right result -> result
    spam = if pluginTestQuiet oldConfig == Quiet then const $ pure () else liftIO . putStrLn

checkBuildResult ::
  (MonadIO m, H.MonadTest m) =>
  PluginTestConfig ->
  String ->
  Either BuildError a ->
  m a
checkBuildResult PluginTestConfig {..} functionName result = case result of
  Left err -> do
    spam $
      "========= Build of function\n"
        <> "========= '"
        <> functionName
        <> "'\n"
        <> "========= from directory\n"
        <> "'"
        <> pluginTestTopDir
        <> "'\n"
        <> "========= failed; discarding!"
    -- Not sure what else to do here, hopefully this won't happen often
    H.annotate $ prettyBuildError err
    H.failure
  Right objName -> pure objName
  where
    spam = if pluginTestQuiet == Quiet then const $ pure () else liftIO . putStrLn
