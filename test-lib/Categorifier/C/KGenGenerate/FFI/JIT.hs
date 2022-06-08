{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Just-in-time compilation of haskell functions via 'CExpr'.
module Categorifier.C.KGenGenerate.FFI.JIT
  ( -- * Types
    JITFunction (..),
    JITError (..),
    BuildOptions (..),

    -- * Calling them
    jitCompile,
    jitCompileMutable,
    defaultBuildOptions,
    jitCompile',
    jitCompileMutable',

    -- ** Safe (wrapped) calls
    withJitFunction,
    withJitFunctionMutable,

    -- * Error-handling
    prettyThrownJITError,
  )
where

import qualified Barbies
import Categorifier.C.Barbies (type (&+&))
import Categorifier.C.CExpr.Function (FunctionGenMode (Standard))
import qualified Categorifier.C.CExpr.IO as CExpr (emitCFunction, prettyFunctionGenError)
import Categorifier.C.CExpr.Types.Core (CExpr)
import Categorifier.C.Codegen.FFI.Call (callSBVCFunctionInPlaceV, callSBVCFunctionStorable)
import Categorifier.C.Codegen.FFI.Spec
  ( CallError (..),
    SBVCFunction,
    SBVSpec,
    Spec (..),
    checkSBVCFunctionSizesIO,
    prettyCallError,
    specToFFISpec,
  )
import Categorifier.C.KGenGenerate.FFI.Plugin
  ( BuildError,
    BuildOptions (..),
    buildObj,
    callPlugin,
    defaultBuildOptions,
    getTempDirectory,
    loadPlugin,
    prettyBuildError,
    unloadPlugin,
  )
import Categorifier.C.KGenGenerate.FFI.Spec (genFunSpec)
import Categorifier.C.KTypes.C (C (..))
import Categorifier.C.KTypes.CExpr.Generate (generateCExprFunction)
import Categorifier.C.KTypes.KLiteral (KLiteral (..))
import Categorifier.C.PolyVec (PolyVec, pdevectorize, pvectorize, pvlengths)
import Categorifier.C.Prim (Arrays (..), IsPrimitive)
import qualified Categorifier.Common.IO.Exception as Exception
import Control.Monad ((<=<))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Bifunctor (first)
import Data.Functor.Compose (Compose (..))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Vector as V (Vector)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable.Mutable as MSV (IOVector, Storable)
import PyF (fmt)
import qualified System.IO as IO

-- | What went wrong trying to JIT-compile this function?
data JITError
  = -- | Something went wrong when compiling
    -- the C code and linking the object file.
    JITBuildError BuildError
  | -- | Something when wrong when calling
    -- the generated object.
    JITCallError CallError
  deriving (Eq, Show)

displayJITError :: JITError -> String
displayJITError (JITBuildError err) =
  [fmt|
Error: building JIT function:
{prettyBuildError err}
|]
displayJITError (JITCallError callErr) =
  [fmt|
Error: calling JIT function:
{prettyCallError callErr}
|]

instance Exception.Exception JITError

-- | This is a JIT-compiled function plugin. You can call it by
-- invoking the 'jitFunctionCall' field and unload it for good by
-- invoking 'jitFunctionUnload'.
data JITFunction b = JITFunction
  { -- | Call the compiled code
    jitFunctionCall :: b,
    -- | Unload the plugin for good
    jitFunctionUnload :: IO ()
  }

jitCompileInternal ::
  BuildOptions ->
  (SBVCFunction -> b) ->
  SBVSpec ->
  (Arrays (Compose V.Vector CExpr) -> IO (Arrays (Compose V.Vector CExpr))) ->
  Maybe FilePath ->
  IO (JITFunction b)
jitCompileInternal options call (Spec inSize outSize) f mbName = do
  -- Create a temporary directory
  tmpdir <- getTempDirectory options
  let -- Compute strings
      prefix x = tmpdir <> "/" <> name <> x
      specSrcPath = prefix "_spec.c"
      specHdrPath = prefix "_spec.h"
      soPath = prefix ".so"
  IO.writeFile specSrcPath specC
  IO.writeFile specHdrPath specH
  generated <- generateCExprFunction Standard name' inSize f
  srcPath <- case generated of
    Left fge ->
      Exception.throwIOAsExceptionWithCallStack
        (Text.unpack . CExpr.prettyFunctionGenError)
        fge
    -- Emit C code
    Right text -> snd <$> CExpr.emitCFunction tmpdir name' text
  ret <- runExceptT $ do
    -- Compile a shared object.
    (objName, _) <-
      err JITBuildError $ buildObj options [srcPath, specSrcPath] soPath
    -- Load the .so and check the FFI against the C-side spec
    plugin <-
      err (JITCallError . SpecMismatchFFI) $ loadPlugin verbose spec objName name
    -- Form a closure that invokes the loaded symbol.
    err (JITCallError . SpecMismatchInputs) . pure . callPlugin plugin $ \p ->
      checkSBVCFunctionSizesIO p inSize outSize
    pure (plugin, callPlugin plugin call)
  case ret of
    Left e -> Exception.throwIOAsExceptionWithCallStack displayJITError e
    -- Give it back to the user with a way to unload when desired.
    Right (plugin, retFun) -> pure . JITFunction retFun $ unloadPlugin verbose plugin
  where
    err :: (e -> JITError) -> IO (Either e a) -> ExceptT JITError IO a
    err toJITErr = ExceptT . fmap (first toJITErr)
    name = fromMaybe "unnamed_function" mbName
    name' = Text.pack name
    verbose = buildOptDisplayWarnings options
    spec = Spec {specInput = inSize, specOutput = outSize}
    (specC, specH) = genFunSpec name $ specToFFISpec spec

-- | Given a function between from one 'PolyVec'torizable type to
-- another, generate C code, compile the code and make it available to
-- be called from Haskell with 'Arrays' of mutable vectors as input
-- and output.
--
-- This may throw a 'JITError' as an exception.
jitCompileMutable ::
  forall s t.
  (PolyVec CExpr s, PolyVec CExpr t) =>
  BuildOptions ->
  (s -> t) ->
  IO (JITFunction (Arrays MSV.IOVector -> Arrays MSV.IOVector -> IO ()))
jitCompileMutable options f =
  let fProxy = Proxy @CExpr
      inLengths = pvlengths fProxy (Proxy @s)
      outLengths = pvlengths fProxy (Proxy @t)
      spec = Spec inLengths outLengths
      f' :: Arrays (Compose V.Vector CExpr) -> IO (Arrays (Compose V.Vector CExpr))
      f' = Exception.throwIOLeft . pvectorize . f <=< Exception.throwIOLeft . pdevectorize
   in jitCompileInternal options callSBVCFunctionInPlaceV spec f' Nothing

-- | This catches any 'JITError' exception and re-throws it using
-- 'error' after formatting it in a more human-readable way.
prettyThrownJITError :: IO a -> IO a
prettyThrownJITError = Exception.handle handler
  where
    handler (JITBuildError be) = error $ prettyBuildError be
    handler (JITCallError ce) = error $ prettyCallError ce

-- | This is a version of 'jitCompileMutable' which always uses the
-- default build flags and turns a thrown 'JITError' exception into a
-- pretty-printed error message.
jitCompileMutable' ::
  (PolyVec CExpr (t CExpr), PolyVec CExpr (s CExpr)) =>
  (s CExpr -> t CExpr) ->
  IO
    (JITFunction (Arrays MSV.IOVector -> Arrays MSV.IOVector -> IO ()))
jitCompileMutable' f = do
  opts <- defaultBuildOptions Nothing
  prettyThrownJITError $ jitCompileMutable opts f

-- | This is a version of 'jitCompile' which always uses the default
-- build flags and turns a thrown 'JITError' exception into a
-- pretty-printed error message.
jitCompile' ::
  ( PolyVec C (t C),
    PolyVec C (s C),
    PolyVec CExpr (t CExpr),
    PolyVec CExpr (s CExpr)
  ) =>
  (s CExpr -> t CExpr) ->
  IO (JITFunction (s C -> IO (t C)))
jitCompile' f = do
  opts <- defaultBuildOptions Nothing
  prettyThrownJITError $ jitCompile opts f

-- | Given a function between from one 'PolyVec'torizable type to
-- another, generate C code, compile the code and make it available to
-- be called from Haskell with the original types as input and output.
--
-- This may throw a 'JITError' as an exception.
jitCompile ::
  forall s t.
  ( PolyVec CExpr (s CExpr),
    PolyVec CExpr (t CExpr),
    PolyVec C (s C),
    PolyVec C (t C)
  ) =>
  BuildOptions ->
  (s CExpr -> t CExpr) ->
  IO (JITFunction (s C -> IO (t C)))
jitCompile options f =
  let cexprProxy = Proxy @CExpr
      inLengths = pvlengths cexprProxy (Proxy @(s CExpr))
      outLengths = pvlengths cexprProxy (Proxy @(t CExpr))
      spec = Spec inLengths outLengths
      -- This is the function we use with 'CExpr' to JIT the code.
      f' :: Arrays (Compose V.Vector CExpr) -> IO (Arrays (Compose V.Vector CExpr))
      f' = Exception.throwIOLeft . pvectorize . f <=< Exception.throwIOLeft . pdevectorize
      -- This is the actual invocation.
      callWrap :: SBVCFunction -> s C -> IO (t C)
      callWrap cf =
        Exception.throwIOLeft
          . pdevectorize
          . Barbies.bmapC @(IsPrimitive &+& MSV.Storable) (Compose . G.map (kliteral @C))
          <=< callSBVCFunctionStorable cf
            . Barbies.bmapC @MSV.Storable (G.map unsafeC . getCompose)
          <=< Exception.throwIOLeft . pvectorize
   in jitCompileInternal options callWrap spec f' Nothing

withJitFunction ::
  forall s t a.
  ( PolyVec CExpr (s CExpr),
    PolyVec CExpr (t CExpr),
    PolyVec C (s C),
    PolyVec C (t C)
  ) =>
  BuildOptions ->
  (s CExpr -> t CExpr) ->
  ((s C -> IO (t C)) -> IO a) ->
  IO a
withJitFunction opts f callback =
  Exception.bracket setup cleanup wrapCallback
  where
    setup = jitCompile opts f
    cleanup = jitFunctionUnload
    wrapCallback = callback . jitFunctionCall

withJitFunctionMutable ::
  forall s t a.
  (PolyVec CExpr (s CExpr), PolyVec CExpr (t CExpr)) =>
  BuildOptions ->
  (s CExpr -> t CExpr) ->
  ((Arrays MSV.IOVector -> Arrays MSV.IOVector -> IO ()) -> IO a) ->
  IO a
withJitFunctionMutable opts f callback =
  Exception.bracket setup cleanup wrapCallback
  where
    setup = jitCompileMutable opts f
    cleanup = jitFunctionUnload
    wrapCallback = callback . jitFunctionCall
