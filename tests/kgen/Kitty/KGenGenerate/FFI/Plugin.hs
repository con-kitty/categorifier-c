{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | FFI calls to dynamically-loaded 'Kitty.KGen.KGen' functions using @dlopen@ and friends.
module Kitty.KGenGenerate.FFI.Plugin
  ( -- * Temporary files
    getTempDirectory,

    -- * Compilation
    BuildFlags (..),
    BuildOptions (..),
    defaultBuildOptions,
    BuildError (..),
    prettyBuildError,
    buildObj,

    -- * Loading
    SBVPlugin,
    loadPlugin,
    unloadPlugin,

    -- * Using
    checkPlugin,
    callPlugin,
    withSBVCFunctionPlugin,
  )
where

import Control.Monad (when)
import Foreign.Ptr (FunPtr)
import Kitty.Codegen.FFI.Foreign (ptrToFunSBV, ptrToFunSBVSpec, ptrToFunSBVSpecSize)
import Kitty.Codegen.FFI.Spec
  ( FFIArrayCount,
    SBVCFunction (..),
    SBVFFISpec,
    SBVSpec,
    SpecMismatch,
    checkSBVCFunctionSpec,
  )
import qualified Kitty.Common.IO.Exception as Exception (Exception, bracket)
import Kitty.KGenGenerate.FFI.Spec
  ( inputSpecFunName,
    inputSpecSizeFunName,
    outputSpecFunName,
    outputSpecSizeFunName,
  )
import qualified System.Directory as Directory
import qualified System.Exit as Exit (ExitCode (..))
import qualified System.Posix.DynamicLinker as DL
import qualified System.Posix.Temp as Temp (mkdtemp)
import qualified System.Process as Process (readProcessWithExitCode)

-- | What flags to pass to the C compiler when compiling.
--
-- Note that if you append flags using 'BuildExtraFlags', they should overwrite the defaults if they
-- conflict; thus you could pass 'BuildExtraFlags $ pure "-Wno-shadow"' to disable shadowing
-- warnings.
data BuildFlags
  = -- | Use the default flags.
    BuildDefaultFlags
  | -- | These will be appended to the default flags.
    BuildExtraFlags [String]
  | -- | The only thing that will be appended to these flags is @<sourceFileName> -o
    -- <objectFileName>@, because these names are calculated internally.
    BuildReplaceFlags [String]
  deriving (Eq, Ord, Show, Read)

-- | Options to control the build of generated-code plugins.
data BuildOptions = BuildOptions
  { buildOptOutputDirectory :: Maybe FilePath,
    buildOptCompiler :: String,
    buildOptFlags :: BuildFlags,
    buildOptDisplayWarnings :: Bool
  }
  deriving (Eq, Ord, Show, Read)

-- | We do plugin-testing with custom compiler options.
--
-- == /GCC bugs/
--
-- <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=85811 this bug> exists in several versions of GCC
-- and clang. As a workaround, we do plugin-testing with clang version 6 (@clang@ on the
-- command-line).
--
-- == /Compile-time evaluation of libm functions/
--
-- We need to convince the compiler not to use MPFR at compile-time to evaluate certain
-- trigonometric functions when it discovers enough constants to partially evaluate. This can cause
-- mismatches, as MPFR will evaluate to the best possible answer given the precision available,
-- whereas calling the math library at runtime may result in slight inaccuracies (usually 1-2 ULP).
--
-- https://www.gnu.org/software/libc/manual/html_node/Errors-in-Math-Functions.html contains a table
-- of known bounds on libm function errors. Any functions in the @x86_64@ column with nonzero error
-- bounds need to be excluded from compile-time evaluation with @-fno-builtin-<function>@ to avoid
-- discovering these discrepancies.
--
-- == /Integer overflow/
--
-- For better or for worse, Haskell and SBV's code-generation ignore integer overflow, simply
-- wrapping around. This is generally considered to be a bad idea, but avoiding the associated
-- problems is too big a task right now. We instruct the C compiler to do wrapping, too, with
-- @-fwrapv@.
--
-- Thus, the default behavior is to:
--
--    * generate a temporary directory under @/tmp@.
--    * call the compiler as @clang@.
--    * use the default flags (see below).
--    * not display any warnings generated.
--
-- The default compiler invocation is:
--
-- @
--     clang -O2 -std=c11 -shared <sourceFileName> -o <objectFileName> -lm \
--     -fwrapv -fstack-protector-all -fno-strict-overflow -fPIC -ftree-vectorize \
--     -Wall -Wextra -Wimplicit -Wshadow -Wswitch-default -Wswitch-enum -Wundef \
--     -Wuninitialized -Wpointer-arith -Wstrict-prototypes -Wmissing-prototypes \
--     -Wcast-align -Wformat=2 -Wimplicit-function-declaration -Wredundant-decls \
--     -Wformat-security -D_FORTIFY_SOURCE=2 -fno-builtin-acosf -fno-builtin-acoshf \
--     -fno-builtin-acosh -fno-builtin-asinf -fno-builtin-asinhf -fno-builtin-asinh \
--     -fno-builtin-atanf -fno-builtin-atan2f -fno-builtin-atanhf \
--     -fno-builtin-atanh -fno-builtin-coshf -fno-builtin-cosh -fno-builtin-logf \
--     -fno-builtin-powf -fno-builtin-sinhf -fno-builtin-sinh -fno-builtin-tanf \
--     -fno-builtin-tanhf -fno-builtin-tanh
-- @
--
-- (We still pass warning flags to the compiler by default because we will still display the full
-- output for debug purposes if there is a problem during compilation and linking.)
defaultBuildOptions :: Maybe FilePath -> IO BuildOptions
defaultBuildOptions mbClangPath = do
  clang <- clangPath mbClangPath
  pure
    BuildOptions
      { buildOptOutputDirectory = Nothing,
        buildOptCompiler = clang,
        buildOptFlags = BuildDefaultFlags,
        buildOptDisplayWarnings = False
      }

clangPath :: Maybe FilePath -> IO FilePath
clangPath = \case
  Just p -> pure p
  Nothing ->
    Directory.findExecutable "clang" >>= \case
      Just p -> pure p
      Nothing -> fail "clang not found"

{- Temporary files -}
getTempDirectory :: BuildOptions -> IO FilePath
getTempDirectory BuildOptions {buildOptOutputDirectory = userPath} = do
  tmpDir <- maybe Directory.getTemporaryDirectory pure userPath
  path <- Directory.makeAbsolute $ tmpDir <> "/" <> "kgen-plugins"
  Temp.mkdtemp path

{- Compiling to objects -}
compilerArgs :: BuildFlags -> [String] -> String -> [String]
compilerArgs BuildDefaultFlags src obj = defaultCompilerArgs src obj
compilerArgs (BuildExtraFlags extras) src obj =
  defaultCompilerArgs src obj <> (words . unwords) extras
compilerArgs (BuildReplaceFlags line) src obj =
  (words . unwords) line <> src <> ["-o", obj]

defaultCompilerArgs :: [String] -> String -> [String]
defaultCompilerArgs srcNames objName =
  concat [base, defines, features, warnings, noBuiltins]
  where
    base =
      [ "-O2",
        "-g3",
        "-std=c11",
        "-shared"
      ]
        <> srcNames
        <> [ "-o",
             objName,
             "-lm"
           ]
    defines =
      fmap
        ("-D" <>)
        [ "_FORTIFY_SOURCE=2"
        ]
    features =
      fmap
        ("-f" <>)
        [ "wrapv",
          "stack-protector-all",
          "no-strict-overflow",
          "PIC",
          "tree-vectorize"
        ]
    warnings =
      fmap
        ("-W" <>)
        [ "all",
          "extra",
          "implicit",
          "shadow",
          "switch-default",
          "switch-enum",
          "undef",
          "uninitialized",
          "pointer-arith",
          "strict-prototypes",
          "missing-prototypes",
          "cast-align",
          "format=2",
          "implicit-function-declaration",
          "redundant-decls",
          "format-security"
        ]
    noBuiltins =
      fmap
        ("-fno-builtin-" <>)
        -- Here is a table of the relevant GNU libm functions that deliver full-precision results on
        -- an architecture we use, from
        -- https://www.gnu.org/software/libc/manual/html_node/Errors-in-Math-Functions.html
        --
        --        | x86_64 | aarch64
        --  ------------------------
        --  exp   | yes    | yes
        --  log   | yes    | yes
        --  cosf  | yes    | no
        --  sinf  | yes    | no
        --  fmod  | yes    | yes
        --  fmodf | yes    | yes
        --  sqrt  | yes    | yes
        --  tan   | yes    | yes
        --  atan2 | yes    | yes
        --  atan  | yes    | no
        --  acos  | yes    | yes
        --  asin  | yes    | yes
        --
        -- For maximum safety, we allow compile-time evaluation by MPFR only on relevant functions
        -- that are safe on all architectures (i.e. only the functions with a "yes" in both columns
        -- above are excluded from the list).
        [ "acosf",
          "acoshf",
          "acosh",
          "asinf",
          "asinhf",
          "asinh",
          "atanf",
          "atan",
          "atan2f",
          "atanhf",
          "atanh",
          "coshf",
          "cosh",
          "cosf",
          "cos",
          "logf",
          "powf",
          "pow",
          -- See https://github.com/LeventErkok/sbv/issues/456 and
          -- https://github.com/LeventErkok/sbv/issues/462 if you have time to kill and really want
          -- to think about floating-point.
          "rint",
          "rintf",
          "sinhf",
          "sinh",
          "sinf",
          "sin",
          "tanf",
          "tanhf",
          "tanh",
          -- You might think that since integer operations are not subject to a few ULPs of error,
          -- these functions could safely be evaluated at compile time without causing
          -- discrepancies.  However, if the absolute value of a number can't be represented
          -- (e.g. @abs(INT32_MIN)@), the behavior is undefined, which the clang project has kindly
          -- pointed out means that the entire program can do anything at that point, at which point
          -- nothing bizarre their optimizer does can be faulted.  It turns out that it does do
          -- bizarre things, so we must disable compile-time evaluation of these functions after
          -- all.
          --
          -- We don't want to rely on undefined behavior at any point, but it's also not clear that
          -- we can rely on the optimizer keeping its paws off.
          "abs",
          "labs",
          "llabs",
          -- C compilers tend to do their own thing with these operations at compile time, since the
          -- IEEE-754 standard says that the sign bit can be ignored when comparing zeros.
          -- Disabling them gives behavior consistent between gcc 10.1 and clang 10.0 as well as SBV
          -- and GHC (for now) -- the second operand is always returned.
          "fmax",
          "fmaxf",
          "fmin",
          "fminf"
        ]

-- | You will get this type of value if something goes wrong with compilation
data BuildError = BuildError
  { -- | Name of the compiler
    beCompilerName :: String,
    -- | Args passed to the compiler
    beCompilerArgs :: [String],
    -- | Exit code the compiler gave back
    beExitCode :: Int,
    -- | @stdout@ from the compiler invocation
    beCompilerStdout :: String,
    -- | @stderr@ from the compiler invocation
    beCompilerStderr :: String
  }
  deriving (Eq, Show, Ord)

instance Exception.Exception BuildError

prettyBuildError :: BuildError -> String
prettyBuildError (BuildError compName args exitCode stdout stderr) =
  unlines
    [ "=== Compiler: '" <> compName <> "'",
      "=== Compiler args:\n" <> unlines args,
      "=== Compiler exit code: '" <> show exitCode <> "'",
      "=== Compiler stderr:\n" <> stderr,
      "=== Compiler stdout:\n" <> stdout
    ]

-- | 'buildObj quiet srcNames objName' compiles an object file by the name given in @objName@ using
-- the source files by the names given in @srcNames@, or a description of why and how compilation
-- failed.
buildObj :: BuildOptions -> [String] -> String -> IO (Either BuildError (String, [String]))
buildObj (BuildOptions _ compilerName flags spam) srcNames objName = do
  (code, stdOut, stdErr) <-
    Process.readProcessWithExitCode compilerName args mempty
  case code of
    Exit.ExitSuccess -> do
      -- Let's inform the user about warnings, as it could indicate a problem with code-generation.
      when (spam && not (null stdErr)) . putStrLn $
        compilerName <> " (non-fatal) warning: '" <> stdErr <> "'"
      pure . Right $ (objName, compilerName : args)
    Exit.ExitFailure retval ->
      pure . Left $ BuildError compilerName args retval stdOut stdErr
  where
    args = compilerArgs flags srcNames objName

{- Dynamic linking -}

-- | This object represents a function dynamically loaded from a shared object file. You can get one
-- by calling 'loadPlugin' and use it by calling 'withSBVCFunctionPlugin'.
data SBVPlugin = SBVPlugin
  { sbvDLObject :: DL.DL,
    sbvSOPath :: String,
    sbvDLFunction :: SBVCFunction
  }

spamStart :: Bool -> String -> String -> IO ()
spamStart spam hdr s =
  if spam then putStr $ "[" <> hdr <> "]  " <> s else pure ()

spamFinish :: Bool -> String -> IO ()
spamFinish spam = if spam then putStrLn else const $ pure ()

grabSymbol :: Bool -> String -> DL.DL -> String -> IO (FunPtr a)
grabSymbol spam hdr dl symName = do
  spamStart spam hdr $
    "Grabbing symbol '" <> symName
      <> "' from shared object . . . "
  ptr <- DL.dlsym dl symName
  spamFinish spam "done!"
  pure ptr

-- | 'unsafeLoadPlugin spam spec soPath symbolName' loads the function @symbolName@ from the object
-- file at @soPath@ and exposes it to you in callable form as an 'SBVPlugin'. Setting @spam@ to
-- 'True' will give you a play-by-play of the loading process.
--
-- You probably want 'loadPlugin', which automatically checks that the spec you supplied matches the
-- object file before giving it back to you.
unsafeLoadPlugin ::
  -- | Spam stdout?
  Bool ->
  -- | Specification for the function you want to load (get this from
  -- 'Kitty.FFI.KGen.codeGenWithBindings')
  SBVSpec ->
  -- | Path to the @.so@ file you want to load
  String ->
  -- | Name of the symbol you want to load
  String ->
  -- | The plugin you want
  IO SBVPlugin
unsafeLoadPlugin spam spec soPath symbolName = do
  spamStart spam thisFunName $
    "Opening shared object file '" <> show soPath <> "' . . . "
  dl <- DL.dlopen soPath $ pure DL.RTLD_NOW
  spamFinish spam "done!"
  let grabSym :: String -> IO (FunPtr a)
      grabSym = grabSymbol spam symbolName dl
  funptr <- grabSym symbolName
  getinsizeptr <- grabSym $ inputSpecSizeFunName symbolName
  getoutsizeptr <- grabSym $ outputSpecSizeFunName symbolName
  inspecptr <- grabSym $ inputSpecFunName symbolName
  outspecptr <- grabSym $ outputSpecFunName symbolName
  pure
    SBVPlugin
      { sbvDLObject = dl,
        sbvSOPath = soPath,
        sbvDLFunction =
          SBVCFunction
            { sbvCInputSpecSize = ptrToFunSBVSpecSize getinsizeptr,
              sbvCOutputSpecSize = ptrToFunSBVSpecSize getoutsizeptr,
              sbvCInputSpec = ptrToFunSBVSpec inspecptr,
              sbvCOutputSpec = ptrToFunSBVSpec outspecptr,
              sbvCFunCall = ptrToFunSBV funptr,
              sbvCFunName = symbolName,
              sbvCFunSpec = spec
            }
      }
  where
    thisFunName = "loadPlugin"

-- | 'loadPlugin spam spec soPath symbolName' loads the function @symbolName@ from the object file
-- at @soPath@ and checks whether it matches the given @spec@ before exposing it to you in callable
-- form as an 'SBVPlugin'. Setting @spam@ to 'True' will give you a play-by-play of the loading
-- process.
loadPlugin ::
  -- | Spam stdout?
  Bool ->
  -- | Specification for the function you want to load (get this from
  -- 'Kitty.FFI.KGen.codeGenWithBindings')
  SBVSpec ->
  -- | Path to the @.so@ file you want to load
  String ->
  -- | Name of the symbol you want to load
  String ->
  -- | A description of the mismatch between the @spec@ you gave and the one embedded in the object
  -- file, or the plugin you want
  IO
    ( Either
        (SpecMismatch FFIArrayCount)
        SBVPlugin
    )
loadPlugin spam spec soPath symbolName = do
  p <- unsafeLoadPlugin spam spec soPath symbolName
  fmap (const p) <$> checkPlugin p

unloadPlugin :: Bool -> SBVPlugin -> IO ()
unloadPlugin spam plugin = do
  spamStart spam thisFunName $
    "Closing shared object file '" <> show (sbvSOPath plugin) <> "' . . . "
  DL.dlclose $ sbvDLObject plugin
  spamFinish spam "done!"
  where
    thisFunName = "unloadPlugin"

checkPlugin ::
  SBVPlugin ->
  IO
    ( Either
        (SpecMismatch FFIArrayCount)
        SBVFFISpec
    )
checkPlugin (SBVPlugin _ _ cfun) = checkSBVCFunctionSpec cfun

callPlugin :: SBVPlugin -> (SBVCFunction -> t) -> t
callPlugin (SBVPlugin _ _ cfun) f = f cfun

-- | Given a function's specification @spec@, a path to a shared object @objPath@ containing the
-- plugin function and the name of the symbol for the plugin function within the shared object
-- @symbolName@,
--
--   'withFunctionSymbolSBV spec objPath symbolName userFunction'
--
-- safely invokes the user-provided @userFunction@ on the specified plugin. The main way to use this
-- is to use 'callPlugin' to create @userFunction@. The specification @spec@ will be checked against
-- the specification embedded in the plugin object, and if it doesn't match, @userFunction@ is never
-- called. When this function returns, the plugin has been safely unloaded, even if @userFunction@
-- has thrown an exception.
withSBVCFunctionPlugin ::
  Bool ->
  SBVSpec ->
  FilePath ->
  String ->
  (SBVCFunction -> IO b) ->
  IO (Either (SpecMismatch FFIArrayCount) b)
withSBVCFunctionPlugin spam spec soPath symbolName =
  Exception.bracket setup cleanup . wrapf
  where
    setup = unsafeLoadPlugin spam spec soPath symbolName
    cleanup = unloadPlugin spam
    -- Check for a mismatch between the I/O spec the plugin says and
    -- what we exspect on the Haskell side before invoking the user
    -- function.
    wrapf userf plugin = do
      chuck <- checkPlugin plugin
      case chuck of
        Left mismatch -> pure $ Left mismatch
        Right _ -> Right <$> callPlugin plugin userf
