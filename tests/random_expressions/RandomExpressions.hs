{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{-
- Regular tests:
  ("test_codegen_small_exprs", "-t 50 -i 1000 -m 1")
  ("test_codegen_little_exprs", "-t 50 -i 1000 -m 2")
  ("test_codegen_medium_exprs", "-t 40 -i 500 -m 6")
  ("test_codegen_large_exprs", "-t 35 -i 500 -m 16")
  ("test_codegen_huge_exprs", "-t 30 -i 500 -m 64")

- Long tests:
  ("test_codegen_long_small_exprs", "-t 1000 -i 10000 -m 1")
  ("test_codegen_long_little_exprs", "-t 500 -i 10000 -m 2")
  ("test_codegen_long_medium_exprs", "-t 100 -i 10000 -m 6")
  ("test_codegen_long_large_exprs", "-t 50 -i 100000 -m 16")
-}
module Main
  ( main,
  )
where

import qualified Categorifier.C.Hedgehog.Options as Options
import qualified Categorifier.C.Hedgehog.Paths as Paths
import qualified Categorifier.C.KGenGenerate.Test.Plugin as Plugin
import Control.Applicative ((<**>))
import Control.Monad.Extra (join, when, whenJust)
import Data.IORef (modifyIORef', newIORef, readIORef)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Hedgehog as H
import Options.Applicative
  ( Parser,
    ParserInfo,
    auto,
    execParser,
    flag,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    value,
  )
import PyF (fmt)
import System.Exit (exitFailure, exitSuccess)
import System.IO (IO)
import System.Time.Extra (offsetTime)

data Options = Options
  { optionsMaxIOPerType :: Int,
    optionsNumInputRuns :: Int,
    optionsSubprocess :: Plugin.SubprocessConfig,
    optionsVerbose :: Plugin.QuietConfig,
    -- | If set, print progress after @x@ test cases.
    optionsShowProgress :: Maybe Int,
    optionsHedgehog :: Options.HedgehogOptions,
    optionsTestTmpDir :: Paths.TestTmpDirOptions
  }
  deriving (Eq, Ord, Show)

parseOptions :: Parser Options
parseOptions = do
  optionsMaxIOPerType <-
    option auto $
      mconcat
        [ long "max-ios-per-type",
          short 'm',
          metavar "NUM",
          help "Maximum number of expression inputs and outputs of each type"
        ]
  optionsNumInputRuns <-
    option auto $
      mconcat
        [ long "num-input-runs",
          short 'i',
          metavar "NUM",
          help "Number of input points to test for each expression"
        ]
  optionsSubprocess <-
    flag Plugin.OneProcess Plugin.Subprocess $
      mconcat
        [ long "use-subprocess",
          short 'p',
          help "Run generated C code in a subprocess to allow shrinking on fatal errors"
        ]
  optionsVerbose <-
    flag Plugin.Quiet Plugin.Noisy $
      mconcat
        [ long "verbose",
          short 'v',
          help "Print a lot of output along the way"
        ]
  optionsShowProgress <-
    option (Just <$> auto) $
      mconcat
        [ long "show-progress",
          value Nothing,
          help "Show progress after x test cases"
        ]
  optionsHedgehog <- Options.parseHedgehogOptions
  optionsTestTmpDir <- Paths.parseTestTmpDirOptions
  pure Options {..}

opts0 :: ParserInfo Options
opts0 =
  info
    (parseOptions <**> helper)
    ( fullDesc
        <> progDesc desc
        <> header "hedgehog-plugin: randomized testing for the test code-generation system"
    )
  where
    desc =
      [fmt|
This program tests the code-generation system by generating random
expressions, code-generating the corresponding C code, running inputs through
both the C and the original Haskell and comparing the results.  The randomized
generation is done in a way that allows the expressions to be shrunk to
minimal reproducible failure cases.

The limiting factor for this program is basically peak memory consumption.  As
hedgehog generates larger and larger expressions, you're more likely to
encounter an out-of-memory failure. All three input parameters correlate with
increased memory consumption, so your best bet is probably to run some tests
with many small expressions, some tests with only a few huge expressions and
some tests with a few small expressions but many inputs.

If you encounter a fatal error like SIGFPE, please re-run the test using the
'--use-subprocess' flag to run generated C code in a subprocess whose failure
can be analyzed.
|]

main :: IO ()
main = do
  setLocaleEncoding utf8
  -- Parse flags.
  opts <- execParser opts0
  path <- Paths.computeTestTmpDir $ optionsTestTmpDir opts
  -- Run the tests.
  result <- testn opts path
  if result
    then exitSuccess
    else exitFailure
  where
    testn
      Options {optionsHedgehog = hedgeopt@Options.HedgehogOptions {..}, ..}
      pth = do
        putStrLn
          [fmt|
Running {show optionsNumTestExprs} plugin-based tests on randomized expressions.
Each expression will be run on {show optionsNumInputRuns} input points.
Each expression may have up to {show optionsMaxIOPerType} input and output variables.
|]
        counter <- newIORef 0
        timeElapsed <- offsetTime
        testcaseCount <- newIORef @Int 0
        let config =
              Plugin.PluginTestConfig
                { pluginTestQuiet = optionsVerbose,
                  pluginTestSubprocess = optionsSubprocess,
                  pluginTestTopDir = pth
                }
            afterTestCase = whenJust optionsShowProgress $ \x -> do
              cnt <- modifyIORef' testcaseCount (+ 1) *> readIORef testcaseCount
              when (cnt `mod` x == 0) $ do
                elapsed <- timeElapsed
                putStrLn
                  [fmt|Finished {show cnt} out of {show optionsNumTestExprs} test cases.
Time elapsed: {show elapsed}s|]
            property =
              H.withTests (fromIntegral optionsNumTestExprs)
                . H.withShrinks (fromIntegral optionsMaxShrinks)
                $ Plugin.pluginTest
                  config
                  counter
                  optionsNumInputRuns
                  optionsMaxIOPerType
                  afterTestCase
        Options.checkRNG "Codegen test" hedgeopt property
