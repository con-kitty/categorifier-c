{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Common Hedgehog setup for tests that do random expression generation and random testing of
-- generated code.
--
-- The idea is that all the common Hedgehog options, including configuration of shrinks and
-- re-checking, should be handled inside this module.  Any program using the types provided by this
-- module and 'checkRNG' to run the property should get deterministic RNG handling, helpful prompts
-- around re-checking and built-in re-check logic with different command-line options.
--
-- The general workflow is this:
--
--     1. You specify the number of tests and how to seed the RNG and run your tests.
--
--     2. Your test fails, and you read the printed failure message from this module which directly
--        tells you the command-line options you must pass to the program to replicate the failure.
--
--     3. You fix the problem and confirm it's fixed by following the prompt given to you at the end
--        of step 2 and failing to replicate the error.
module Categorifier.C.Hedgehog.Options
  ( hedgehogTerminalSetup,
    HedgehogOptions (..),
    parseHedgehogOptions,
    parseHedgehogOptionsWithDefaultNumTestExprs,
    checkRNG,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String (fromString)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Runner as H (RunnerConfig (..), checkGroup)
import Options.Applicative
  ( Parser,
    auto,
    flag,
    flag',
    help,
    long,
    metavar,
    option,
    short,
    showDefault,
    value,
  )
import qualified Options.Applicative.Builder.Internal as OptionsInternal
  ( Mod,
    OptionFields,
  )
import PyF (fmt)

hedgehogTerminalSetup :: IO ()
hedgehogTerminalSetup = setLocaleEncoding utf8

spamConfig :: MonadIO m => HedgehogOptions -> m ()
spamConfig HedgehogOptions {..} =
  liftIO $
    putStrLn
      [fmt|
Running {show optionsNumTestExprs} individual Hedgehog tests.
Maximum of {show optionsMaxShrinks} successful shrink steps allowed.
|]

checkRNG :: forall m. MonadIO m => String -> HedgehogOptions -> H.Property -> m Bool
checkRNG propertyName opts@HedgehogOptions {..} property = do
  let runnerConfig =
        H.RunnerConfig
          { H.runnerWorkers = Nothing,
            H.runnerColor = Nothing,
#if MIN_VERSION_hedgehog(1, 1, 1)
            H.runnerSeed = Nothing,
#endif
            H.runnerVerbosity = Nothing
          }
  run runnerConfig property
  where
    run :: H.RunnerConfig -> H.Property -> m Bool
    run runnerConfig prop =
      case (optionsRecheck, optionsRNG) of
        (Reproduce, RNGIO) ->
          seedMissingError
        (Reproduce, RNGFixed seed) -> do
          liftIO . putStrLn $ recheckMessage seed
          H.recheck (fromIntegral optionsNumTestExprs) seed (configShrinks prop)
          pure False
        (Test, _) -> do
          spamConfig opts
          res <-
            H.checkGroup runnerConfig . H.Group (fromString propertyName) $
              pure (fromString propertyName, configTest prop)
          unless res . liftIO $ putStrLn onFailureMessage
          pure res
    configTest =
      H.withTests (fromIntegral optionsNumTestExprs) . configShrinks
    configShrinks =
      H.withShrinks (fromIntegral optionsMaxShrinks)
    onFailureMessage =
      [fmt|
If this test failed with a Hedgehog error message like

    recheck (Size <some-size>) (Seed <seed-a> <seed-b>) <property>

you should be able to reproduce the error by passing

    -r -t <some-size> -a <seed-a> -b <seed-b>

on the command line to this program, removing  -o / --seed-from-io
if it was passed and leaving all other options identical.
|]
    recheckMessage seed =
      [fmt|
Re-checking hedgehog test number {show optionsNumTestExprs}, allowing
{show optionsMaxShrinks} successful shrinking step(s).

If you matched the command-line arguments properly, then this run will
correspond to Hedgehog's message:

    recheck (Size {show optionsNumTestExprs}) ({show seed}) <property>
|]
    seedMissingError =
      error
        [fmt|
I can't reproduce a test failure if you don't tell me the seed!
Please use explicit '-a' and '-b' options on the command line!
|]

data RNGOptions
  = RNGFixed H.Seed
  | RNGIO
  deriving (Eq, Ord, Show)

parseRNGFixed :: Parser RNGOptions
parseRNGFixed =
  fmap RNGFixed
    . H.Seed
    <$> option
      auto
      ( long "seed-a"
          <> short 'a'
          <> metavar "SEED-VALUE"
          <> value 222
          <> showDefault
          <> help "First Hedgehog seed value"
      )
    <*> option
      auto
      ( long "seed-b"
          <> short 'b'
          <> metavar "SEED-VALUE"
          <> help "Second Hedgehog seed value"
          <> value 2222
          <> showDefault
      )

parseRNGIO :: Parser RNGOptions
parseRNGIO =
  flag'
    RNGIO
    ( long "seed-from-io"
        <> short 'o'
        <> help "Choose a seed value internally."
    )

data RunMode
  = Test
  | Reproduce
  deriving (Eq, Ord, Show)

data HedgehogOptions = HedgehogOptions
  { optionsNumTestExprs :: Int,
    optionsMaxShrinks :: Int,
    optionsRNG :: RNGOptions,
    optionsRecheck :: RunMode
  }
  deriving (Eq, Ord, Show)

parseTestExprOptions :: OptionsInternal.Mod OptionsInternal.OptionFields Int
parseTestExprOptions =
  long "num-test-exprs"
    <> short 't'
    <> metavar "NUM"
    <> help "Number of expressions to test"

parseNumTestExprs :: Parser Int
parseNumTestExprs =
  option auto parseTestExprOptions

parseNumTestExprsWithDefault :: Int -> Parser Int
parseNumTestExprsWithDefault defaultValue =
  option auto $
    parseTestExprOptions
      <> value defaultValue
      <> showDefault

-- | Conditions on this parser:
--
--   * You __must__ specify a number of test expressions
--
-- If you want to reproduce a failure, it's obviously going to be mandatory to pass the same value.
parseHedgehogOptions :: Parser HedgehogOptions
parseHedgehogOptions = parseMostHedgehogOptions parseNumTestExprs

-- | Parse hedgehog testing options, given a number of test expressions to run if no value is given
-- on the command line.
--
-- If you want to reproduce a failure, it's obviously going to be mandatory to pass the same value.
parseHedgehogOptionsWithDefaultNumTestExprs :: Int -> Parser HedgehogOptions
parseHedgehogOptionsWithDefaultNumTestExprs =
  parseMostHedgehogOptions . parseNumTestExprsWithDefault

parseMostHedgehogOptions :: Parser Int -> Parser HedgehogOptions
parseMostHedgehogOptions parseNum =
  HedgehogOptions
    <$> parseNum
    <*> option
      auto
      ( long "max-shrinks"
          <> short 's'
          <> metavar "NUM"
          <> value 100
          <> showDefault
          <> help "Maximum number of shrink steps hedgehog may take in case of failure"
      )
    -- If the user passes `--seed-from-io` on the command-line, generate a new seed ourselves.
    -- Otherwise, fall back to the `--seed-a` and `--seed-b` values, which have defaults.
    <*> (parseRNGIO <|> parseRNGFixed)
    <*> flag
      Test
      Reproduce
      ( long "reproducer-mode"
          <> short 'r'
          <> help ("Enable reproduction of a test failure " <> "(default is to search for bugs)")
      )
