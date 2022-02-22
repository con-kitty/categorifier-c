{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Path handling is handled specially in code generation tests.  We need a place for temporary
-- source and object files, but Bazel needs to know about them in the CI case where it's being used
-- to invoke these test programs.  This module lets you pass your own preferred path, but if you
-- don't set it, it will expect to receive a path from Bazel in the @TEST_TEMPDIR@ environment
-- variable.  There is no default path; you must provide the path in one of these two ways.
module Categorifier.C.Hedgehog.Paths
  ( computeTestTmpDir,
    lookupTestTmpDir,
    TestTmpDirOptions (..),
    parseTestTmpDirOptions,
  )
where

import Data.Maybe (fromMaybe)
import Options.Applicative (Parser, help, long, metavar, short, showDefault, strOption, value)
import System.Environment (lookupEnv)

newtype TestTmpDirOptions = TestTmpDirOptions
  { optionsCodegenDirectory :: FilePath
  }
  deriving (Eq, Ord, Show)

parseTestTmpDirOptions :: Parser TestTmpDirOptions
parseTestTmpDirOptions =
  TestTmpDirOptions
    <$> strOption
      ( long "codegen-directory"
          <> short 'd'
          <> metavar "DIRECTORY"
          <> value ""
          <> showDefault
          <> help
            ( "Alternative directory in which to generate plugin code "
                <> "(system temp dir if nothing is specified)"
            )
      )

-- | Get the path to do codegen in.  If path is passed with @-d@, use that.
-- If no path is passed, try @$TEST_TMPDIR@
-- (see <https://docs.bazel.build/versions/master/test-encyclopedia.html the bazel docs>).
-- If neither exists, error out.
computeTestTmpDir :: TestTmpDirOptions -> IO FilePath
computeTestTmpDir TestTmpDirOptions {..} = do
  path <- case optionsCodegenDirectory of
    "" -> lookupTestTmpDir -- if path not passed, get $TEST_TMPDIR
    r -> pure r -- if path is passed, use it
  putStrLn $ "Generating C code and objects under top-level path '" <> path <> "'."
  pure path

-- | Just get the default path.
lookupTestTmpDir :: IO FilePath
lookupTestTmpDir =
  fromMaybe (error "Please set codegen path with -d or $TEST_TMPDIR.") <$> lookupEnv "TEST_TMPDIR"
