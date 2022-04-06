{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | Property-based tests for selected functions
module Main
  ( main,
  )
where

import Categorifier.C.CExpr.Types.Core (CExpr)
import Categorifier.C.CTypes.CGeneric (CGeneric)
import Categorifier.C.CTypes.GArrays (GArrays)
import Categorifier.C.CTypes.ToCxxType (ToCxxType)
import Categorifier.C.Hedgehog.Options
  ( HedgehogOptions (..),
    checkRNG,
    parseHedgehogOptionsWithDefaultNumTestExprs,
  )
import qualified Categorifier.C.Hedgehog.Paths as Paths
import Categorifier.C.KGenGenerate.FFI.JIT (BuildOptions (..), defaultBuildOptions, withJitFunction)
import Categorifier.C.KGenGenerate.Test.Equality (polyEqNaN)
import Categorifier.C.KTypes.C (C)
import Categorifier.C.KTypes.FromIntegral (KFromIntegral (..))
import Categorifier.C.KTypes.KDivisible (KDivisible (..))
import Categorifier.C.KTypes.KLiteral (KLiteral (..))
import Categorifier.C.KTypes.Round (KRound (..))
import Categorifier.C.PolyVec (PolyVec)
import Categorifier.C.Prim (IsPrimitive)
import Categorifier.C.Prim.Hedgehog (getPrimitiveGen)
import qualified Categorifier.Common.IO.Exception as Exception (bracket_)
import Control.Applicative ((<**>))
import qualified Control.Concurrent as Concurrent (getNumCapabilities)
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.QSem (QSem)
import qualified Control.Concurrent.QSem as QSem
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import qualified Hedgehog as H
import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    flag,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    progDesc,
    short,
  )
import System.Exit (exitFailure, exitSuccess)
import qualified System.IO as IO (hPutStrLn, stderr)

data PairOf a f = PairOf {_leftOfPair :: f a, _rightOfPair :: f a}
  deriving (Show, Eq, Generic)

instance CGeneric (PairOf a f)

instance GArrays f (f a) => GArrays f (PairOf a f)

newtype Identity1 a (f :: Type -> Type) = Identity1 {getIdentity1 :: f a}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord)

instance CGeneric (Identity1 a f)

instance GArrays f (f a) => GArrays f (Identity1 a f)

instance (Typeable f, Typeable a, ToCxxType f (f a)) => ToCxxType f (PairOf a f)

getCGen :: forall a m. (IsPrimitive a, H.MonadGen m) => m (C a)
getCGen = kliteral <$> getPrimitiveGen

testBinaryFunction ::
  forall a.
  ( PolyVec CExpr (CExpr a),
    PolyVec C (C a),
    IsPrimitive a
  ) =>
  ProgramConfig ->
  String ->
  (CExpr a -> CExpr a -> CExpr a) ->
  (C a -> C a -> C a) ->
  IO Bool
testBinaryFunction (ProgramConfig hedgeopts jitOpts) name kGenFun nativeFun =
  withJitFunction jitOpts (wrapCFunction kGenFun) $ \cFun ->
    checkRNG name hedgeopts . H.property $ do
      inputs <- H.forAll $ (,) <$> getCGen @a <*> getCGen
      nativeOutput <- H.eval $ uncurry nativeFun inputs
      cOutput <- H.evalIO $ callCFunction cFun inputs
      H.annotateShow cOutput
      H.annotateShow nativeOutput
      H.assert $ polyEqNaN cOutput nativeOutput
  where
    callCFunction f (a, b) = fmap getIdentity1 . f $ PairOf a b
    wrapCFunction f (PairOf a b) = Identity1 $ f a b

testUnaryFunction ::
  forall a.
  ( PolyVec CExpr (CExpr a),
    PolyVec C (C a),
    IsPrimitive a
  ) =>
  ProgramConfig ->
  String ->
  (CExpr a -> CExpr a) ->
  (C a -> C a) ->
  IO Bool
testUnaryFunction (ProgramConfig hedgeopts jitOpts) name kGenFun nativeFun =
  withJitFunction jitOpts (wrapCFunction kGenFun) $ \cFun ->
    checkRNG name hedgeopts . H.property $ do
      inputs <- H.forAll $ getCGen @a
      nativeOutput <- H.eval $ nativeFun inputs
      cOutput <- H.evalIO $ callCFunction cFun inputs
      H.annotateShow cOutput
      H.annotateShow nativeOutput
      H.assert $ polyEqNaN cOutput nativeOutput
  where
    callCFunction f = fmap getIdentity1 . f . Identity1
    wrapCFunction f = Identity1 . f . getIdentity1

testCastFunction ::
  forall b a.
  ( PolyVec CExpr (CExpr a),
    PolyVec C (C a),
    PolyVec CExpr (CExpr b),
    PolyVec C (C b),
    Show b,
    IsPrimitive a
  ) =>
  ProgramConfig ->
  String ->
  (CExpr a -> CExpr b) ->
  (C a -> C b) ->
  IO Bool
testCastFunction (ProgramConfig hedgeopts jitOpts) name kGenFun nativeFun =
  withJitFunction jitOpts (wrapCFunction kGenFun) $ \cFun ->
    checkRNG name hedgeopts . H.property $ do
      input <- H.forAll getCGen
      nativeOutput <- H.eval $ nativeFun input
      cOutput <- H.evalIO $ callCFunction cFun input
      H.annotateShow cOutput
      H.annotateShow nativeOutput
      H.assert $ polyEqNaN cOutput nativeOutput
  where
    callCFunction f = fmap getIdentity1 . f . Identity1
    wrapCFunction f = Identity1 . f . getIdentity1

functionProperties :: ProgramConfig -> [IO Bool]
functionProperties conf =
  -- Mods and divs
  [ testBinary @Int8 ("mod   Int8", kMod, kMod),
    testBinary @Int16 ("mod  Int16", kMod, kMod),
    testBinary @Int32 ("mod  Int32", kMod, kMod),
    testBinary @Int64 ("mod  Int64", kMod, kMod),
    testBinary @Int8 ("div   Int8", kDiv, kDiv),
    testBinary @Int16 ("div  Int16", kDiv, kDiv),
    testBinary @Int32 ("div  Int32", kDiv, kDiv),
    testBinary @Int64 ("div  Int64", kDiv, kDiv),
    testBinary @Word8 ("mod  Word8", kMod, kMod),
    testBinary @Word16 ("mod Word16", kMod, kMod),
    testBinary @Word32 ("mod Word32", kMod, kMod),
    testBinary @Word64 ("mod Word64", kMod, kMod),
    testBinary @Word8 ("div  Word8", kDiv, kDiv),
    testBinary @Word16 ("div Word16", kDiv, kDiv),
    testBinary @Word32 ("div Word32", kDiv, kDiv),
    testBinary @Word64 ("div Word64", kDiv, kDiv),
    -- abs
    testUnary @Int8 ("abs Int8", abs, abs),
    testUnary @Int16 ("abs Int16", abs, abs),
    testUnary @Int32 ("abs Int32", abs, abs),
    testUnary @Int64 ("abs Int64", abs, abs),
    testUnary @Word8 ("abs Word8", abs, abs),
    testUnary @Word16 ("abs Word16", abs, abs),
    testUnary @Word32 ("abs Word32", abs, abs),
    testUnary @Word64 ("abs Word64", abs, abs),
    testUnary @Float ("abs Float", abs, abs),
    testUnary @Double ("abs Double", abs, abs),
    -- negate
    testUnary @Int8 ("negate Int8", negate, negate),
    testUnary @Int16 ("negate Int16", negate, negate),
    testUnary @Int32 ("negate Int32", negate, negate),
    testUnary @Int64 ("negate Int64", negate, negate),
    testUnary @Word8 ("negate Word8", negate, negate),
    testUnary @Word16 ("negate Word16", negate, negate),
    testUnary @Word32 ("negate Word32", negate, negate),
    testUnary @Word64 ("negate Word64", negate, negate),
    testUnary @Float ("negate Float", negate, negate),
    testUnary @Double ("negate Double", negate, negate),
    -- signum
    testUnary @Int8 ("signum Int8", signum, signum),
    testUnary @Int16 ("signum Int16", signum, signum),
    testUnary @Int32 ("signum Int32", signum, signum),
    testUnary @Int64 ("signum Int64", signum, signum),
    testUnary @Word8 ("signum Word8", signum, signum),
    testUnary @Word16 ("signum Word16", signum, signum),
    testUnary @Word32 ("signum Word32", signum, signum),
    testUnary @Word64 ("signum Word64", signum, signum),
    testUnary @Float ("signum Float", signum, signum),
    testUnary @Double ("signum Double", signum, signum),
    -- Rounding
    testCast @Int8 ("cast Double -> Int8", kRoundDouble, kRoundDouble),
    testCast @Int16 ("cast Double -> Int16", kRoundDouble, kRoundDouble),
    testCast @Int32 ("cast Double -> Int32", kRoundDouble, kRoundDouble),
    testCast @Int64 ("cast Double -> Int64", kRoundDouble, kRoundDouble),
    testCast @Int8 ("cast Float -> Int8", kRoundFloat, kRoundFloat),
    testCast @Int16 ("cast Float -> Int16", kRoundFloat, kRoundFloat),
    testCast @Int32 ("cast Float -> Int32", kRoundFloat, kRoundFloat),
    testCast @Int64 ("cast Float -> Int64", kRoundFloat, kRoundFloat),
    testCast @Word8 ("cast Double -> Word8", kRoundDouble, kRoundDouble),
    testCast @Word16 ("cast Double -> Word16", kRoundDouble, kRoundDouble),
    testCast @Word32 ("cast Double -> Word32", kRoundDouble, kRoundDouble),
    testCast @Word64 ("cast Double -> Word64", kRoundDouble, kRoundDouble),
    testCast @Word8 ("cast Float -> Word8", kRoundFloat, kRoundFloat),
    testCast @Word16 ("cast Float -> Word16", kRoundFloat, kRoundFloat),
    testCast @Word32 ("cast Float -> Word32", kRoundFloat, kRoundFloat),
    testCast @Word64 ("cast Float -> Word64", kRoundFloat, kRoundFloat),
    -- Casting from integer
    testCast @Double @Int8 ("cast Int8 -> Double", kFromIntegral, kFromIntegral),
    testCast @Double @Int16 ("cast Int16 -> Double", kFromIntegral, kFromIntegral),
    testCast @Double @Int32 ("cast Int32 -> Double", kFromIntegral, kFromIntegral),
    testCast @Double @Int64 ("cast Int64 -> Double", kFromIntegral, kFromIntegral),
    testCast @Float @Int8 ("cast Int8 -> Float", kFromIntegral, kFromIntegral),
    testCast @Float @Int16 ("cast Int16 -> Float", kFromIntegral, kFromIntegral),
    testCast @Float @Int32 ("cast Int32 -> Float", kFromIntegral, kFromIntegral),
    testCast @Float @Int64 ("cast Int64 -> Float", kFromIntegral, kFromIntegral),
    testCast @Double @Word8 ("cast Word8 -> Double", kFromIntegral, kFromIntegral),
    testCast @Double @Word16 ("cast Word16 -> Double", kFromIntegral, kFromIntegral),
    testCast @Double @Word32 ("cast Word32 -> Double", kFromIntegral, kFromIntegral),
    testCast @Double @Word64 ("cast Word64 -> Double", kFromIntegral, kFromIntegral),
    testCast @Float @Word8 ("cast Word8 -> Float", kFromIntegral, kFromIntegral),
    testCast @Float @Word16 ("cast Word16 -> Float", kFromIntegral, kFromIntegral),
    testCast @Float @Word32 ("cast Word32 -> Float", kFromIntegral, kFromIntegral),
    testCast @Float @Word64 ("cast Word64 -> Float", kFromIntegral, kFromIntegral)
  ]
  where
    testBinary ::
      forall a.
      ( PolyVec CExpr (CExpr a),
        PolyVec C (C a),
        IsPrimitive a
      ) =>
      (String, CExpr a -> CExpr a -> CExpr a, C a -> C a -> C a) ->
      IO Bool
    testBinary = uncurry3 (testBinaryFunction conf)
    testUnary ::
      forall a.
      ( PolyVec CExpr (CExpr a),
        PolyVec C (C a),
        IsPrimitive a
      ) =>
      (String, CExpr a -> CExpr a, C a -> C a) ->
      IO Bool
    testUnary = uncurry3 (testUnaryFunction conf)
    testCast ::
      forall b a.
      ( PolyVec CExpr (CExpr a),
        PolyVec CExpr (CExpr b),
        PolyVec C (C a),
        PolyVec C (C b),
        Show b,
        IsPrimitive a
      ) =>
      (String, CExpr a -> CExpr b, C a -> C b) ->
      IO Bool
    testCast = uncurry3 (testCastFunction conf)

runInPool :: QSem -> IO Bool -> Concurrently Bool
runInPool semaphore =
  Concurrently
    . Exception.bracket_
      (QSem.waitQSem semaphore)
      (QSem.signalQSem semaphore)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

data Parallelism
  = Sequential
  | Parallel
  deriving (Eq, Ord, Show)

data Options = Options
  { optionsParallelism :: Parallelism,
    optionsHedgehog :: HedgehogOptions,
    optionsTestTmpDir :: Paths.TestTmpDirOptions
  }
  deriving (Eq, Ord, Show)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> flag
      Sequential
      Parallel
      ( long "parallel"
          <> short 'p'
          <> help "Run all properties in parallel using all available cores."
      )
    <*> parseHedgehogOptionsWithDefaultNumTestExprs 500
    <*> Paths.parseTestTmpDirOptions


opts :: ParserInfo Options
opts =
  info
    (parseOptions <**> helper)
    ( fullDesc
        <> progDesc desc
        <> header "randomized testing for specific code-generated functions"
    )
  where
    desc = "Uses hedgehog to generate a lot of inputs and test known-tricky functions"

data ProgramConfig = ProgramConfig
  { configHedgehog :: HedgehogOptions,
    configJITOptions :: BuildOptions
  }
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  Options parallel hedgeopts tmpDir <- execParser opts
  path <- Paths.computeTestTmpDir tmpDir
  jitOptions <- getJitOpts path
  semaphore <- computeNumRunners parallel >>= QSem.newQSem
  let properties = functionProperties (ProgramConfig hedgeopts jitOptions)
      propertyResults = runConcurrently $ traverse (runInPool semaphore) properties
  ret <- and <$> propertyResults
  if ret
    then exitSuccess
    else IO.hPutStrLn IO.stderr "Test failure!" *> exitFailure
  where
    computeNumRunners Sequential = pure 1
    computeNumRunners Parallel = do
      numCores <- Concurrent.getNumCapabilities
      -- Run with one core fewer than we have -- this is Bazel's strategy
      let numRunners = max 1 $ numCores - 1
      putStrLn $ "Running tests in " <> show numRunners <> " parallel threads."
      pure numRunners
    getJitOpts path = do
      defaultOpts <- defaultBuildOptions Nothing
      pure defaultOpts {buildOptOutputDirectory = Just path}
