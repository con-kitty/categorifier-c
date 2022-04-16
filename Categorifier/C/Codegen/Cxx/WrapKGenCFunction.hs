{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Generate C++ wrapper function for exported C function
module Categorifier.C.Codegen.Cxx.WrapKGenCFunction
  ( ArrayConversionFailure (..),
    FunctionExporterConfig (..),
    toKGenWrapper,
    declareArrayBuffers,
    CheckFinite (..),
  )
where

import Categorifier.C.CTypes.ArrayLengths (Mismatch, checkForMismatches)
import Categorifier.C.CTypes.Codegen.Arrays
  ( CheckFinite (..),
    NonFiniteField (..),
    ToOrFromArrays (..),
    WrittenArrayFunctions (..),
    incrementalConvertArrays',
    nonFiniteStatsParam,
    withArrayCounts,
  )
import Categorifier.C.CTypes.Codegen.Render.Render (renderModuleHeader, renderModuleSource)
import Categorifier.C.CTypes.DSL.CxxAst
  ( CExpr (..),
    CFunction (..),
    CTypeWithBackdoor (..),
    Comment (..),
    CxxModule (..),
    CxxOrC (..),
    CxxTarget (..),
    Define (..),
    Identifier (..),
    Include (..),
    Param (..),
    ParamType (..),
    SystemLib (..),
    TapeElement (..),
    (#!),
  )
import Categorifier.C.CTypes.DSL.FunctionWriter
  ( FunWriter,
    comment,
    emptyCArrayNamed,
    force_,
    newDefaultNamed,
    runFunWriter,
    zeroCArrayNamed,
    (=:),
  )
import Categorifier.C.CTypes.ToCxxType (ToCxxType (..))
import Categorifier.C.CTypes.Types (CType, CTypeF (..), CxxType (..), makeRfName)
import Categorifier.C.Codegen.ToKioTypes (KioType (..))
import Categorifier.C.KTypes.C (C, toCxxTypeViaC)
import Categorifier.C.Prim (ArrayCount (..), Arrays, Prim, getPrimCName)
import Control.Monad.Trans.RWS (tell)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Bifunctor (first)
import Data.Foldable (sequenceA_)
import Data.Functor (void)
import Data.Int (Int32, Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Generic1)
import PyF (fmt)
import qualified Text.Casing as Casing

data AutogenFunTiming a = AutogenFunTiming
  { aftToArrays :: a,
    aftEvaluation :: a,
    aftFromArrays :: a
  }
  deriving (Functor, Foldable, Traversable, Eq, Ord, Generic, Generic1, Show)

instance (ToCxxType C a, Typeable a) => ToCxxType C (AutogenFunTiming a)

-- | same as C's struct timespec
-- time_t tv_sec whole seconds (valid values are >= 0)
-- long tv_nsec  nanoseconds (valid values are [0, 999999999])
--
-- >>> toInteger (maxBound :: Word32) > 999999999
-- True
--
-- TODO(greg): Consider adding phantom type for which clock it's relative to.
data Timespec = Timespec
  { tvSec :: C Int64,
    tvNSec :: C Int32
  }
  deriving (Generic)

instance ToCxxType C Timespec

-- | Difference between two 'Timespec's.
--
-- This value may represent any time difference, positive or negative.
newtype TimeDifference = TimeDifference
  { -- | Unlike above, this 'Timespec' is allowed to have a negative 'tvSec' field.  The
    --   nanoseconds field is always positive; thus -1 nanosecond in time difference is
    --   represented by -1 second and 999999999 nanoseconds.
    unTimeDifference :: Timespec
  }
  deriving (Generic)

instance ToCxxType C TimeDifference

data IOParam = IOParam
  { iopCType :: CType Proxy,
    iopArrayCounts :: Arrays ArrayCount,
    iopParamType :: ParamType,
    iopIdentifier :: Identifier
  }

data FunctionExporterConfig = FunctionExporterConfig
  { fecGenerateTimingInfo :: Bool,
    fecCheckFinite :: CheckFinite
  }

data ArrayConversionFailure
  = InputArraysMismatch [KioType] (Arrays Mismatch)
  | OutputArraysMismatch [KioType] (Arrays Mismatch)
  | InternalConstructorMismatches (NonEmpty (T.Text, Arrays Mismatch))

toKGenWrapper ::
  FunctionExporterConfig ->
  [KioType] ->
  [KioType] ->
  T.Text ->
  T.Text ->
  Either ArrayConversionFailure (T.Text, T.Text)
toKGenWrapper config inputIOTypes outputIOTypes wrapperName lowLevelName = do
  (inputsToArrays, totalInputArrayLengths) <-
    runConverterHack . incrementalConvertArrays' "input" ToArrays' $
      fmap toIncrementalParams inputParams
  (outputsFromArrays, totalOutputArrayLengths) <-
    runConverterHack . incrementalConvertArrays' "output" FromArrays' $
      fmap toIncrementalParams outputParams
  let body :: [TapeElement]
      body = runFunWriter $ do
        allocateBuffers
        timer <- allocateTimer
        -- TODO: something better than tell here
        timeit "aftToArrays" timer (tell inputsToArrays)
        timeit "aftEvaluation" timer callCFunction
        timeit "aftFromArrays" timer (tell outputsFromArrays)
      wrapperFunction =
        UnsafeCFunction
          { cfName = Identifier wrapperFunctionName,
            cfInlineOverloadName = Nothing,
            cfReturnType = Nothing,
            cfStaticLinkage = False,
            cfComment =
              Just $
                Comment
                  [fmt|Higher level typed wrapper around autogenerated C function {lowLevelName}|],
            cfParams =
              let toFunctionParam mutable p =
                    Param
                      { pType = iopParamType p,
                        pId = iopIdentifier p,
                        pUnused = False,
                        pMutable = mutable
                      }
                  timerParam =
                    Param
                      { pType =
                          ParamCxxType . toCxxTypeViaC $
                            Proxy @(AutogenFunTiming TimeDifference),
                        pId = Identifier "timing",
                        pUnused = False,
                        pMutable = True
                      }
               in fmap (toFunctionParam False) inputParams
                    <> fmap (toFunctionParam True) outputParams
                    <>
                    -- maybe add non-finite param
                    [ toNonFiniteParam param
                      | fecCheckFinite config == CheckFinite,
                        param <- outputParams
                    ]
                    <> [ nonFiniteStatsParam
                         | fecCheckFinite config == CheckFinite
                       ]
                    -- maybe add timing param
                    <> [ timerParam
                         | fecGenerateTimingInfo config
                       ],
            cfTape = body
          }
      wrapperModule =
        CxxModule
          { moduleIncludes =
              [ IncludeSystemLib Time,
                IncludeModule CTypes
              ],
            moduleDefines = [],
            moduleTypedefs = [],
            moduleUsingDecls = [],
            moduleFunctions = [wrapperFunction],
            moduleTypeLevelFunctions = []
          }
  maybe
    ( maybe
        ( pure
            ( renderModuleSource moduleLanguage wrapperModule sourceExtraIncludes,
              renderModuleHeader moduleLanguage wrapperModule
            )
        )
        (Left . OutputArraysMismatch outputIOTypes)
        $ checkForMismatches outputTotalLengths totalOutputArrayLengths
    )
    (Left . InputArraysMismatch inputIOTypes)
    $ checkForMismatches inputTotalLengths totalInputArrayLengths
  where
    moduleLanguage = C
    sourceExtraIncludes =
      [ IncludeThatNeedsDefines [Define Nothing "_POSIX_C_SOURCE" "199309L"] $
          IncludeSystemLib Time,
        IncludeLocalFile [fmt|{wrapperName}.h|],
        IncludeLocalFile [fmt|{lowLevelName}.h|],
        IncludeModule FromArrays,
        IncludeModule ToArrays
      ]
        <>
        -- include diff_timespec if we're timing this function
        [ IncludeLocalFile "time.h"
          | fecGenerateTimingInfo config
        ]
    wrapperFunctionName :: T.Text
    wrapperFunctionName = T.pack $ Casing.pascal (T.unpack wrapperName)
    inputParams = zipWith (toParam False) [(0 :: Int) ..] inputIOTypes
    outputParams = zipWith (toParam True) [(0 :: Int) ..] outputIOTypes
    inputTotalLengths = foldMap iopArrayCounts inputParams
    outputTotalLengths = foldMap iopArrayCounts outputParams
    toNonFiniteParam :: IOParam -> Param
    toNonFiniteParam (IOParam _cty _counts pty ident) =
      Param
        { pId = Identifier . (<> "_when_nonfinite") $ unIdentifier ident,
          pType = pty,
          pUnused = False,
          pMutable = True
        }
    toParam :: Bool -> Int -> KioType -> IOParam
    toParam isOutput k kiot =
      IOParam
        { iopCType = ctype,
          iopArrayCounts = kiotArraySizes kiot,
          iopParamType = ParamCxxType (CxxTypeCType ctype),
          iopIdentifier = Identifier [fmt|{prefix}_{k}|]
        }
      where
        ctype = kiotCType kiot
        prefix = if isOutput then "output" else "input" :: T.Text
    toIncrementalParams :: IOParam -> (CExpr, CType Proxy, NonFiniteField)
    toIncrementalParams iop =
      ( Ident (iopIdentifier iop),
        iopCType iop,
        case fecCheckFinite config of
          CheckFinite ->
            DistinctArgument
              . Identifier
              . (<> "_when_nonfinite")
              . unIdentifier
              $ iopIdentifier iop
          Don'tCheckFinite ->
            NoField
      )

    -- Hack!
    -- Right now just run the function writer and get the tapes, ignoring all
    -- the functions it's writing. In the future we could skip creating array functions
    -- for all types and only derive them from what this function needs.
    runConverterHack ::
      StateT WrittenArrayFunctions (Either (NonEmpty (T.Text, Arrays Mismatch))) a ->
      Either ArrayConversionFailure a
    runConverterHack =
      first InternalConstructorMismatches . flip evalStateT (WrittenArrayFunctions [] mempty)
    (declareInputBuffers, inputBufferNames) = declareArrayBuffers "input" inputTotalLengths
    (declareOutputBuffers, outputBufferNames) = declareArrayBuffers "output" outputTotalLengths
    getClock t =
      force_ $ Identifier "clock_gettime" #! [Ident (Identifier "CLOCK_MONOTONIC"), TakeAddress t]
    timeit _fieldName Nothing x = x
    timeit fieldName (Just (tic, toc, ktic, ktoc)) x = do
      getClock tic
      x
      getClock toc
      ktic =: (Identifier "FromSystemTimespec" #! [tic])
      ktoc =: (Identifier "FromSystemTimespec" #! [toc])
      Ident (Identifier "timing") :-> makeRfName fieldName
        =: (Identifier "DiffTime" #! [ktoc, ktic])
    allocateTimer
      | fecGenerateTimingInfo config = do
          tic <- newDefaultNamed "tic" (CTypeBackdoor "struct timespec")
          toc <- newDefaultNamed "toc" (CTypeBackdoor "struct timespec")
          ktic <- newDefaultNamed "ktic" (CTypeBackdoor "Timespec")
          ktoc <- newDefaultNamed "ktoc" (CTypeBackdoor "Timespec")
          pure $ Just (tic, toc, ktic, ktoc)
      | otherwise = pure Nothing
    allocateBuffers = do
      comment "Input buffers."
      declareInputBuffers
      comment "Output buffers."
      declareOutputBuffers
    callCFunction = force_ $ Identifier lowLevelName #! ccallParams
      where
        ccallParams = inputBufferNames <> outputBufferNames

-- TODO(greg/guillaume): de-duplicate from ArrayTests. Where should it go?
declareArrayBuffers :: T.Text -> Arrays ArrayCount -> (FunWriter (), [CExpr])
declareArrayBuffers prefix arrayCounts = (sequenceA_ allocateBuffers, bufferNames)
  where
    (allocateBuffers, bufferNames) = unzip $ withArrayCounts f arrayCounts
    f :: Prim Proxy -> Int -> (FunWriter (), CExpr)
    f cprim count = (decl, Ident bufferName)
      where
        bufferName = Identifier [fmt|{prefix}_{ctype}|]
        ctype = getPrimCName cprim
        decl
          | count == 0 = do
              comment "Initialize because it's really length 0."
              void $ zeroCArrayNamed bufferName (CTypePrim cprim) 1
          | otherwise =
              void $ emptyCArrayNamed bufferName (CTypePrim cprim) count