{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Specification types and checking for SBV-generated C code.
--
-- Nothing in this module lets you actually call the SBV-generated C
-- function. See 'module Kitty.Codegen.FFI.Call' for functions that can do
-- this.
module Kitty.Codegen.FFI.Spec
  ( -- * FFI call types
    SBVGetSpecSize,
    SBVGetSpec,
    SBVFunCall,

    -- * A generated function
    SBVCFunction (..),
    HasSBVCFunction (..),

    -- * Function specifications
    FFIArrayCount (..),
    Spec (..),
    HasSpec (..),
    SBVSpec,
    SBVFFISpec,
    arrayCountsToSpec,
    mkSpec,
    specToFFISpec,

    -- * Checking function specifications
    checkSBVCFunctionSpec,
    checkSBVCFunctionIOV,
    checkSBVCFunctionInputs,
    checkSBVCFunctionV,
    checkSBVCFunction,

    -- ** Checking function specifications against sizes
    checkSBVCFunctionSizesIO,
    checkSBVCFunctionSizeInputs,
    checkSBVCFunctionSizes,
    checkSBVCFunctionSize,

    -- ** Function specification errors
    SpecMismatchInfo (..),
    HasSpecMismatchInfo (..),
    prettySpecMismatchInfo,
    SpecMismatch (..),
    AsSpecMismatch (..),
    prettySpecMismatch,
    CallError (..),
    AsCallError (..),
    prettyCallError,
  )
where

import qualified Barbies
import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Control.Lens (makeClassyPrisms, makeLensesWith, set)
import Control.Monad (when)
import Data.Bifunctor (bimap, first)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import Data.Semigroup (getSum)
import Data.Vector (Vector)
import qualified Data.Vector as V (length)
import qualified Data.Vector.Storable.Mutable as MSV
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CBool (..))
import qualified Foreign.Marshal.Array as Marshal
import Foreign.Ptr (Ptr)
import GHC.Generics (Generic)
import qualified Kitty.Common.IO.Exception as Exception (Exception)
import Kitty.KTypes.C (C)
import Kitty.Lens.Rules (kittyLensRules)
import Kitty.PolyVec (PolyVec, pvlengths)
import Kitty.Prim (ArrayCount (..), Arrays (..), countLenses)

-- | The type of a function call to find out the argument count for a
-- generated function (in one direction).
type SBVGetSpecSize =
  -- | Number of input or output
  -- arguments this function expects
  IO Word32

-- | The type of a function call to find out the argument sizes for a
-- generated function (in one direction).
type SBVGetSpec =
  -- | Pointer to an array of input or output counts
  Ptr Word32 ->
  -- | Array size (see 'SBVGetSpecSize' to find this out
  -- ahead of time)
  IO Word32

-- | The type of a function call to 'Kitty.CExpr.Types.Core.CExpr'-produced C code
-- at the FFI boundary.
type SBVFunCall =
  -- Boolean inputs
  Ptr CBool ->
  -- Signed integral inputs
  Ptr Int8 ->
  Ptr Int16 ->
  Ptr Int32 ->
  Ptr Int64 ->
  -- Unsigned integral inputs
  Ptr Word8 ->
  Ptr Word16 ->
  Ptr Word32 ->
  Ptr Word64 ->
  -- Floating-point inputs
  Ptr Float ->
  Ptr Double ->
  -- Boolean outputs
  Ptr CBool ->
  -- Signed integral outputs
  Ptr Int8 ->
  Ptr Int16 ->
  Ptr Int32 ->
  Ptr Int64 ->
  -- Unsigned integral outputs
  Ptr Word8 ->
  Ptr Word16 ->
  Ptr Word32 ->
  Ptr Word64 ->
  -- Floating-point outputs
  Ptr Float ->
  Ptr Double ->
  IO ()

-- | This is an 'Arrays ArrayCount' that is wrapped to indicate that
-- it contains FFI counts, i.e. no array size is less than 1.
newtype FFIArrayCount = FFIArrayCount {getFFIArrayCount :: Arrays ArrayCount}
  deriving newtype (Eq, Show, NFData, Serialise)

data Spec a = Spec
  { specInput :: a,
    specOutput :: a
  }
  deriving (Eq, Ord, Show, Generic, Functor)

instance NFData a => NFData (Spec a)

instance Serialise a => Serialise (Spec a)

makeLensesWith kittyLensRules ''Spec

type SBVSpec = Spec (Arrays ArrayCount)

mkSpec :: forall a b. (PolyVec C a, PolyVec C b) => Proxy a -> Proxy b -> SBVSpec
mkSpec a b = Spec (pvlengths (Proxy @C) a) (pvlengths (Proxy @C) b)

type SBVFFISpec = Spec FFIArrayCount

-- | This represents a single code-generated function. It contains
-- the specification helpers and some metadata as well. In generated
-- FFI bindings, this structure is pre-assembled for you. In plugin
-- bindings, this structure is calculated for you by the plugin
-- system.
data SBVCFunction = SBVCFunction
  { sbvCInputSpecSize :: SBVGetSpecSize,
    sbvCOutputSpecSize :: SBVGetSpecSize,
    sbvCInputSpec :: SBVGetSpec,
    sbvCOutputSpec :: SBVGetSpec,
    sbvCFunCall :: SBVFunCall,
    sbvCFunName :: String,
    sbvCFunSpec :: SBVSpec
  }

makeLensesWith kittyLensRules ''SBVCFunction

-- | This structure describes what went wrong when a function's
-- specification was checked.
data SpecMismatchInfo a = SpecMismatchInfo
  { specMismatchExpected :: Spec a,
    specMismatchGot :: Spec a
  }
  deriving (Eq, Show, Ord, Generic)

instance NFData a => NFData (SpecMismatchInfo a)

instance Serialise a => Serialise (SpecMismatchInfo a)

makeLensesWith kittyLensRules ''SpecMismatchInfo

-- | This function renders a 'SpecMismatchInfo' in human-readable form.
prettySpecMismatchInfo ::
  Show a =>
  -- | Contextual label (anything you want)
  String ->
  -- | The mismatch itself
  SpecMismatchInfo a ->
  String
prettySpecMismatchInfo ctx (SpecMismatchInfo (Spec ei eo) (Spec gi go)) =
  unlines
    [ "Specification mismatch (" <> ctx <> "):",
      "  Expected inputs: " <> show ei,
      "       Got inputs: " <> show gi,
      "",
      "  Expected outputs: " <> show eo,
      "       Got outputs: " <> show go
    ]

-- | A specification mismatch may be either in the number of function
-- arguments or in the sizes of the arguments.
data SpecMismatch a
  = -- | Mismatch in the number of arguments
    SpecMismatchArgCount (SpecMismatchInfo Word)
  | -- | Mismatch in the argument sizes
    SpecMismatchArgSize (SpecMismatchInfo a)
  deriving (Eq, Ord, Show, Generic)

instance NFData a => NFData (SpecMismatch a)

instance Serialise a => Serialise (SpecMismatch a)

makeClassyPrisms ''SpecMismatch

-- | This function renders a 'SpecMismatch' in human-readable form.
prettySpecMismatch :: Show a => SpecMismatch a -> String
prettySpecMismatch (SpecMismatchArgCount info) =
  prettySpecMismatchInfo "spec size" info
prettySpecMismatch (SpecMismatchArgSize info) =
  prettySpecMismatchInfo "argument count" info

-- | What went wrong when we called a C function?
data CallError
  = -- | The Haskell spec didn't match the C function
    SpecMismatchFFI (SpecMismatch FFIArrayCount)
  | -- | The inputs didn't match the Haskell spec
    SpecMismatchInputs (SpecMismatch (Arrays ArrayCount))
  deriving (Eq, Show, Generic)

makeClassyPrisms ''CallError

instance NFData CallError

instance Serialise CallError

instance Exception.Exception CallError

-- | This function renders a 'CallError' in human-readable form.
prettyCallError :: CallError -> String
prettyCallError (SpecMismatchFFI mismatch) =
  "FFI: " <> prettySpecMismatch mismatch
prettyCallError (SpecMismatchInputs mismatch) =
  "Inputs: " <> prettySpecMismatch mismatch

-- | This function transforms the 'Arrays ArrayCount' specification returned by
-- 'Kitty.PolyVec.PolyVec.pvlengths' into the "spec" for an FFI call.
--
-- This is needed because we can never use empty arrays with SBV-generated C code, but all
-- 'Kitty.CExpr.Types.Core.CExpr'-related C functions have an identical calling convention, wherein
-- two pointers to each primitive type are always passed in: one array for inputs and one array for
-- outputs, for each primitive type. When we hit an empty array, we bump its size to 1 to keep SBV
-- happy, but we have no need to look at its memory at either end (which means the original spec
-- is still needed to call correctly).
arrayCountsToSpec :: Arrays ArrayCount -> FFIArrayCount
arrayCountsToSpec = FFIArrayCount . Barbies.bmap (max (ArrayCount 1))

specToFFISpec :: SBVSpec -> SBVFFISpec
specToFFISpec (Spec ins outs) =
  Spec (arrayCountsToSpec ins) (arrayCountsToSpec outs)

-- | Checks an 'SBVCFunction's Haskell-side specification against the spec contained in the C
-- helpers built into the C object. Returns a validated 'SBVFFISpec' if there is no mismatch;
-- otherwise it returns a 'SpecMismatch' structure containing information about what went wrong.
checkSBVCFunctionSpec :: SBVCFunction -> IO (Either (SpecMismatch FFIArrayCount) SBVFFISpec)
checkSBVCFunctionSpec SBVCFunction {..} = do
  inputCount <- sbvCInputSpecSize
  outputCount <- sbvCOutputSpecSize
  Marshal.allocaArray (fromIntegral inputCount) $ \inputArr ->
    Marshal.allocaArray (fromIntegral outputCount) $ \outputArr -> do
      -- Ignore the results here; they should be the same as our
      -- 'inputCount' and 'outputCount' respectively.
      specInputCount <- sbvCInputSpec inputArr
      specOutputCount <- sbvCOutputSpec outputArr
      when (inputCount /= specInputCount) $
        badNews inputCount specInputCount "input"
      when (outputCount /= specOutputCount) $
        badNews outputCount specOutputCount "output"
      -- We have to do a `fromIntegral` conversion here -- we only use
      -- explicitly-signed types to call across the FFI, but all size
      -- calculations in Haskell are done using `Int`.
      inputSpec <-
        fmap fromIntegral
          <$> Marshal.peekArray (fromIntegral inputCount) inputArr ::
          IO [Int]
      outputSpec <-
        fmap fromIntegral
          <$> Marshal.peekArray (fromIntegral outputCount) outputArr ::
          IO [Int]
      let cInputSpec = formSpec inputSpec
          cOutputSpec = formSpec outputSpec
          cFunSpec = Spec cInputSpec cOutputSpec
          cFunSpecSizes@(cInputSize, cOutputSize) =
            (fromIntegral inputCount, fromIntegral outputCount) :: (Word, Word)
          decide
            | cFunSpecSizes /= hsSpecSizes =
                Left . SpecMismatchArgCount $
                  SpecMismatchInfo
                    (Spec hsInputSize hsOutputSize)
                    (Spec cInputSize cOutputSize)
            | cFunSpec /= sbvCFunFFISpec =
                Left . SpecMismatchArgSize $
                  SpecMismatchInfo
                    (Spec (specInput sbvCFunFFISpec) (specOutput sbvCFunFFISpec))
                    (Spec cInputSpec cOutputSpec)
            | otherwise = pure sbvCFunFFISpec
      pure decide
  where
    sbvCFunFFISpec = specToFFISpec sbvCFunSpec
    hsSpecSizes@(hsInputSize, hsOutputSize) =
      (getArraysSize mempty, getArraysSize mempty) :: (Word, Word)
    getArraysSize :: Integral a => Arrays ArrayCount -> a
    getArraysSize = getSum . Barbies.bfoldMap (const 1)
    formSpec :: [Int] -> FFIArrayCount
    formSpec =
      arrayCountsToSpec . foldr ($) mempty . zipWith set countLenses
    badNews specSz specRet iostr =
      error $
        unlines
          [ "Terrifying error in " <> iostr <> "s for function '"
              <> sbvCFunName
              <> "': C spec size is '"
              <> show specSz
              <> "', but the spec function returned '"
              <> show specRet
              <> "'!",
            "This should never happen; there's a bug in C generation."
          ]

-- | This function checks the given @inputSizes@ and @outputSizes@
-- vectors against the spec contained in the 'SBVCFunction'
-- @function@.
checkSBVCFunctionSizesIO ::
  -- | @function@
  SBVCFunction ->
  -- | @inputSizes@
  Arrays ArrayCount ->
  -- | @outputSizes@
  Arrays ArrayCount ->
  -- | Mismatch
  -- description in
  -- case of failure
  Either (SpecMismatch (Arrays ArrayCount)) ()
checkSBVCFunctionSizesIO function inputSizes outputSizes =
  -- Check that our arguments are compatible with the function.
  if inputSizes /= inputSpec || outputSizes /= outputSpec
    then
      Left . SpecMismatchArgSize . SpecMismatchInfo spec $
        Spec inputSizes outputSizes
    else Right ()
  where
    spec@(Spec inputSpec outputSpec) = sbvCFunSpec function

-- | This function checks the sizes of the given @input@ and @output@
-- vectors against the spec contained in the 'SBVCFunction'
-- @function@.
checkSBVCFunctionIOV ::
  -- | @function@
  SBVCFunction ->
  -- | @input@
  Arrays MSV.IOVector ->
  -- | @output@
  Arrays MSV.IOVector ->
  -- | Mismatch
  -- description in
  -- case of failure
  Either (SpecMismatch (Arrays ArrayCount)) ()
checkSBVCFunctionIOV function inputs outputs =
  checkSBVCFunctionSizesIO function inputSizes outputSizes
  where
    inputSizes = Barbies.bmapC @MSV.Storable (ArrayCount . MSV.length) inputs
    outputSizes = Barbies.bmapC @MSV.Storable (ArrayCount . MSV.length) outputs

checkSBVCFunctionSizeInputs ::
  -- | @function@
  SBVCFunction ->
  -- | @input@
  Arrays ArrayCount ->
  -- | Mismatch
  -- description in
  -- case of failure
  Either (SpecMismatch (Arrays ArrayCount)) ()
checkSBVCFunctionSizeInputs function inputSizes =
  if inputSizes /= inputSpec
    then
      Left . SpecMismatchArgSize . SpecMismatchInfo spec $
        Spec inputSizes outputSpec
    else Right ()
  where
    spec@(Spec inputSpec outputSpec) = sbvCFunSpec function

-- | This function checks the sizes of the given @input@ vectors
-- against the spec contained in the 'SBVCFunction' @function@.
checkSBVCFunctionInputs ::
  -- | @function@
  SBVCFunction ->
  -- | @input@
  Arrays Vector ->
  -- | Mismatch
  -- description in
  -- case of failure
  Either (SpecMismatch (Arrays ArrayCount)) ()
checkSBVCFunctionInputs function inputs =
  checkSBVCFunctionSizeInputs function inputSizes
  where
    inputSizes = Barbies.bmap (ArrayCount . V.length) inputs

checkSBVCFunction_ ::
  SBVCFunction ->
  (SBVCFunction -> Either (SpecMismatch (Arrays ArrayCount)) ()) ->
  IO (Either CallError ())
checkSBVCFunction_ function argCheck = do
  specResult <- bimap SpecMismatchFFI (const ()) <$> checkSBVCFunctionSpec function
  pure $ specResult *> argResult
  where
    argResult :: Either CallError ()
    argResult = first SpecMismatchInputs $ argCheck function

-- | A combination of 'checkSBVCFunctionSpec' and 'checkSBVCFunctionIOV'.
checkSBVCFunctionV ::
  SBVCFunction ->
  Arrays MSV.IOVector ->
  Arrays MSV.IOVector ->
  IO (Either CallError ())
checkSBVCFunctionV function inputs outputs =
  checkSBVCFunction_ function (\f -> checkSBVCFunctionIOV f inputs outputs)

-- | A combination of 'checkSBVCFunctionSpec' and 'checkSBVCFunctionSizesIO'.
checkSBVCFunctionSizes ::
  SBVCFunction ->
  Arrays ArrayCount ->
  Arrays ArrayCount ->
  IO (Either CallError ())
checkSBVCFunctionSizes function inputSizes outputSizes =
  checkSBVCFunction_ function (\f -> checkSBVCFunctionSizesIO f inputSizes outputSizes)

-- | A combination of 'checkSBVCFunctionSpec' and 'checkSBVCFunctionInputs'.
checkSBVCFunction ::
  SBVCFunction ->
  Arrays Vector ->
  IO (Either CallError ())
checkSBVCFunction function inputs =
  checkSBVCFunction_ function (`checkSBVCFunctionInputs` inputs)

-- | A combination of 'checkSBVCFunctionSpec' and 'checkSBVCFunctionSizeInputs'.
checkSBVCFunctionSize ::
  SBVCFunction ->
  Arrays ArrayCount ->
  IO (Either CallError ())
checkSBVCFunctionSize function inputSizes =
  checkSBVCFunction_ function (`checkSBVCFunctionSizeInputs` inputSizes)
