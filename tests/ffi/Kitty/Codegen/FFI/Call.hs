-- | Calling functions in SBV-generated C code.
--
-- These functions perform no checking. See 'module Kitty.Codegen.FFI.Spec'
-- for functions that can perform a variety of safety checks.
module Kitty.Codegen.FFI.Call
  ( -- * Call with immutable inputs
    callSBVCFunction,
    callSBVCFunction',
    callSBVCFunctionStorable,

    -- * Call with mutable inputs and outputs
    callSBVCFunctionInPlace,
    callSBVCFunctionInPlaceV,
  )
where

import qualified Barbies
import qualified Control.Monad.Trans.Cont as Cont
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import Foreign.C.Types (CBool (..))
import qualified Foreign.Marshal.Array as Marshal
import qualified Foreign.Marshal.Utils as Marshal (fromBool, toBool)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import qualified Kitty.Barbies as Barbies
import Kitty.Codegen.FFI.Spec (SBVCFunction (..), SBVFunCall, SBVSpec, Spec (..))
import Kitty.Prim (ArrayCount (..), Arrays (Arrays, arrayBool))

withArrayPointer ::
  Storable a => Int -> Ptr a -> (Ptr a -> IO b) -> IO b
withArrayPointer len arr f =
  -- If we're dealing with an empty array, create 1 spot so SBV can
  -- read from it or write to it.
  if len > 0 then f arr else Marshal.allocaArray 1 f

-- | This function does some special handling, because Haskell 'Bool's are not guaranteed to be
-- represented in a way that C understands. We have to allocate temporary arrays of C booleans
-- ('Data.Word.Word8' under the hood) and pass those to and from the function.
withBoolArrayPointers ::
  Int ->
  Int ->
  Ptr Bool ->
  Ptr Bool ->
  ((Ptr CBool, Ptr CBool) -> IO b) ->
  IO b
withBoolArrayPointers inLen outLen inVec outVec f =
  -- Stack-allocate temporary 'Bool' vectors (I think this makes it
  -- easier on the GC?)
  Marshal.allocaArray inArrLen $ \inArrPtr ->
    Marshal.allocaArray outArrLen $ \outArrPtr -> do
      -- Copy the incoming 'Bool's to the stack array, converting to
      -- 'Word8'. This is done via lists ('peekArray', 'pokeArray')
      -- rather than an explicit mutating loop, hopefully it gets
      -- optimized.
      Marshal.peekArray inLen inVec
        >>= Marshal.pokeArray inArrPtr . fmap Marshal.fromBool
      -- Run the action
      ret <- f (inArrPtr, outArrPtr)
      -- Copy the outputs of the array we gave to the C function
      -- back into the user array of 'Bool's.
      Marshal.peekArray outLen outArrPtr
        >>= Marshal.pokeArray outVec . fmap Marshal.toBool
      pure ret
  where
    -- We have to handle the empty-input case here to give SBV
    -- something to read from -- this function doesn't compose
    -- well with 'withArrayPointer'.
    inArrLen = if inLen <= 0 then 1 else inLen
    outArrLen = if outLen <= 0 then 1 else outLen

-- | This function takes the provided SBV-generated foreign object and
-- calls it with the given pointer arguments.
--
-- The input and output are allowed to be the same values if the
-- function's input and output are the same type and size. This is
-- doubly safe (as in-place updates go), because the first thing a
-- SBV-generated C function does is copy everything out of its input
-- arrays, and the last thing it does after all calculation is
-- complete is set output array values.
callSBVCFunctionInPlace' ::
  -- | Function to call
  SBVFunCall ->
  -- | Input and output specs.
  -- We still need to use the given spec, because our incoming
  -- 'Ptr's don't know how many boolean elements they point to.
  SBVSpec ->
  -- | Input values (won't be mutated)
  Arrays Ptr ->
  -- | Output values (will be mutated)
  Arrays Ptr ->
  -- | Possible exception
  IO ()
callSBVCFunctionInPlace' function (Spec inputSpec outputSpec) inputs outputs =
  withBoolArrayPointers
    (getBoolSize inputSpec)
    (getBoolSize outputSpec)
    (arrayBool inputs)
    (arrayBool outputs)
    $ \(bp, obp) ->
      -- Set up all remaining pointers
      Cont.runContT (Barbies.bzipWithMC @MSV.Storable contArrayPointer inputSpec inputs) $
        \(Arrays _ i8p i16p i32p i64p w8p w16p w32p w64p fp dp) ->
          Cont.runContT (Barbies.bzipWithMC @MSV.Storable contArrayPointer outputSpec outputs) $
            \(Arrays _ oi8p oi16p oi32p oi64p ow8p ow16p ow32p ow64p ofp odp) ->
              function
                -- Input pointers. We use 'coerce' to penetrate the
                -- 'Foreign.C.Types' newtype wrapping for free. Unfortunately
                -- applying this in a HK1 fashion doesn't work too well,
                -- because 'Arrays' is fixed to the base Haskell types; you'd
                -- need some kind of type family mapping between the C and
                -- Haskell types.
                bp
                i8p
                i16p
                i32p
                i64p
                w8p
                w16p
                w32p
                w64p
                fp
                dp
                -- Output pointers are the same.
                obp
                oi8p
                oi16p
                oi32p
                oi64p
                ow8p
                ow16p
                ow32p
                ow64p
                ofp
                odp
  where
    contArrayPointer (ArrayCount i) = Cont.ContT . withArrayPointer i
    getBoolSize = unArrayCount . arrayBool

callSBVCFunctionInPlace ::
  -- | Function to call
  SBVCFunction ->
  -- | Input values (won't be mutated)
  Arrays Ptr ->
  -- | Output values (will be mutated)
  Arrays Ptr ->
  -- | Possible exception
  IO ()
callSBVCFunctionInPlace function =
  callSBVCFunctionInPlace' (sbvCFunCall function) (sbvCFunSpec function)

callSBVCFunctionInPlaceV' ::
  SBVFunCall ->
  SBVSpec ->
  Arrays MSV.IOVector ->
  Arrays MSV.IOVector ->
  IO ()
callSBVCFunctionInPlaceV' function spec inputs outputs =
  Cont.runContT (Barbies.btraverseC @MSV.Storable (Cont.ContT . MSV.unsafeWith) inputs) $ \inArr ->
    Cont.runContT (Barbies.btraverseC @MSV.Storable (Cont.ContT . MSV.unsafeWith) outputs) $
      \outArr -> callSBVCFunctionInPlace' function spec inArr outArr

callSBVCFunctionInPlaceV ::
  SBVCFunction ->
  Arrays MSV.IOVector ->
  Arrays MSV.IOVector ->
  IO ()
callSBVCFunctionInPlaceV function =
  callSBVCFunctionInPlaceV' (sbvCFunCall function) (sbvCFunSpec function)

callSBVCFunctionStorable' ::
  SBVFunCall ->
  SBVSpec ->
  Arrays SV.Vector ->
  IO (Arrays SV.Vector)
callSBVCFunctionStorable' function spec inputs = do
  mInputs <- Barbies.btraverseC @MSV.Storable SV.thaw inputs
  mOutputs <- Barbies.btraverseC @MSV.Storable (MSV.unsafeNew . unArrayCount) (specOutput spec)
  callSBVCFunctionInPlaceV' function spec mInputs mOutputs
  Barbies.btraverseC @MSV.Storable SV.unsafeFreeze mOutputs

callSBVCFunctionStorable ::
  SBVCFunction ->
  Arrays SV.Vector ->
  IO (Arrays SV.Vector)
callSBVCFunctionStorable function =
  callSBVCFunctionStorable' (sbvCFunCall function) (sbvCFunSpec function)

callSBVCFunction' ::
  SBVFunCall ->
  SBVSpec ->
  Arrays V.Vector ->
  IO (Arrays V.Vector)
callSBVCFunction' function spec =
  fmap (Barbies.bmapC @MSV.Storable SV.convert)
    . callSBVCFunctionStorable' function spec
    . Barbies.bmapC @MSV.Storable SV.convert

callSBVCFunction ::
  SBVCFunction ->
  Arrays V.Vector ->
  IO (Arrays V.Vector)
callSBVCFunction function =
  callSBVCFunction' (sbvCFunCall function) (sbvCFunSpec function)
