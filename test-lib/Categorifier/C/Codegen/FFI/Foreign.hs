{-# LANGUAGE ForeignFunctionInterface #-}

-- | FFI declarations for C functions
module Categorifier.C.Codegen.FFI.Foreign
  ( -- * Working with 'FunPtr's
    ptrToFunSBVSpecSize,
    ptrToFunSBVSpec,
    ptrToFunSBV,
  )
where

import Categorifier.C.Codegen.FFI.Spec (SBVFunCall, SBVGetSpec, SBVGetSpecSize)
import Foreign.Ptr (FunPtr)

-- | Convert a function pointer to a function
foreign import ccall unsafe "dynamic"
  ptrToFunSBVSpecSize :: FunPtr SBVGetSpecSize -> SBVGetSpecSize

-- | Convert a function pointer to a function
foreign import ccall unsafe "dynamic"
  ptrToFunSBVSpec :: FunPtr SBVGetSpec -> SBVGetSpec

-- | Convert a function pointer to a function
foreign import ccall unsafe "dynamic"
  ptrToFunSBV :: FunPtr SBVFunCall -> SBVFunCall
