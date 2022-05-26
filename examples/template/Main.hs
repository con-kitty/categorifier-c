{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Categorifier.C.Generate (writeCFiles)
import F (fCategorified)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import PyF (fmt)
import TH

$(myFunction)

main :: IO ()
main = do
  writeCFiles "." "simple_example" fCategorified
  putStrLn
    "C code insertion using addForeignSource"
  c_test
  alloca $ \p_input_int32 -> do
    poke p_input_int32 1
    alloca $ \p_input_double -> do
      poke p_input_double 1.0
      alloca $ \p_output_bool ->
        alloca $ \p_output_uint64 ->
          alloca $ \p_output_float -> do
            c_simple_example
              nullPtr
              nullPtr
              nullPtr
              p_input_int32
              nullPtr
              nullPtr
              nullPtr
              nullPtr
              nullPtr
              nullPtr
              p_input_double
              p_output_bool
              nullPtr
              nullPtr
              nullPtr
              nullPtr
              nullPtr
              nullPtr
              nullPtr
              p_output_uint64
              p_output_float
              nullPtr
            output_b <- peek p_output_bool
            output_uint64 <- peek p_output_uint64
            output_float <- peek p_output_float
            putStrLn [fmt| b = {show output_b}, uint64 = {show output_uint64}, float = {show output_float}|]
