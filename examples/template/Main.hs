{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

-- import Categorifier.C.Generate (writeCFiles)
import Categorifier.C.CExpr.Types.Core (CExpr)
import Categorifier.C.Codegen.FFI.ArraysCC (fromArraysCC)
import Categorifier.C.Codegen.FFI.Call (callSBVCFunction')
import Categorifier.C.Codegen.FFI.Spec (SBVSpec (..), Spec (..), mkSpec)
import Categorifier.C.KTypes.C (C)
import Categorifier.C.PolyVec (pdevectorize, pvectorize, pvlengths)
import Categorifier.C.Prim (Arrays (..))
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import F (Input (..), Output (..), wrap_f)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek, poke)
import PyF (fmt)
import TH

$(embedFunction "simple_example" wrap_f)

main :: IO ()
main = do
  let input = Input 1 5.0
  output <- fromArraysCC (Proxy @(Input -> Output)) c_simple_example input
  print output
