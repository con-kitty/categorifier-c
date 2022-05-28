{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Categorifier.C.Codegen.FFI.TH (embedFunction)
import F (Input (..), Output (..), wrap_f)

$(embedFunction "simple_example" wrap_f)

main :: IO ()
main = do
  let input = Input 1 5.0
  output <- hs_simple_example input
  print output
