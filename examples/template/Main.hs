{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import F (Input (..), Output (..), wrap_f)
import TH (embedFunction)

$(embedFunction "simple_example" wrap_f)

main :: IO ()
main = do
  let input = Input 1 5.0
  output <- hs_simple_example input
  print output
