{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Categorifier.C.Generate (writeCFiles)
import F (fCategorified)

-- This generates /tmp/simple_example.c
main :: IO ()
main = writeCFiles "/tmp" "simple_example" fCategorified
