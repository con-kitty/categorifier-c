{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Categorifier.C.Generate (writeCFiles)
import F (fCategorified)

-- This generates /tmp/separate_categorification.c
main :: IO ()
main = writeCFiles "/tmp" "separate_categorification" fCategorified
