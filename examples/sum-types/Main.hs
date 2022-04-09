{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Categorifier.C.Generate (writeCFiles)
import F (fCategorified)

-- This generates /tmp/sum_types.c
main :: IO ()
main = writeCFiles "/tmp" "sum_types" fCategorified
