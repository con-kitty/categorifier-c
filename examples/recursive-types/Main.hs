{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Categorifier.C.Generate (writeCFiles)
import F (fCategorified)

-- This generates /tmp/recursive_types.c
main :: IO ()
main = writeCFiles "/tmp" "recursive_types" fCategorified
