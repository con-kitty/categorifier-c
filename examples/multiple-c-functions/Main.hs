{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Categorifier.C.Generate (writeCFiles)
import F (fCategorified)

-- This generates /tmp/multiple_c_functions.c, which contains two
-- C functions: `multiple_c_functions` (the main function), and `g`.
main :: IO ()
main = writeCFiles "/tmp" "multiple_c_functions" fCategorified
