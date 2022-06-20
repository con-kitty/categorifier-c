{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Categorifier.C.Generate (writeCFiles)
import F (wrap_f)

-- This generates /tmp/auto_interpret.c
main :: IO ()
main = writeCFiles "/tmp" "auto_interpret" wrap_f
