{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Categorifier.C.Generate (writeCFiles, writeCWrapperFiles)
import F (fCategorified)

-- This generates /tmp/low_level.c(h) and /tmp/wrapper.c(h), as well as the modules
-- wrapper.c(h) depends on.
main :: IO ()
main = do
  let lowLevelName = "low_level"
      dir = "/tmp"
  writeCFiles dir lowLevelName fCategorified
  writeCWrapperFiles dir "wrapper" lowLevelName fCategorified
