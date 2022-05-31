{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Categorifier.C.Codegen.FFI.ArraysCC (fromArraysCC)
import Categorifier.C.Codegen.FFI.Spec (SBVFunCall)
import Categorifier.C.Codegen.FFI.TH (embedFunction)
import Categorifier.C.Generate (writeCFiles)
import Categorifier.C.KTypes.C (C (unsafeC))
import Categorifier.C.KTypes.KLiteral (kliteral)
import Data.List (iterate)
import Data.Proxy (Proxy (..))
import F
  ( Input (..),
    Output (..),
    Param (..),
    XY (..),
    dRosenbrock,
    rosenbrock,
    wrap_dRosenbrockF,
    wrap_rosenbrockF,
  )

$(embedFunction "rosenbrockF" wrap_rosenbrockF)

$(embedFunction "dRosenbrockF" wrap_dRosenbrockF)

gamma :: Double
gamma = 0.01

step ::
  ((Double, Double) -> IO Double) ->
  ((Double, Double) -> IO (Double, Double)) ->
  (Double, Double) ->
  IO (Double, Double)
step f df (x0, y0) = do
  (dfdx, dfdy) <- df (x0, y0)
  let (x1, y1) = (x0 - gamma * dfdx, y0 - gamma * dfdy)
  pure (x1, y1)

iterateNM :: (Monad m) => Int -> (a -> m a) -> a -> m [a]
iterateNM n f x0 = go n f x0 id
  where
    go k f x acc
      | k > 0 = do
        y <- f x
        go (k - 1) f y (acc . (y :))
      | otherwise = pure (acc [])

main :: IO ()
main = do
  let (x0, y0) = (0.1, 0.4)
  -- pure haskell
  putStrLn "pure haskell"
  let f = pure . rosenbrock (1, 10)
      df = pure . dRosenbrock (1, 10)
  histH <- iterateNM 10 (step f df) (x0, y0)
  mapM_ print histH

  -- C
  putStrLn "codegen C"
  let g (x, y) = do
        Output z <- hs_rosenbrockF (Input (Param 1 10) (XY (kliteral x) (kliteral y)))
        pure (unsafeC z)
      dg (x, y) = do
        XY x' y' <- hs_dRosenbrockF (Input (Param 1 10) (XY (kliteral x) (kliteral y)))
        pure (unsafeC x', unsafeC y')
  histC <- iterateNM 10 (step g dg) (x0, y0)
  mapM_ print histC
