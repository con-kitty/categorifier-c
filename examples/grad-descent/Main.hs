{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Categorifier.C.Codegen.FFI.TH (embedFunction)
import Data.List (iterate)
import F
  ( Input (..),
    Output (..),
    dRosenbrock,
    rosenbrock,
    wrap_rosenbrockF,
  )

$(embedFunction "rosenbrock" wrap_rosenbrockF)

gamma :: Double
gamma = 0.01

step ::
  ((Double, Double) -> Double) ->
  ((Double, Double) -> (Double, Double)) ->
  (Double, Double) ->
  (Double, Double)
step f df (x0, y0) =
  let (dfdx, dfdy) = df (x0, y0)
      (x1, y1) = (x0 - gamma * dfdx, y0 - gamma * dfdy)
   in (x1, y1)

main :: IO ()
main = do
  let f = rosenbrock (1, 10)
      df = dRosenbrock (1, 10)
      hist = take 1500 $ iterate (step f df) (0.1, 0.4)
  mapM_ print hist

  z <- hs_rosenbrock (Input 1 10 0.1 0.4)
  let z' = f (0.1, 0.4)
  print (z, z')
