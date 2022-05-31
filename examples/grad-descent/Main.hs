{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Categorifier.C.Codegen.FFI.ArraysCC (fromArraysCC)
import Categorifier.C.Codegen.FFI.Spec (SBVFunCall)
import Categorifier.C.Codegen.FFI.TH (embedFunction, embedFunctionCTemp)
import Categorifier.C.Generate (writeCFiles)
import Categorifier.C.KTypes.C (C)
import Data.List (iterate)
import Data.Proxy (Proxy (..))
import F

{-  ( Input (..),
    Output (..),
    Param (..),
    XY (..),
    dRosenbrock,
    rosenbrock,
    wrap_dRosenbrockF,
    wrap_rosenbrockF,
  )
-}
{-
$(embedFunction "rosenbrock" wrap_rosenbrockF)

$(embedFunction "dRosenbrock" wrap_dRosenbrockF)
-}

$(embedFunctionCTemp "dummy" wrap_dummy)

{-
foreign import ccall safe "dummy" c_dummy :: SBVFunCall

hs_dummy :: XY C -> IO (XY C)
hs_dummy input = fromArraysCC (Proxy @(XY C -> XY C)) c_dummy input
-}

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
  {-
    writeCFiles "/tmp" "dRosenbrock" wrap_dRosenbrockF

    let f = rosenbrock (1, 10)
        df = dRosenbrock (1, 10)
        hist = take 10 {- 1500 -} $ iterate (step f df) (0.1, 0.4)
    mapM_ print hist

    z <- hs_rosenbrock (Input (Param 1 10) (XY 0.1 0.4))
    let z' = f (0.1, 0.4)
    print (z, z')

    out3 <- hs_dRosenbrock (XY 0.1 0.4)
    print out3
  -}
  out3 <- hs_dummy (XY 0.1 0.4)
  print out3
