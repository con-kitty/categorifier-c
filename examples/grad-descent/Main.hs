module Main where

import Data.List (iterate)
import F (dRosenbrock, rosenbrock)

gamma = 0.01

step f df (x0, y0) =
  let (dfdx, dfdy) = df (x0, y0)
      (x1, y1) = (x0 - gamma * dfdx, y0 - gamma * dfdy)
   in (x1, y1)

main :: IO ()
main = do
  let f = rosenbrock (1, 100)
      df = dRosenbrock (1, 100)
      (x0, y0) = (0, 0)
      (x1, y1) = step f df (x0, y0)
      (x2, y2) = step f df (x1, y1)
      hist = take 10 $ iterate (step f df) (0, 0)
  print hist
