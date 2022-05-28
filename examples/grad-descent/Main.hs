module Main where

import F (dRosenbrock, rosenbrock)

main :: IO ()
main = do
  print (rosenbrock (1, 100) (0, 0))

  print (dRosenbrock (1, 100) (0, 0))
