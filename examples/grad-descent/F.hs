{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module F
  ( rosenbrock,
    dRosenbrock,
  )
where

import Data.Reflection (Reifies)
import Numeric.AD (grad)
import Numeric.AD.Internal.Reverse (Tape)
import Numeric.AD.Mode.Reverse (Reverse)

rosenbrock :: RealFloat a => (a, a) -> (a, a) -> a
rosenbrock (a, b) (x, y) = (a - x) ^ 2 + b * (y - x ^ 2) ^ 2

dRosenbrock :: (Double, Double) -> (Double, Double) -> (Double, Double)
dRosenbrock (a, b) (x, y) =
  let rosenbrock' :: forall s. Reifies s Tape => [Reverse s Double] -> Reverse s Double
      rosenbrock' [x', y'] =
        let a' = realToFrac a
            b' = realToFrac b
         in rosenbrock (a', b') (x', y')
      [dfdx, dfdy] = grad rosenbrock' [x, y]
   in (dfdx, dfdy)
