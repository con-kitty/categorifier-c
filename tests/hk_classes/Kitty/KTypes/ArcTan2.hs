-- | Define a class that just does arctan2, so that we don't need RealFloat.
module Kitty.KTypes.ArcTan2
  ( ArcTan2 (..),
  )
where

import qualified Kitty.KTypes.Libm as Libm

-- $setup
-- |
-- >>> :{
--     let inf = 1/0
--         neginf = negate inf
-- :}

-- | doesn't require 'Prelude.RealFloat', used for overloading symbolics
class Floating a => ArcTan2 a where
  -- | @arctan2 y x@ computes the arctangent from two arguments.  The
  -- 'Double' and 'Float' instances call out to a sufficiently recent
  -- version of @libm@ to compute this.
  --
  -- The following test cases are the /full/ set of recommended
  -- function properties specified for function @atan2Pi()@ on page 45
  -- of the IEEE Std 754-2008 document.
  --
  -- >>> arctan2 0 (-0) :: Double
  -- 3.141592653589793
  -- >>> arctan2 (-0) (-0) :: Double
  -- -3.141592653589793
  -- >>> arctan2 0 0 :: Double
  -- 0.0
  -- >>> arctan2 (-0) 0 :: Double
  -- -0.0
  --
  -- prop> \x -> x < 0 ==> arctan2 (-0) x == (-pi :: Double)
  -- prop> \x -> x < 0 ==> arctan2 0 x == (pi :: Double)
  -- prop> \x -> x > 0 ==> arctan2 (-0) x == (-0 :: Double)
  -- prop> \x -> x > 0 ==> arctan2 0 x == (0 :: Double)
  -- prop> \y -> y < 0 ==> arctan2 y (-0) == (-pi / 2 :: Double)
  -- prop> \y -> y > 0 ==> arctan2 y 0 == (pi / 2 :: Double)
  -- prop> \y ->
  --         y > 0 && not (isNaN y || isInfinite y) ==> arctan2 y (negate $ 1/0) == (pi :: Double)
  -- prop> \y ->
  --         y < 0 && not (isNaN y || isInfinite y) ==> arctan2 y (negate $ 1/0) == (-pi :: Double)
  -- prop> \y -> y > 0 && not (isNaN y || isInfinite y) ==> arctan2 y (1/0) == (0 :: Double)
  -- prop> \y -> y < 0 && not (isNaN y || isInfinite y) ==> arctan2 y (1/0) == (-0 :: Double)
  -- prop> \x -> not (isNaN x || isInfinite x) ==> arctan2 (negate $ 1/0) x == (-pi/2 :: Double)
  -- prop> \x -> not (isNaN x || isInfinite x) ==> arctan2 (1/0) x == (pi/2 :: Double)
  --
  -- >>> arctan2 neginf neginf :: Double
  -- -2.356194490192345
  -- >>> arctan2 inf neginf :: Double
  -- 2.356194490192345
  -- >>> arctan2 neginf inf :: Double
  -- -0.7853981633974483
  -- >>> arctan2 inf inf :: Double
  -- 0.7853981633974483
  arctan2 :: a -> a -> a

instance ArcTan2 Double where arctan2 = Libm.libmAtan2

instance ArcTan2 Float where arctan2 = Libm.libmAtan2f
