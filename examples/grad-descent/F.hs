{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module F
  ( Input (..),
    Output (..),
    rosenbrock,
    dRosenbrock,
    wrap_rosenbrockF,
    -- wrap_dRosenbrockF,
  )
where

import qualified Categorifier.C.CExpr.Cat as C
import Categorifier.C.CExpr.Cat.TargetOb (TargetOb)
import Categorifier.C.CTypes.CGeneric (CGeneric)
import qualified Categorifier.C.CTypes.CGeneric as CG
import Categorifier.C.CTypes.GArrays (GArrays)
import Categorifier.C.KTypes.C (C)
-- import Categorifier.C.KTypes.KLiteral (kliteral)
import qualified Categorifier.Categorify as Categorify
import Categorifier.Client (deriveHasRep)
import Data.Int (Int32)
import Data.Reflection (Reifies)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Numeric.AD (grad)
import Numeric.AD.Internal.Reverse (Tape)
import Numeric.AD.Mode.Reverse (Reverse)

rosenbrock :: RealFrac a => (a, a) -> (a, a) -> a
rosenbrock (a, b) (x, y) = (a - x) ^ 2 + b * (y - x ^ 2) ^ 2

dRosenbrock :: forall a. RealFrac a => (a, a) -> (a, a) -> (a, a)
dRosenbrock (a, b) (x, y) =
  let rosenbrock' :: forall s. Reifies s Tape => [Reverse s a] -> Reverse s a
      rosenbrock' [x', y'] =
        let a' = realToFrac a
            b' = realToFrac b
         in rosenbrock (a', b') (x', y')
      [dfdx, dfdy] = grad rosenbrock' [x, y]
   in (dfdx, dfdy)

data Input = Input
  { iA :: C Double,
    iB :: C Double,
    iX :: C Double,
    iY :: C Double
  }
  deriving (Generic, Show)

deriveHasRep ''Input

instance CGeneric Input

instance GArrays C Input

type instance TargetOb Input = TargetOb (CG.Rep Input ())

newtype Output = Output
  {oF :: C Double}
  deriving (Generic, Show)

deriveHasRep ''Output

instance CGeneric Output

instance GArrays C Output

type instance TargetOb Output = TargetOb (CG.Rep Output ())

data Output2 = Output2
  { oDFDX :: C Double,
    oDFDY :: C Double
  }
  deriving (Generic, Show)

deriveHasRep ''Output2

instance CGeneric Output2

instance GArrays C Output2

type instance TargetOb Output2 = TargetOb (CG.Rep Output2 ())

rosenbrockF :: Input -> Output
rosenbrockF (Input a b x y) = Output $ rosenbrock (a, b) (x, y)

dRosenbrockF :: Input -> Output2
dRosenbrockF (Input a b x y) =
  let (dfdx, dfdy) = dRosenbrock (a, b) (x, y)
   in Output2 dfdx dfdy

$(Categorify.function 'rosenbrockF [t|C.Cat|] [])

--  $(Categorify.function 'dRosenbrockF [t|C.Cat|] [])
