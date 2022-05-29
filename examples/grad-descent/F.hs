{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module F
  ( Input (..),
    Output (..),
    Param (..),
    XY (..),
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
import Categorifier.C.KTypes.Function (kFunctionCall)
import qualified Categorifier.Categorify as Categorify
import Categorifier.Client (deriveHasRep)
import Data.Int (Int32)
import Data.Proxy (Proxy (..))
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

-----------

data Param = Param
  { paramA :: C Double,
    paramB :: C Double
  }
  deriving (Generic, Show)

deriveHasRep ''Param

instance CGeneric Param

instance GArrays C Param

type instance TargetOb Param = TargetOb (CG.Rep Param ())

data XY = XY
  { xyX :: C Double,
    xyY :: C Double
  }
  deriving (Generic, Show)

deriveHasRep ''XY

instance CGeneric XY

instance GArrays C XY

type instance TargetOb XY = TargetOb (CG.Rep XY ())

data Input = Input
  { iParam :: Param,
    iCoord :: XY
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

rosenbrockF :: Input -> Output
rosenbrockF (Input (Param a b) (XY x y)) = Output $ rosenbrock (a, b) (x, y)

data Input2 = Input2
  { i2X :: C Double,
    i2Y :: C Double
  }
  deriving (Generic, Show)

deriveHasRep ''Input2

instance CGeneric Input2

instance GArrays C Input2

type instance TargetOb Input2 = TargetOb (CG.Rep Input2 ())

data Output2 = Output2
  { oDFDX :: C Double,
    oDFDY :: C Double
  }
  deriving (Generic, Show)

deriveHasRep ''Output2

instance CGeneric Output2

instance GArrays C Output2

type instance TargetOb Output2 = TargetOb (CG.Rep Output2 ())

--------------------------------------

dRosenbrock_ :: Param -> XY -> XY
dRosenbrock_ (Param a b) (XY x y) =
  let rosenbrock' :: forall s. Reifies s Tape => [Reverse s (C Double)] -> Reverse s (C Double)
      rosenbrock' [x', y'] =
        let a' = realToFrac a
            b' = realToFrac b
         in rosenbrock (a', b') (x', y')
      [dfdx, dfdy] = grad rosenbrock' [x, y]
   in XY dfdx dfdy

dRosenbrockF :: Param -> XY -> XY
dRosenbrockF p =
  kFunctionCall (Proxy @C) "dRosenbrock_" (dRosenbrock_ p)

$(Categorify.function 'rosenbrockF [t|C.Cat|] [])

$(Categorify.function 'dRosenbrockF [t|C.Cat|] [])

-- wrap_dRosenbrockF = id
