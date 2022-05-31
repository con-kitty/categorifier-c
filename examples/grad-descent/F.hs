{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module F where

{-  ( Input (..),
    Output (..),
    Param (..),
    XY (..),
    rosenbrock,
    dRosenbrock,
    wrap_rosenbrockF,
    wrap_dRosenbrockF,
  ) -}

import qualified Categorifier.C.CExpr.Cat as C
import Categorifier.C.CExpr.Cat.TargetOb (TargetOb, TargetObTC1)
import Categorifier.C.CExpr.Types.Core (CExpr)
import Categorifier.C.CTypes.CGeneric (CGeneric)
import qualified Categorifier.C.CTypes.CGeneric as CG
import Categorifier.C.CTypes.GArrays (GArrays)
import Categorifier.C.KTypes.C (C)
import Categorifier.C.KTypes.Function (kFunctionCall)
import Categorifier.C.KTypes.KType1 (KType1)
import qualified Categorifier.Categorify as Categorify
import qualified Categorifier.Category as Category
import Categorifier.Client (deriveHasRep)
import Data.Int (Int32)
import Data.Proxy (Proxy (..))
import Data.Reflection (Reifies)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Numeric.AD (grad)
import Numeric.AD.Internal.Reverse (Reverse (Lift), Tape)

data Param f = Param
  { paramA :: f Double,
    paramB :: f Double
  }
  deriving (Generic)

deriving instance Show (Param C)

deriveHasRep ''Param

instance CGeneric (Param f)

instance GArrays f (Param f)

type instance TargetOb (Param f) = Param (TargetObTC1 f)

data XY f = XY
  { xyX :: f Double,
    xyY :: f Double
  }
  deriving (Generic)

deriving instance Show (XY C)

deriveHasRep ''XY

instance CGeneric (XY f)

instance GArrays f (XY f)

type instance TargetOb (XY f) = XY (TargetObTC1 f)

data Input f = Input
  { iParam :: Param f,
    iCoord :: XY f
  }
  deriving (Generic)

deriving instance Show (Input C)

deriveHasRep ''Input

instance CGeneric (Input f)

instance GArrays f (Input f)

type instance TargetOb (Input f) = Input (TargetObTC1 f)

newtype Output f = Output
  {oF :: f Double}
  deriving (Generic)

deriving instance Show (Output C)

deriveHasRep ''Output

instance CGeneric (Output f)

instance GArrays f (Output f)

type instance TargetOb (Output f) = Output (TargetObTC1 f)

rosenbrock :: Num a => (a, a) -> (a, a) -> a
rosenbrock (a, b) (x, y) = (a - x) ^ 2 + b * (y - x ^ 2) ^ 2

dRosenbrock :: forall a. Num a => (a, a) -> (a, a) -> (a, a)
dRosenbrock (a, b) (x, y) =
  let rosenbrock' :: forall s. Reifies s Tape => [Reverse s a] -> Reverse s a
      rosenbrock' [x', y'] =
        let a' = Lift a
            b' = Lift b
         in rosenbrock (a', b') (x', y')
      [dfdx, dfdy] = grad rosenbrock' [x, y]
   in (dfdx, dfdy)

rosenbrockF :: KType1 f => Input f -> Output f
rosenbrockF (Input (Param a b) (XY x y)) = Output $ rosenbrock (a, b) (x, y)

dRosenbrockF :: forall f. (KType1 f) => Input f -> XY f
dRosenbrockF (Input (Param a b) (XY x y)) =
  let (dfdx, dfdy) = dRosenbrock (a, b) (x, y)
   in XY dfdx dfdy

$(Categorify.separately 'rosenbrockF [t|C.Cat|] [pure [t|C|]])

$(Categorify.separately 'dRosenbrockF [t|C.Cat|] [pure [t|C|]])

{-
        let rosenbrock' :: forall s. Reifies s Tape => [Reverse s a] -> Reverse s a
            rosenbrock' [x', y'] =
              let a' = Lift a
                  b' = Lift b
               in rosenbrock (a', b') (x', y')
            [dfdx, dfdy] = grad rosenbrock' [x, y]
         in (dfdx, dfdy)
-}
{-
  let rosenbrock' :: forall s. Reifies s Tape => [Reverse s (f Double)] -> Reverse s (f Double)
      rosenbrock' [x', y'] =
        let a' = realToFrac a
            b' = realToFrac b
         in rosenbrock (a', b') (x', y')
      [dfdx, dfdy] = grad rosenbrock' [x, y]
   in XY dfdx dfdy
-}
-- dummy_ :: XY C -> XY C
-- dummy_ xy = kFunctionCall (Proxy @C) "dummy" dummy xy

{-
dRosenbrockF {- Param -> -} ::
  XY -> XY
dRosenbrockF (XY x y) =
  let p = Param 1 10
   in -- x' = gg x
      -- kFunctionCall (Proxy @C) "dRosenbrock_c" dummy (XY x y)

      -- dummy 2 (XY x y)
      -- kFunctionCall (Proxy @C) "dRosenbrock_c" dRosenbrock_ (XY x y)
-}

{- dummydRosenbrock_ p -}

--  $(Categorify.function 'rosenbrockF [t|C.Cat|] [])

--  $(Categorify.function 'dRosenbrockF [t|C.Cat|] [])

--  $(Categorify.function 'dummy_ [t|C.Cat|] [])

--  $(Categorify.separately 'dRosenbrock_ [t|C.Cat|] [])

-- wrap_dRosenbrockF = id
