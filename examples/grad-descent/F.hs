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

{-
data Param = Param
  { paramA :: C Double,
    paramB :: C Double
  }
  deriving (Generic, Show)

deriveHasRep ''Param

instance CGeneric Param

instance GArrays C Param

type instance TargetOb Param = TargetOb (CG.Rep Param ())
-}

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

dummy :: (KType1 f) => XY f -> XY f
dummy (XY x y) =
  let f [x', y'] = x' * x' + y' * y'
      [dfdx, dfdy] = grad f [x, y]
   in XY dfdx dfdy

--  $(Categorify.separately 'dummy [t|C.Cat|] [pure [t|C|]])

$(Categorify.separately 'dummy [t|C.Cat|] [pure [t|C|]])

-- wrap_dummy :: KType1 f => C.Cat (XY f) (XY f)
-- wrap_dummy :: C.Cat (XY C) (XY C)
-- wrap_dummy :: XY C -> XY C
-- wrap_dummy = dummy -- Categorify.expression dummy

-- instance Category.NativeCat (->) "F.dummy" (XY C) (XY C) where
--   nativeK = wrap_dummy

-- dummy_ :: XY C -> XY C
-- dummy_ = kFunctionCall (Proxy @C) "dummy" dummy

-- $(Categorify.separately 'dummy_ [t|C.Cat|] [])

-- if x > 0 then x + 5 else 42

-- kFunctionCall (Proxy @C) "g" $

-- type instance TargetOb (XY C) = TargetOb (CG.Rep (XY C) ())

-- type instance TargetOb (XY C) = Float

{-
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

--------------------------------------

dRosenbrock_ {- Param -> -} :: (KType f) -> XY f -> XY f
dRosenbrock_ {- (Param a b) -} (XY x y) =
  let rosenbrock' :: forall s. Reifies s Tape => [Reverse s (C Double)] -> Reverse s (C Double)
      rosenbrock' [x', y'] = x' + y'
      {- let a' = 1 -- realToFrac a
            b' = 10 -- realToFrac b
         in rosenbrock (a', b') (x', y') -}
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
