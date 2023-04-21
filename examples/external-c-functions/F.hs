{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module F (fCategorified) where

import CTypes
import qualified Categorifier.C.CExpr.Cat as C
import Categorifier.C.CExpr.Cat.TargetOb (TargetOb)
import Categorifier.C.CTypes.CGeneric (CGeneric)
import qualified Categorifier.C.CTypes.CGeneric as CG
import Categorifier.C.CTypes.GArrays (GArrays)
import Categorifier.C.KTypes.C (C)
import Categorifier.C.KTypes.Function (Callee (Imported), kForeignFunctionCall)
import qualified Categorifier.Categorify as Categorify
import Categorifier.Client (deriveHasRep)
import Data.Int (Int32, Int64)
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.Generics (Generic)

data Input = Input
  { iInt32 :: C Int32,
    iInt64 :: C Int64
  }
  deriving (Generic)

deriveHasRep ''Input

instance CGeneric Input

instance GArrays C Input

type instance TargetOb Input = TargetOb (CG.Rep Input ())

data Output = Output
  { oWord64 :: C Word64,
    oDouble :: C Double,
    oBool :: Bool
  }
  deriving (Generic)

deriveHasRep ''Output

instance CGeneric Output

instance GArrays C Output

type instance TargetOb Output = TargetOb (CG.Rep Output ())

f :: Input -> Output
f inp =
  Output
    { oWord64 = fromIntegral x,
      oDouble = z * 3.14,
      oBool = x >= y * 2
    }
  where
    COutput x y z =
      runVeryUsefulFunction $
        CInput
          { iX = fromIntegral (iInt32 inp) + 1,
            iY = iInt64 inp
          }

runVeryUsefulFunction :: CInput -> COutput
runVeryUsefulFunction =
  kForeignFunctionCall
    (Proxy @C)
    "Wrap_VeryUsefulFunction"
    (Imported "very_useful_function_wrapper.h")
    Nothing

fCategorified :: Input `C.Cat` Output
fCategorified = Categorify.expression f
