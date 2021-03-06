{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module F (fCategorified) where

import qualified Categorifier.C.CExpr.Cat as C
import Categorifier.C.CExpr.Cat.TargetOb (TargetOb)
import Categorifier.C.CTypes.CGeneric (CGeneric)
import qualified Categorifier.C.CTypes.CGeneric as CG
import Categorifier.C.CTypes.GArrays (GArrays)
import Categorifier.C.KTypes.C (C)
import qualified Categorifier.Categorify as Categorify
import Categorifier.Client (deriveHasRep)
import Data.Int (Int32)
import Data.Word (Word64)
import G (g)
import GHC.Generics (Generic)

data Input = Input
  { iInt32 :: C Int32,
    iDouble :: C Double
  }
  deriving (Generic)

deriveHasRep ''Input

instance CGeneric Input

instance GArrays C Input

type instance TargetOb Input = TargetOb (CG.Rep Input ())

data Output = Output
  { oWord64 :: C Word64,
    oFloat :: C Float,
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
    { oWord64 = g (iInt32 inp),
      oFloat = realToFrac $ min (iDouble inp) 3.14,
      oBool = iDouble inp > 0
    }

fCategorified :: Input `C.Cat` Output
fCategorified = Categorify.expression f
