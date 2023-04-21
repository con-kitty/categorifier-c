{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CTypes where

import Categorifier.C.CExpr.Cat.TargetOb (TargetOb)
import Categorifier.C.CTypes.CGeneric (CGeneric)
import qualified Categorifier.C.CTypes.CGeneric as CG
import Categorifier.C.CTypes.GArrays (GArrays)
import Categorifier.C.CTypes.ToCxxType (ToCxxType)
import Categorifier.C.KTypes.C (C)
import Categorifier.Client (deriveHasRep)
import Data.Int (Int64)
import GHC.Generics (Generic)

data CInput = CInput
  { iX :: C Int64,
    iY :: C Int64
  }
  deriving (Generic)

deriveHasRep ''CInput

instance CGeneric CInput

instance GArrays C CInput

instance ToCxxType C CInput

type instance TargetOb CInput = TargetOb (CG.Rep CInput ())

data COutput = COutput
  { oSum :: C Int64,
    oDiff :: C Int64,
    oAvg :: C Double
  }
  deriving (Generic)

deriveHasRep ''COutput

instance CGeneric COutput

instance GArrays C COutput

instance ToCxxType C COutput

type instance TargetOb COutput = TargetOb (CG.Rep COutput ())
