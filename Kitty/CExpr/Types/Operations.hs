{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  Kitty.CExpr.Types.Operations
-- Copyright   :  (c) Kittyhawk 2018
-- License     :  AllRightsReversed
-- Maintainer  :  Matthew Peddie <matt.peddie@kittyhawk.aero>
-- Stability   :  provisional
-- Portability :  GHC
--
-- Core operation types for the post-SBV code-generation system.
module Kitty.CExpr.Types.Operations
  ( IntBinOp (..),
    IntUnOp (..),
    IntTestOp (..),
    IntSetBit (..),
    Round (..),
    FPBinOp (..),
    FPUnOp (..),
    FPTestOp (..),
    FPConvert (..),
    CmpOp (..),
    Assert (..),
    BoolBinOp (..),
    BoolUnOp (..),
  )
where

import Data.Data (Data)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data IntBinOp
  = Add
  | Mul
  | Sub
  | Quot
  | Rem
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable IntBinOp

data IntUnOp
  = Negate
  | Abs
  | Signum
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable IntUnOp

data IntTestOp
  = TestBit
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable IntTestOp

data IntSetBit
  = SetBit
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable IntSetBit

data FPBinOp
  = FPAdd
  | FPMul
  | FPSub
  | FPDiv
  | FPPow
  | FPAtan2
  | FPFmod
  | FPMax
  | FPMin
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable FPBinOp

data FPUnOp
  = FPNegate
  | FPAbs
  | FPSqrt
  | FPExp
  | FPLog
  | FPSin
  | FPCos
  | FPTan
  | FPASin
  | FPACos
  | FPATan
  | FPSinh
  | FPCosh
  | FPTanh
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable FPUnOp

data FPTestOp
  = FPIsNaN
  | FPIsInfinite
  | FPSignBit
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable FPTestOp

data FPConvert
  = FPConvert
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable FPConvert

data CmpOp
  = CmpEq
  | CmpNeq
  | CmpGT
  | CmpLT
  | CmpGE
  | CmpLE
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable CmpOp

data Round
  = RoundToIntegral
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable Round

data BoolBinOp
  = BAnd
  | BOr
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable BoolBinOp

data BoolUnOp
  = BNot
  deriving (Eq, Ord, Show, Read, Data, Generic)

instance Hashable BoolUnOp

newtype Assert = Assert {assertMessage :: String}
  deriving newtype (Show, Read, Eq, Ord, Hashable)
  deriving stock (Generic, Data)
