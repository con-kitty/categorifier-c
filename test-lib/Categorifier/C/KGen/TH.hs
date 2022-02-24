{-# LANGUAGE TemplateHaskell #-}

-- | This is the TH which generates boilerplate instances for the
-- typeclass hierarchy.
module Categorifier.C.KGen.TH
  ( declareOrdIEEE,
    declareKDivisibleSigned,
  )
where

import Categorifier.C.KTypes.KDivisible (KDivisible (..))
import Categorifier.C.KTypes.TotalOrder (KOrd (..))
import qualified Data.SBV as SBV
import qualified Language.Haskell.TH as TH

-- pass the name to avoid
declareOrdIEEE :: TH.Name -> TH.Name -> TH.Name -> TH.DecsQ
declareOrdIEEE overName tyConName primName =
  [d|
    instance KOrd $(TH.conT tyConName) ($(TH.conT tyConName) $(TH.conT primName)) where
      (.<) = $(TH.varE overName) (SBV..<)
      (.<=) = $(TH.varE overName) (SBV..<=)
      (.>) = $(TH.varE overName) (SBV..>)
      (.>=) = $(TH.varE overName) (SBV..>=)
      kMin = compareZerosGuard SBV.fpMin
      kMax = compareZerosGuard SBV.fpMax
    |]

declareKDivisibleSigned :: TH.Name -> TH.Name -> TH.Name -> TH.Name -> TH.Name -> TH.DecsQ
declareKDivisibleSigned overName kgen primName divFun modFun =
  [d|
    instance KDivisible ($(TH.conT kgen) $(TH.conT primName)) where
      kDiv = $(TH.varE overName) $(TH.varE divFun)
      kMod = $(TH.varE overName) $(TH.varE modFun)
    |]
