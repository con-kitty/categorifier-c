{-# LANGUAGE OverloadedStrings #-}

module Categorifier.C.CTypes.Codegen.NonFinite
  ( containsFloatingPointArgs,
    makeNonFiniteParams,
    nonFiniteStatsParam,
    NonFiniteField (..),
    whenNonFiniteParamExpr,
    whenNonFiniteParamIdent,
    nonFiniteStatsParamExpr,
    needsNonFiniteParams,
    fpPrimFunctionBody,
    safeNonFiniteAccess,
  )
where

import Categorifier.C.CTypes.Codegen.Arrays.Types
  ( ArraysFunType (..),
    ToOrFromArrays (..),
    passByValue,
    toArrayIdentifier,
  )
import Categorifier.C.CTypes.DSL.CxxAst
  ( CExpr (..),
    ExternalType (..),
    Identifier (..),
    Param (..),
    ParamType (..),
    dereference,
    (!),
  )
import Categorifier.C.CTypes.DSL.FunctionWriter
  ( FunWriter,
    comment,
    ifElse_,
    if_,
    (=:),
  )
import Categorifier.C.CTypes.Types
  ( CType,
    RfName,
    makeRfName,
  )
import Categorifier.C.Prim
  ( ArrayCount (..),
    Arrays (arrayDouble, arrayFloat),
    PrimType,
  )
import Data.Proxy (Proxy (..))
import Data.Text (Text)

containsFloatingPointArgs :: Arrays ArrayCount -> Bool
containsFloatingPointArgs counts =
  arrayDouble counts > mempty || arrayFloat counts > mempty

isFromArrays :: ToOrFromArrays -> Bool
isFromArrays = (== FromArrays')

needsNonFiniteParams :: Arrays ArrayCount -> ToOrFromArrays -> Bool
needsNonFiniteParams counts toOrFrom =
  containsFloatingPointArgs counts && isFromArrays toOrFrom

makeNonFiniteParams ::
  ToOrFromArrays ->
  Arrays ArrayCount ->
  NonFiniteField ->
  CType Proxy ->
  Maybe (CExpr, CExpr)
makeNonFiniteParams toOrFrom counts fieldName elemType =
  if needsNonFiniteParams counts toOrFrom
    then case fieldName of
      NoField -> pure (nullPtr, nullPtr)
      StructMember rfName ->
        pure
          ( safeNonFiniteAccess
              . maybeAddress
              $ whenNonFiniteParamExpr :-> rfName,
            nonFiniteStatsParamExpr
          )
      DistinctArgument paramIdent -> pure (Ident paramIdent, nonFiniteStatsParamExpr)
    else Nothing
  where
    maybeAddress
      | passByValue toOrFrom (AfCType elemType) = id
      | otherwise = TakeAddress

nonFiniteStatsParam :: Param
nonFiniteStatsParam =
  Param
    { pId = nonFiniteStatsParamIdent,
      pUnused = False,
      pMutable = True,
      pType = ParamExternalType $ ExternalType "NonFiniteStats"
    }

data NonFiniteField
  = -- | No non-finite handling for this parameter
    NoField
  | -- | Pull out and dereference a struct member from the non-finite parameter
    -- to the value parameter
    StructMember RfName
  | -- | The given `Identifier` is an argument to the outer function; no struct manipulation is
    -- necessary.
    DistinctArgument Identifier
  deriving (Show)

whenNonFiniteParamName :: Text
whenNonFiniteParamName = "whenNonFinite"

whenNonFiniteParamIdent :: Identifier
whenNonFiniteParamIdent = Identifier whenNonFiniteParamName

whenNonFiniteParamExpr :: CExpr
whenNonFiniteParamExpr = Ident whenNonFiniteParamIdent

nonFiniteStatsParamName :: Text
nonFiniteStatsParamName = "nonFiniteStats"

nonFiniteStatsParamIdent :: Identifier
nonFiniteStatsParamIdent = Identifier nonFiniteStatsParamName

nonFiniteStatsParamExpr :: CExpr
nonFiniteStatsParamExpr = Ident nonFiniteStatsParamIdent

fpPrimFunctionBody :: PrimType -> ToOrFromArrays -> CExpr -> Text -> FunWriter ()
fpPrimFunctionBody cprim toOrFrom valueParamExpr typeStr =
  ifElse_ (nullPtr :== whenNonFiniteParamExpr) body $
    ifElse_
      (IsNan primValue)
      nanFunctionBody
      $ ifElse_
        (IsInf primValue)
        infFunctionBody
        body
  where
    nanFunctionBody = nfFunctionBody "NaN"
    infFunctionBody = nfFunctionBody "Inf"
    fallbackValue = whenNonFiniteParamExpr ! LiteralInt 0
    primValue = arrayIdent ! LiteralInt 0
    arrayIdent = Ident $ toArrayIdentifier toOrFrom cprim
    body = comment "Take unmodified input" *> (dereference valueParamExpr =: primValue)
    nfFunctionBody :: Text -> FunWriter ()
    nfFunctionBody nfStr = do
      comment $ "Input was " <> nfStr <> "; use fallback value."
      dereference valueParamExpr =: fallbackValue
      if_ (nullPtr :!= nonFiniteStatsParamExpr) $
        nfsStatsElement =: nfsStatsElement :+ LiteralInt 1
      where
        nfsStatsElement = nonFiniteStatsParamExpr :-> makeRfName ("nfs" <> typeStr <> nfStr)

nullPtr :: CExpr
nullPtr = Ident $ Identifier "NULL"

safeNonFiniteAccess :: CExpr -> CExpr
safeNonFiniteAccess = whenNotNull whenNonFiniteParamExpr
  where
    whenNotNull val = IfThenElse (isNull val) nullPtr
    isNull = (nullPtr :==)
