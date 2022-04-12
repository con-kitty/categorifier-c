{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Functions for printing out structs.
module Categorifier.C.CTypes.Codegen.ToString
  ( toToStringModule,
    printBytes,
  )
where

import Categorifier.C.CTypes.Codegen.Helpers (CStructOrUnion (..), CxxStructOrUnion (..))
import Categorifier.C.CTypes.Codegen.SimpleModule
  ( CCon' (..),
    CxxCon' (..),
    FunctionBaseName (..),
    SimpleCxxModuleSpec (..),
    SimpleModuleSpec (..),
    toSimpleModuleCxxFunctions,
  )
import Categorifier.C.CTypes.DSL.CxxAst
  ( CExpr (..),
    CFunction (..),
    CTypeWithBackdoor (..),
    CxxModule (..),
    CxxTarget (..),
    ExternalType (..),
    Identifier (..),
    Include (..),
    Param (..),
    ParamType (..),
    StdFunc (..),
    TapeElement (..),
    dereference,
    false,
    natExpr,
    staticCast,
    (!),
    (#!),
    pattern StdIosBaseFmtFlags,
  )
import Categorifier.C.CTypes.DSL.FunctionWriter
  ( FunWriter,
    Func3,
    caseEnum,
    caseUnion,
    cexprFunctionFromParams,
    comment,
    cxxForLoopWithTypeNamed,
    cxxLambda,
    force_,
    loopWithType,
    retVoid,
    traverseStdMap,
    unsafeAsIdentifier,
    unsafeNewNamed,
  )
import Categorifier.C.CTypes.Types
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import PyF (fmt)

baseName :: FunctionBaseName
baseName = FunctionBaseName "ToString"

toToStringModule :: [CEnum Proxy] -> [CStructOrUnion] -> [CxxStructOrUnion] -> CxxModule
toToStringModule cenums cstructsOrUnions cxxStructsOrUnions =
  CxxModule
    { moduleIncludes = includes,
      moduleDefines = [],
      moduleTypedefs = [],
      moduleUsingDecls = [],
      moduleFunctions =
        toSimpleModuleCxxFunctions
          spec
          cxxSpec
          cenums
          cstructsOrUnions
          cxxStructsOrUnions
          [],
      moduleTypeLevelFunctions = []
    }
  where
    includes =
      [ IncludeSystemFile "iomanip",
        IncludeSystemFile "iostream",
        IncludeModule CTypes,
        IncludeModule CxxTypes
      ]

spec :: SimpleModuleSpec
spec =
  SimpleModuleSpec
    { smFunctionBaseName = baseName,
      smReturnType = Nothing,
      smFunctionParams = toFunctionParams,
      smArrayOfElemFunctionBody = arrayOfElemFunctionBody,
      smCConFunctionBody = toCConFunctionBody,
      smPrimFunctionBody = toPrimFunctionBody,
      smEnumFunctionBody = toEnumFunctionBody,
      smUnionFunctionBody = toUnionFunctionBody
    }

cxxSpec :: SimpleCxxModuleSpec
cxxSpec =
  SimpleCxxModuleSpec
    { smCxxConFunctionBody = toCxxConFunctionBody,
      smCxxArrayOfElemFunctionBody = arrayOfElemFunctionBody,
      smTreatListsAsVectors = True,
      smTupleFunctionBody = toTupleFunctionBody,
      smMapFunctionBody = toMapFunctionBody,
      smVectorFunctionBody = toVectorFunctionBody,
      smCxxPrimFunctionBody = toCxxPrimFunctionBody,
      smCxxUnionFunctionBody = toCxxUnionFunctionBody
    }

-- TODO: the undefined fields won't be used by 'cexprFunctionFromParams' so we are good
funcToString :: Func3
funcToString = cexprFunctionFromParams (toFunctionParams undefined undefined)

toFunctionParams :: Bool -> ParamType -> [Param]
toFunctionParams unused t =
  [ toParam "output_stream" ostreamType True,
    toParam "value" t False,
    toParam "prefix" prefixType False
  ]
  where
    ostreamType = ParamExternalType (ExternalType "std::ostream")
    prefixType = ParamCxxType (CxxTypePrim (PrimString Proxy))
    toParam name type' mutable =
      Param {pType = type', pId = Identifier name, pUnused = unused, pMutable = mutable}

arrayOfElemFunctionBody :: CNat -> a -> CFunction -> [TapeElement]
arrayOfElemFunctionBody cnat _elemType elemFun = funcToString $ \stream array prefix ->
  -- special behavior for 0-length arrays
  if cnatValue cnat == 0
    then retVoid
    else loopWithType (CTypeBackdoor "int") (natExpr cnat) $
      \k ->
        force_ $ elemFun #! [stream, array ! k, prefix :++ "[" :++ (StdToString #! [k]) :++ "]"]

toCConFunctionBody :: CCon' -> [TapeElement]
toCConFunctionBody (CNullaryCon' _) = [Comment' "Nullary struct has no fields."]
toCConFunctionBody (CBitfieldCon' (CBitfield _ fields _) _ _) = toBitfieldFunctionBody fields
toCConFunctionBody (CNormalCon' _ _ fields) = toNormalConBody (fmap f fields)
  where
    f :: (RfName, CType Proxy, CFunction) -> (RfName, CxxType Proxy, CFunction)
    f (rfName, ctype, fieldFun) = (rfName, CxxTypeCType ctype, fieldFun)

toCxxConFunctionBody :: CxxCon' -> [TapeElement]
toCxxConFunctionBody (CxxNormalCon' _ fields) = toNormalConBody fields

toNormalConBody :: NE.NonEmpty (RfName, CxxType Proxy, CFunction) -> [TapeElement]
toNormalConBody = concatMap toNormalFieldBody . NE.toList

toNormalFieldBody :: (RfName, CxxType Proxy, CFunction) -> [TapeElement]
toNormalFieldBody (rfName, _fieldType, fieldToStringFun) = funcToString $ \stream value prefix ->
  force_ $ fieldToStringFun #! [stream, value :. rfName, prefix :++ [fmt|.{fieldName}|]]
  where
    fieldName = sanitizedRfName rfName

toBitfieldFunctionBody :: NE.NonEmpty RfName -> [TapeElement]
toBitfieldFunctionBody fields = funcToString $ \stream value prefix ->
  for_ (NE.toList fields) $ \fieldName ->
    force_ $
      dereference stream
        :<< prefix
        :<< [fmt|.{sanitizedRfName fieldName} = |]
        :<< IfThenElse (value :. fieldName) (LiteralChar '1') (LiteralChar '0')
        :<< StdEndl

toUnionFunctionBody :: CUnion Proxy -> M.Map (CCon Proxy) CFunction -> [TapeElement]
toUnionFunctionBody cunion conFunMap = funcToString $ \stream value prefix ->
  caseUnion
    cunion
    value
    conFunMap
    ( \conToStringFun memberName _conName ->
        force_ $
          conToStringFun
            #! [stream, value :. memberName, prefix :++ [fmt|.{sanitizedRfName memberName}|]]
    )
    (force_ $ StdAssert #! [false])

toCxxUnionFunctionBody :: CxxUnion Proxy -> M.Map (CxxOrCCon Proxy) CFunction -> [TapeElement]
toCxxUnionFunctionBody cxxUnion conFunMap = funcToString $ \stream value prefix -> do
  let unionConCase :: CxxOrCCon Proxy -> CExpr
      unionConCase ccon =
        let conType = CxxUnionCon cxxUnion ccon
            DcName conName = cxxOrCConName ccon
         in case M.lookup ccon conFunMap of
              Nothing -> error $ "missing CCon " <> show ccon
              Just conToStringFun ->
                cxxLambda [unsafeAsIdentifier stream, unsafeAsIdentifier prefix] [conType] $
                  \xc -> force_ $ conToStringFun #! [stream, xc, prefix :++ [fmt|::{conName}|]]
  -- TODO(greg): this is missing part of the body for NormalCons
  let cases = fmap unionConCase (V.toList $ cxxuCons cxxUnion)
  force_ $ value :. #payload :. #match #! cases

toEnumFunctionBody :: CEnum Proxy -> [TapeElement]
toEnumFunctionBody cenum = funcToString $ \stream value prefix ->
  caseEnum
    cenum
    value
    (\conString -> force_ $ dereference stream :<< prefix :<< [fmt| = {conString}|] :<< StdEndl)
    (force_ $ dereference stream :<< prefix :<< " = <<UNDEFINED VALUE>>" :<< StdEndl)

printBytes :: CExpr -> CExpr -> FunWriter ()
printBytes stream value = do
  comment [fmt|Print bytes of {unIdentifier (unsafeAsIdentifier value)}.|]
  force_ $ dereference stream :<< StdHex
  cxxForLoopWithTypeNamed "byte" (CTypePrim' (PrimWord8 Proxy)) value $ \byte ->
    force_ $
      dereference stream
        :<< (StdSetw #! [LiteralInt 2])
        :<< (StdSetfill #! [LiteralChar '0'])
        :<< staticCast (CTypePrim' (PrimInt32 Proxy)) byte
        :<< " "

toCxxPrimFunctionBody :: CxxPrim Proxy -> [TapeElement]
toCxxPrimFunctionBody (PrimString _) = funcToString $
  \stream value prefix -> force_ $ dereference stream :<< prefix :<< " = " :<< value :<< StdEndl
toCxxPrimFunctionBody (PrimByteString _) = funcToString $ \stream value prefix -> do
  comment "Prefix."
  force_ $ dereference stream :<< prefix :<< " = "
  -- TODO: add an helper function to preserve format flags
  comment "Save format flags."
  original_format_flags <-
    unsafeNewNamed
      "original_format_flags"
      StdIosBaseFmtFlags
      (stream :-> #flags #! [])
  printBytes stream value
  comment "Restore format flags."
  force_ $ (stream :-> #flags) #! [original_format_flags]
  comment "Don't forget the newline."
  force_ $ dereference stream :<< StdEndl

simpleOutput :: [TapeElement]
simpleOutput = funcToString $
  \stream value prefix -> force_ $ dereference stream :<< prefix :<< " = " :<< value :<< StdEndl

simpleOutputWithCast :: CType Proxy -> [TapeElement]
simpleOutputWithCast t = funcToString $ \stream value prefix ->
  force_ $ dereference stream :<< prefix :<< " = " :<< staticCast t value :<< StdEndl

-- TODO(greg/guillaume): code duplication with "PrintDifferences".
toPrimFunctionBody :: Prim Proxy -> [TapeElement]
toPrimFunctionBody (PrimBool _) = simpleOutput
toPrimFunctionBody (PrimInt8 _) = simpleOutputWithCast (CTypePrim (PrimInt32 Proxy))
toPrimFunctionBody (PrimInt16 _) = simpleOutput
toPrimFunctionBody (PrimInt32 _) = simpleOutput
toPrimFunctionBody (PrimInt64 _) = simpleOutput
toPrimFunctionBody (PrimWord8 _) = simpleOutputWithCast (CTypePrim (PrimWord32 Proxy))
toPrimFunctionBody (PrimWord16 _) = simpleOutput
toPrimFunctionBody (PrimWord32 _) = simpleOutput
toPrimFunctionBody (PrimWord64 _) = simpleOutput
toPrimFunctionBody (PrimFloat _) = simpleOutput
toPrimFunctionBody (PrimDouble _) = simpleOutput

toVectorFunctionBody :: CxxType Proxy -> CFunction -> [TapeElement]
toVectorFunctionBody _ elemToStringFun = funcToString $ \stream value prefix ->
  loopWithType (CTypeBackdoor "size_t") (value :. #size #! []) $ \k ->
    force_ $ elemToStringFun #! [stream, value ! k, prefix :++ "[" :++ (StdToString #! [k]) :++ "]"]

toMapFunctionBody :: (CxxType Proxy, CFunction) -> (CxxType Proxy, CFunction) -> [TapeElement]
toMapFunctionBody (keyType, _keyFun) (valueType, valueFun) =
  funcToString $ \stream value prefix -> traverseStdMap keyType valueType value $
    \key val -> force_ $ valueFun #! [stream, val, prefix :++ "[" :++ key :++ "]"]

toTupleFunctionBody :: [CFunction] -> [TapeElement]
toTupleFunctionBody funcs = funcToString $ \stream tuple prefix -> for_ (zip funcs [0 ..]) $
  \(f, idx) -> force_ $ f #! [stream, StdGet idx #! [tuple], prefix :++ [fmt|<{idx}>|]]
