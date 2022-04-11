{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Functions for printing out differences between two structs.
module Categorifier.C.CTypes.Codegen.PrintDifferences
  ( toPrintDifferencesModule,

    -- * for test generation
    toPrintDifferencesFunctionName,
  )
where

import Categorifier.C.CTypes.Codegen.Helpers
  ( CStructOrUnion (..),
    CxxStructOrUnion (..),
  )
import Categorifier.C.CTypes.Codegen.IsEqual (getMember, getMemberType)
import Categorifier.C.CTypes.Codegen.SimpleModule
  ( CCon' (..),
    CxxCon' (..),
    FunctionBaseName (..),
    SimpleCxxFunType (..),
    SimpleCxxModuleSpec (..),
    SimpleModuleSpec (..),
    toSimpleCxxFunctionName,
    toSimpleModuleCxxFunctions,
  )
import Categorifier.C.CTypes.Codegen.ToString (printBytes)
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
  ( Func4,
    caseUnion,
    cexprFunctionFromParams,
    comment,
    cxxLambda,
    force_,
    if_,
    loopWithType,
    retVoid,
    unsafeAsIdentifier,
    unsafeNewNamed,
    zipMapAll,
  )
import qualified Categorifier.C.CTypes.Render as R
import Categorifier.C.CTypes.Types
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import PyF (fmt)

baseName :: FunctionBaseName
baseName = FunctionBaseName "PrintDifferences"

-- for test generation
toPrintDifferencesFunctionName :: CxxType Proxy -> T.Text
toPrintDifferencesFunctionName = unIdentifier . toSimpleCxxFunctionName baseName . SftCxxType

toPrintDifferencesModule ::
  [CEnum Proxy] -> [CStructOrUnion] -> [CxxStructOrUnion] -> [CxxType Proxy] -> CxxModule
toPrintDifferencesModule cenums cstructsOrUnions cxxStructsOrUnions topLevelTypes =
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
          topLevelTypes,
      moduleTypeLevelFunctions = []
    }
  where
    includes =
      [ IncludeSystemFile "iomanip",
        IncludeSystemFile "iostream",
        IncludeModule CTypes,
        IncludeModule CxxTypes,
        IncludeModule EnumUtils
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
funcPrintDifferences :: Func4
funcPrintDifferences = cexprFunctionFromParams (toFunctionParams undefined undefined)

toFunctionParams :: Bool -> ParamType -> [Param]
toFunctionParams unused t =
  [ toParam "output_stream" ostreamType True,
    toParam "x" t False,
    toParam "y" t False,
    toParam "prefix" prefixType False
  ]
  where
    ostreamType = ParamExternalType (ExternalType "std::ostream")
    prefixType = ParamCxxType (CxxTypePrim (PrimString Proxy))
    toParam name type' mutable =
      Param {pType = type', pId = Identifier name, pUnused = unused, pMutable = mutable}

arrayOfElemFunctionBody :: CNat -> a -> CFunction -> [TapeElement]
arrayOfElemFunctionBody cnat _elemType elemFun = funcPrintDifferences $ \stream x y prefix ->
  -- special behavior for 0-length arrays
  if cnatValue cnat == 0
    then retVoid
    else loopWithType (CTypeBackdoor "int") (natExpr cnat) $
      \k ->
        force_ $
          elemFun
            #! [stream, x ! k, y ! k, prefix :++ "[" :++ (StdToString #! [k] :++ "]")]

toCConFunctionBody :: CCon' -> [TapeElement]
toCConFunctionBody (CNullaryCon' _) =
  funcPrintDifferences $ \_stream _x _y _prefix -> comment "Nullary struct has no fields."
toCConFunctionBody (CBitfieldCon' (CBitfield _ fields _) _ _) = toBitfieldFunctionBody fields
toCConFunctionBody (CNormalCon' _ _ fields) = toNormalConBody (fmap f fields)
  where
    f :: (RfName, CType Proxy, CFunction) -> (RfName, CxxType Proxy, CFunction)
    f (rfName, ctype, fieldFun) = (rfName, CxxTypeCType ctype, fieldFun)

toCxxConFunctionBody :: CxxCon' -> [TapeElement]
toCxxConFunctionBody (CxxNormalCon' _ fields) = toNormalConBody fields

toNormalConBody :: NE.NonEmpty (RfName, CxxType Proxy, CFunction) -> [TapeElement]
toNormalConBody fields = funcPrintDifferences $ \stream x y prefix ->
  for_ (NE.toList fields) $ \(fieldName, _fieldType, fieldPrintDifferencesFun) ->
    force_ $
      fieldPrintDifferencesFun
        #! [stream, x :. fieldName, y :. fieldName, prefix :++ [fmt|.{sanitizedRfName fieldName}|]]

-- This function assumes the equality function is named IsEqual{structName}.
-- This is ugly but should be fine.
-- NOTE: Padding bits in the bitfield are not guranteed to be equal!
toBitfieldFunctionBody :: NE.NonEmpty RfName -> [TapeElement]
toBitfieldFunctionBody fields = funcPrintDifferences $ \stream x y prefix -> do
  comment "Show all the field differences."
  for_ (NE.toList fields) $ \fieldName -> if_ (x :. fieldName :!= y :. fieldName) $ do
    let valBit v = IfThenElse v (LiteralChar '1') (LiteralChar '0')
        valBitX = valBit (x :. fieldName)
        valBitY = valBit (y :. fieldName)
    force_ $
      dereference stream
        :<< prefix
        :<< [fmt|.{sanitizedRfName fieldName}: |]
        :<< valBitX
        :<< " != "
        :<< valBitY
        :<< StdEndl

toUnionFunctionBody :: CUnion Proxy -> M.Map (CCon Proxy) CFunction -> [TapeElement]
toUnionFunctionBody cunion conFunMap = funcPrintDifferences $ \stream x y prefix -> do
  let tagMember = makeRfName $ R.renderCUnionTagMember cunion
      unionToString :: CExpr
      unionToString = Ident (Identifier [fmt|EnumToString_{R.renderCUnionTagType cunion}|])
  if_ (x :. tagMember :!= (y :. tagMember)) $ do
    force_ $
      dereference stream
        :<< prefix
        :<< ": "
        :<< (unionToString #! [x :. tagMember])
        :<< " != "
        :<< (unionToString #! [y :. tagMember])
        :<< StdEndl
    retVoid
  caseUnion
    cunion
    x
    conFunMap
    ( \conFun memberName conName -> do
        let newPrefix = prefix :++ [fmt|.{conName}|]
        force_ $ conFun #! [stream, x :. memberName, y :. memberName, newPrefix]
    )
    (force_ $ StdAssert #! [false])

toCxxUnionFunctionBody :: CxxUnion Proxy -> M.Map (CxxOrCCon Proxy) CFunction -> [TapeElement]
toCxxUnionFunctionBody cxxUnion conFunMap = funcPrintDifferences $ \stream x y prefix -> do
  let xPayloadWhich = CallFunction (x :. #payload :. #which) []
      yPayloadWhich = CallFunction (y :. #payload :. #which) []
  if_ (xPayloadWhich :!= yPayloadWhich)
    . force_
    $ dereference stream
      :<< prefix
      :<< ": variant "
      :<< xPayloadWhich
      :<< " != "
      :<< yPayloadWhich
      :<< StdEndl
  if_ (xPayloadWhich :< LiteralInt 0) $ force_ $ StdAssert #! [false]
  if_ (xPayloadWhich :> LiteralInt (fromIntegral $ length (cxxuCons cxxUnion)))
    . force_
    $ StdAssert
      #! [false]
  let unionConCase :: CxxOrCCon Proxy -> CExpr
      unionConCase ccon =
        let conType = CxxUnionCon cxxUnion ccon
         in case M.lookup ccon conFunMap of
              Nothing -> error $ "missing CCon " <> show ccon
              Just conPrintDifferencesFun ->
                cxxLambda
                  [unsafeAsIdentifier stream, unsafeAsIdentifier prefix, unsafeAsIdentifier y]
                  [conType]
                  $ \xc -> do
                    let newPrefix =
                          prefix :++ [fmt|.{R.renderCxxUnionConType (CxxUnionCon cxxUnion ccon)}|]
                    yc <-
                      unsafeNewNamed
                        "yc"
                        (getMemberType conType)
                        (y :. #payload :. getMember conType #! [])
                    force_ $ conPrintDifferencesFun #! [stream, xc, yc, newPrefix]
      -- TODO(greg): this is missing part of the body for NormalCons
      cases = fmap unionConCase (V.toList $ cxxuCons cxxUnion)
  force_ $ x :. #payload :. #match #! cases

enumToString :: CEnum Proxy -> CExpr
enumToString cenum = Ident (Identifier [fmt|EnumToString_{R.renderCEnumType cenum}|])

toEnumFunctionBody :: CEnum Proxy -> [TapeElement]
toEnumFunctionBody cenum = funcPrintDifferences $ \stream x y prefix ->
  if_ (x :!= y)
    . force_
    $ dereference stream
      :<< prefix
      :<< ": "
      :<< (enumToString cenum #! [x])
      :<< " != "
      :<< (enumToString cenum #! [y])
      :<< StdEndl

toCxxPrimFunctionBody :: CxxPrim Proxy -> [TapeElement]
toCxxPrimFunctionBody (PrimString _) = funcPrintDifferences $ \stream x y prefix ->
  if_ (x :!= y) $ force_ $ dereference stream :<< prefix :<< ": " :<< x :<< " != " :<< y :<< StdEndl
toCxxPrimFunctionBody (PrimByteString _) = funcPrintDifferences $ \stream x y prefix -> do
  comment "Prefix."
  force_ $ dereference stream :<< prefix :<< ": "
  -- TODO: add an helper function to preserve format flags
  comment "Save format flags."
  original_format_flags <-
    unsafeNewNamed
      "original_format_flags"
      StdIosBaseFmtFlags
      (stream :-> #flags #! [])
  printBytes stream x
  comment "Restore format flags."
  force_ $ (stream :-> #flags) #! [original_format_flags]
  comment "print not equals"
  force_ $ dereference stream :<< " != "
  printBytes stream y
  comment "Restore format flags."
  force_ $ (stream :-> #flags) #! [original_format_flags]
  comment "Don't forget the newline."
  force_ $ dereference stream :<< StdEndl

simplePrintDifferences :: [TapeElement]
simplePrintDifferences = funcPrintDifferences $ \stream x y prefix ->
  if_ (x :!= y) $ force_ $ dereference stream :<< prefix :<< ": " :<< x :<< " != " :<< y :<< StdEndl

-- You have to cast 8 bit ints to something else or they will be printed out as ASCII characters.
simplePrintDifferencesWithCast :: CType Proxy -> [TapeElement]
simplePrintDifferencesWithCast t = funcPrintDifferences $ \stream x y prefix ->
  if_ (x :!= y)
    . force_
    $ dereference stream
      :<< prefix
      :<< ": "
      :<< staticCast t x
      :<< " != "
      :<< staticCast t y
      :<< StdEndl

-- TODO(greg/guillaume): code duplication with "ToString".
toPrimFunctionBody :: Prim Proxy -> [TapeElement]
toPrimFunctionBody (PrimDouble _) = simplePrintDifferences
toPrimFunctionBody (PrimFloat _) = simplePrintDifferences
toPrimFunctionBody (PrimBool _) = simplePrintDifferences
toPrimFunctionBody (PrimInt64 _) = simplePrintDifferences
toPrimFunctionBody (PrimInt32 _) = simplePrintDifferences
toPrimFunctionBody (PrimInt16 _) = simplePrintDifferences
toPrimFunctionBody (PrimInt8 _) = simplePrintDifferencesWithCast (CTypePrim (PrimInt32 Proxy))
toPrimFunctionBody (PrimWord64 _) = simplePrintDifferences
toPrimFunctionBody (PrimWord32 _) = simplePrintDifferences
toPrimFunctionBody (PrimWord16 _) = simplePrintDifferences
toPrimFunctionBody (PrimWord8 _) = simplePrintDifferencesWithCast (CTypePrim (PrimWord32 Proxy))

toVectorFunctionBody :: CxxType Proxy -> CFunction -> [TapeElement]
toVectorFunctionBody _ elemPrintDifferencesFun = funcPrintDifferences $ \stream x y prefix -> do
  let xSize = x :. #size #! []
      ySize = y :. #size #! []
  if_ (xSize :!= ySize) $ do
    force_ $
      dereference stream
        :<< prefix
        :<< ": vector size mismatch: "
        :<< xSize
        :<< " != "
        :<< ySize
        :<< StdEndl
    retVoid
  loopWithType (CTypeBackdoor "size_t") xSize $ \k ->
    force_ $
      elemPrintDifferencesFun
        #! [stream, x ! k, y ! k, prefix :++ "[" :++ (StdToString #! [k]) :++ "]"]

toMapFunctionBody :: (CxxType Proxy, CFunction) -> (CxxType Proxy, CFunction) -> [TapeElement]
toMapFunctionBody (keyType, _keyFun) (valueType, valueFun) =
  funcPrintDifferences $ \stream x y prefix -> do
    let xSize = x :. #size #! []
        ySize = y :. #size #! []
    if_ (xSize :!= ySize) $ do
      force_ $
        dereference stream
          :<< prefix
          :<< ": map size mismatch: "
          :<< xSize
          :<< " != "
          :<< ySize
          :<< StdEndl
      retVoid
    zipMapAll
      keyType
      valueType
      x
      y
      ( -- on both keys
        \key valA valB -> force_ $ valueFun #! [stream, valA, valB, prefix :++ "[" :++ key :++ "]"]
      )
      ( -- on key x
        \key _val ->
          force_ $ dereference stream :<< prefix :<< ": key " :<< key :<< " only in LHS" :<< StdEndl
      )
      ( -- on key y
        \key _val ->
          force_ $ dereference stream :<< prefix :<< ": key " :<< key :<< " only in RHS" :<< StdEndl
      )

toTupleFunctionBody :: [CFunction] -> [TapeElement]
toTupleFunctionBody funcs = funcPrintDifferences $ \stream x y prefix ->
  for_ (zip funcs [0 ..]) $ \(f, idx) ->
    force_ $ f #! [stream, StdGet idx #! [x], StdGet idx #! [y], prefix :++ [fmt|<{idx}>|]]
