{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Equality by calling == on each element (except for NaN).
-- This is used to compare that two structs are bitwise equal
-- (not including padding which is undefined).
-- It is used for testing that a haskell function matches a C function, either
-- for testing purposes. For this use case we must consider floats/doubles equal if both elements
-- are NaN.
module Categorifier.C.CTypes.Codegen.IsEqual
  ( toIsEqualModule,

    -- * for test generation
    toIsEqualFunctionName,
    getMember,
    getMemberType,
  )
where

import Categorifier.C.CTypes.Codegen.Helpers
  ( CStructOrUnion (..),
    CxxStructOrUnion (..),
  )
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
import Categorifier.C.CTypes.DSL.CxxAst
  ( CExpr (..),
    CFunction (..),
    CTypeWithBackdoor (..),
    CxxModule (..),
    CxxTarget (..),
    Identifier (..),
    Include (..),
    Param (..),
    ParamType (..),
    StdFunc (..),
    SystemLib (Math),
    TapeElement (..),
    false,
    natExpr,
    true,
    (!),
    (#!),
  )
import Categorifier.C.CTypes.DSL.FunctionWriter
  ( FunWriter,
    Func2,
    caseUnion,
    cexprFunctionFromParams,
    comment,
    cxxLambda,
    force_,
    if_,
    loopWithType,
    ret,
    unsafeAsIdentifier,
    unsafeNewNamed,
    zipMap,
    (=:),
  )
import qualified Categorifier.C.CTypes.Render as R
import Categorifier.C.CTypes.Types
import Control.Monad (zipWithM_)
import Data.Foldable (for_, traverse_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import PyF (fmt)

baseName :: FunctionBaseName
baseName = FunctionBaseName "IsEqual"

-- for test generation
toIsEqualFunctionName :: CxxType Proxy -> T.Text
toIsEqualFunctionName = unIdentifier . toSimpleCxxFunctionName baseName . SftCxxType

toIsEqualModule ::
  [CEnum Proxy] -> [CStructOrUnion] -> [CxxStructOrUnion] -> [CxxType Proxy] -> CxxModule
toIsEqualModule cenums cstructsOrUnions cxxStructsOrUnions topLevelTypes =
  CxxModule
    { moduleIncludes = [IncludeSystemLib Math, IncludeModule CTypes, IncludeModule CxxTypes],
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

spec :: SimpleModuleSpec
spec =
  SimpleModuleSpec
    { smFunctionBaseName = baseName,
      smReturnType = Just $ ParamCxxType (CxxTypeCType (CTypePrim (PrimBool Proxy))),
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
funcIsEqual :: Func2
funcIsEqual = cexprFunctionFromParams (toFunctionParams undefined undefined)

toFunctionParams :: Bool -> ParamType -> [Param]
toFunctionParams unused t = [toParam "x", toParam "y"]
  where
    toParam name = Param {pType = t, pId = Identifier name, pUnused = unused, pMutable = False}

arrayOfElemFunctionBody :: CNat -> a -> CFunction -> [TapeElement]
arrayOfElemFunctionBody cnat _elemType elemFun = funcIsEqual $ \x y ->
  -- special behavior for 0-length arrays
  if cnatValue cnat == 0
    then ret true
    else do
      loopWithType (CTypeBackdoor "int") (natExpr cnat) $
        \k -> if_ (Not (elemFun #! [x ! k, y ! k])) $ ret false
      ret true

toCConFunctionBody :: CCon' -> [TapeElement]
toCConFunctionBody (CNullaryCon' _) = funcIsEqual $ \(_ :: CExpr) (_ :: CExpr) -> do
  comment "Nullary struct has no fields."
  ret true
toCConFunctionBody (CBitfieldCon' cbitfield _ _) = toBitfieldFunctionBody cbitfield
toCConFunctionBody (CNormalCon' _ _ fields) = funcIsEqual $
  \x y -> toNormalConBody x y (fmap f fields)
  where
    f :: (RfName, CType Proxy, CFunction) -> (RfName, CxxType Proxy, CFunction)
    f (rfName, ctype, fieldFun) = (rfName, CxxTypeCType ctype, fieldFun)

toCxxConFunctionBody :: CxxCon' -> [TapeElement]
toCxxConFunctionBody (CxxNormalCon' _ fields) = funcIsEqual $ \x y -> toNormalConBody x y fields

toNormalConBody :: CExpr -> CExpr -> NE.NonEmpty (RfName, CxxType Proxy, CFunction) -> FunWriter ()
toNormalConBody x y fields = do
  -- give all fields a chance to return false, then return true
  traverse_ (toNormalFieldBody x y) (NE.toList fields)
  ret true

toNormalFieldBody :: CExpr -> CExpr -> (RfName, CxxType Proxy, CFunction) -> FunWriter ()
toNormalFieldBody x y (rfName, _fieldType, fieldIsEqualFun) =
  if_ (Not (fieldIsEqualFun #! [x :. rfName, y :. rfName])) $ ret false

-- NOTE: Padding bits in the bitfield are not guranteed to be equal!
toBitfieldFunctionBody :: CBitfield Proxy -> [TapeElement]
toBitfieldFunctionBody (CBitfield _ fields _) =
  funcIsEqual $ \x y -> do
    for_ (NE.toList fields) $ \fieldName ->
      if_ (x :. fieldName :!= y :. fieldName) $
        ret (LiteralBool False)
    ret (LiteralBool True)

getMember :: CxxUnionCon Proxy -> RfName
getMember cxxUnionCon = makeRfName [fmt|get<{R.renderCxxUnionConType cxxUnionCon}>|]

getMemberType :: CxxUnionCon Proxy -> CTypeWithBackdoor
getMemberType = CTypeBackdoor . R.renderCxxUnionConType

toUnionFunctionBody :: CUnion Proxy -> M.Map (CCon Proxy) CFunction -> [TapeElement]
toUnionFunctionBody cunion conFunMap = funcIsEqual $ \x y -> do
  let tagMember = makeRfName $ R.renderCUnionTagMember cunion
  if_ (x :. tagMember :!= (y :. tagMember)) $ ret false
  caseUnion
    cunion
    x
    conFunMap
    ( \conIsEqualFun memberName _conName ->
        if_ (Not (conIsEqualFun #! [x :. memberName, y :. memberName])) $ ret false
    )
    . force_
    $ StdAssert
      #! [false]
  ret true

toCxxUnionFunctionBody :: CxxUnion Proxy -> M.Map (CxxOrCCon Proxy) CFunction -> [TapeElement]
toCxxUnionFunctionBody cxxUnion conFunMap = funcIsEqual $ \x y -> do
  let x_payload_which = (x :. #payload :. #which) #! []
      y_payload_which = (y :. #payload :. #which) #! []
  if_ (x_payload_which :!= y_payload_which) $ ret false
  if_ (x_payload_which :< LiteralInt 0) $ ret false
  if_ (x_payload_which :> LiteralInt (fromIntegral $ length (cxxuCons cxxUnion))) $ ret false
  retVal <- unsafeNewNamed "ret" (CTypePrim' (PrimBool Proxy)) false
  let unionConCase :: CxxOrCCon Proxy -> CExpr
      unionConCase ccon =
        let conType = CxxUnionCon cxxUnion ccon
         in case M.lookup ccon conFunMap of
              Nothing -> error $ "missing CCon " <> show ccon
              Just conIsEqualFun ->
                cxxLambda [unsafeAsIdentifier retVal, unsafeAsIdentifier y] [conType] $ \xc -> do
                  yc <-
                    unsafeNewNamed
                      "yc"
                      (getMemberType conType)
                      (y :. #payload :. getMember conType #! [])
                  retVal =: (conIsEqualFun #! [xc, yc])
  -- TODO(greg): this is missing part of the body for NormalCons
  let cases = fmap unionConCase (V.toList $ cxxuCons cxxUnion)
  force_ $ x :. #payload :. #match #! cases
  ret retVal

toEnumFunctionBody :: CEnum Proxy -> [TapeElement]
toEnumFunctionBody cenum = funcIsEqual $ \x y -> do
  comment [fmt|{R.renderCEnumType cenum}|]
  ret (x :== y)

toCxxPrimFunctionBody :: CxxPrim Proxy -> [TapeElement]
toCxxPrimFunctionBody proxy = funcIsEqual $ \x y -> case proxy of
  PrimString _ -> ret (x :== y)
  PrimByteString _ -> do
    comment "TODO(greg): i don't think this works"
    ret (x :== y)

floatCompareBody :: [TapeElement]
floatCompareBody = funcIsEqual $ \x y -> do
  if_ ((StdIsNaN #! [x]) :&& (StdIsNaN #! [y])) $ ret true
  ret $ x :== y

compareWithEqualsOperator :: [TapeElement]
compareWithEqualsOperator = funcIsEqual $ \x y -> ret (x :== y)

toPrimFunctionBody :: Prim Proxy -> [TapeElement]
toPrimFunctionBody (PrimDouble _) = floatCompareBody
toPrimFunctionBody (PrimFloat _) = floatCompareBody
toPrimFunctionBody (PrimBool _) = compareWithEqualsOperator
toPrimFunctionBody (PrimInt64 _) = compareWithEqualsOperator
toPrimFunctionBody (PrimInt32 _) = compareWithEqualsOperator
toPrimFunctionBody (PrimInt16 _) = compareWithEqualsOperator
toPrimFunctionBody (PrimInt8 _) = compareWithEqualsOperator
toPrimFunctionBody (PrimWord64 _) = compareWithEqualsOperator
toPrimFunctionBody (PrimWord32 _) = compareWithEqualsOperator
toPrimFunctionBody (PrimWord16 _) = compareWithEqualsOperator
toPrimFunctionBody (PrimWord8 _) = compareWithEqualsOperator

toVectorFunctionBody :: CxxType Proxy -> CFunction -> [TapeElement]
toVectorFunctionBody _ elemIsEqualFun = funcIsEqual $ \x y -> do
  if_ ((x :. #size #! []) :!= (y :. #size #! [])) $ ret false
  loopWithType (CTypeBackdoor "size_t") (x :. #size #! []) $
    \k -> if_ (Not (elemIsEqualFun #! [x ! k, y ! k])) $ ret false
  ret true

toMapFunctionBody :: (CxxType Proxy, CFunction) -> (CxxType Proxy, CFunction) -> [TapeElement]
toMapFunctionBody (keyType, keyIsEqualFun) (valueType, valueIsEqualFun) = funcIsEqual $ \x y -> do
  if_ (x :. #size #! [] :!= (y :. #size #! [])) $ ret false
  zipMap keyType valueType x y $ \(keyX, keyY) (valX, valY) -> do
    if_ (Not (keyIsEqualFun #! [keyX, keyY])) $ ret false
    if_ (Not (valueIsEqualFun #! [valX, valY])) $ ret false
  ret true

toTupleFunctionBody :: [CFunction] -> [TapeElement]
toTupleFunctionBody funcs = funcIsEqual $ \x y -> case funcs of
  [] -> error "tuple with 0 elements"
  [_] -> error "tuple with 1 elements"
  [f0, f1] -> do
    if_ (Not (f0 #! [x :. #first, y :. #first])) $ ret false
    if_ (Not (f1 #! [x :. #second, y :. #second])) $ ret false
    ret true
  _ -> do
    let writeCompare k f = if_ (Not (f #! [StdGet k #! [x], StdGet k #! [y]])) $ ret false
    zipWithM_ writeCompare [0 ..] funcs
    ret true
