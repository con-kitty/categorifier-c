{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

-- | Interface for recursively generating a set of simple functions, one for each type.
module Categorifier.C.CTypes.Codegen.SimpleModule
  ( SimpleModuleSpec (..),
    SimpleCxxModuleSpec (..),
    SimpleCFunType (..),
    SimpleCxxFunType (..),
    CxxCon' (..),
    CCon' (..),
    FunctionBaseName (..),
    toSimpleModuleCFunctions,
    toSimpleModuleCxxFunctions,

    -- * some backdoors
    toSimpleCFunctionName,
    toSimpleCxxFunctionName,
  )
where

import Categorifier.C.CTypes.Codegen.Helpers
  ( CStructOrUnion (..),
    CxxStructOrUnion (..),
  )
import Categorifier.C.CTypes.DSL.CxxAst
  ( CFunction (..),
    Comment (..),
    Identifier (..),
    Param (..),
    ParamType (..),
    TapeElement (..),
  )
import qualified Categorifier.C.CTypes.Render as R
import Categorifier.C.CTypes.Types
import Categorifier.C.Prim (allPrimTypes, getPrimHaskellName)
import Categorifier.C.Recursion (hproject)
import Control.Monad.Trans.State.Strict (State, execState, get, put)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import PyF (fmt)

-- | Make functions for each type plus union constructor types
data SimpleCFunType
  = SftCType (CType Proxy)
  | SftCUnionCon (CUnionCon Proxy)
  deriving (Eq, Ord)

-- | Make functions for each type plus union constructor types
data SimpleCxxFunType
  = SftCxxType (CxxType Proxy)
  | SftCxxUnionCon (CxxUnionCon Proxy)
  deriving (Eq, Ord)

-- | Render the @SimpleCFunType@ underlying type using the 'Kitty.CType.Render' module.
renderSimpleCFunType :: SimpleCFunType -> T.Text
renderSimpleCFunType (SftCType r) = R.renderCType r
renderSimpleCFunType (SftCUnionCon r) = R.renderCUnionConType r

-- | Render the @SimpleCxxFunType@ underlying type using the 'Kitty.CType.Render' module.
renderSimpleCxxFunType :: SimpleCxxFunType -> T.Text
renderSimpleCxxFunType (SftCxxType r) = R.renderCxxType r
renderSimpleCxxFunType (SftCxxUnionCon r) = R.renderCxxUnionConType r

data SimpleFunWriterState
  = SFWState
      [CFunction]
      (M.Map SimpleCFunType CFunction)
      (M.Map SimpleCxxFunType CFunction)

lookupCFun :: SimpleCFunType -> SimpleFunWriterState -> Maybe CFunction
lookupCFun cfunType (SFWState _ cfunMap _) = M.lookup cfunType cfunMap

lookupCxxFun :: SimpleCxxFunType -> SimpleFunWriterState -> Maybe CFunction
lookupCxxFun cxxFunType (SFWState _ _ cxxFunMap) = M.lookup cxxFunType cxxFunMap

insertCFun :: SimpleCFunType -> CFunction -> SimpleFunWriterState -> SimpleFunWriterState
insertCFun cfunType cfun (SFWState funs cfunMap cxxFunMap) =
  SFWState
    (cfun : funs)
    (M.insertWith err cfunType cfun cfunMap)
    cxxFunMap
  where
    err _ _ =
      error [fmt|inserted C function to writer state twice ({renderSimpleCFunType cfunType})|]

insertCxxFun :: SimpleCxxFunType -> CFunction -> SimpleFunWriterState -> SimpleFunWriterState
insertCxxFun (SftCxxType (CxxTypeCType ctype)) _ _ =
  error
    [fmt|Inserted CxxTypeCType ({R.renderCType ctype}) into Cxx function map.|]
insertCxxFun cxxFunType cxxFun (SFWState funs cfunMap cxxFunMap) =
  SFWState
    (cxxFun : funs)
    cfunMap
    (M.insertWith err cxxFunType cxxFun cxxFunMap)
  where
    err _ _ =
      error [fmt|inserted C function to writer state twice ({renderSimpleCxxFunType cxxFunType})|]

emptyFunWriterState :: SimpleFunWriterState
emptyFunWriterState = SFWState [] mempty mempty

getFunctionList :: SimpleFunWriterState -> [CFunction]
getFunctionList (SFWState functions _ _) = functions

newtype FunctionBaseName = FunctionBaseName T.Text

data CxxOrCCon'
  = CxxCon' CxxCon'
  | CCon' CCon'

data CCon'
  = -- | same as before
    CNullaryCon' DcName
  | -- | rendered struct type and underlying type function
    CBitfieldCon' (CBitfield Proxy) T.Text CFunction
  | -- | add field functions
    CNormalCon' DcName IsTuple (NE.NonEmpty (RfName, CType Proxy, CFunction))

data CxxCon'
  = -- | add field functions
    CxxNormalCon' DcName (NE.NonEmpty (RfName, CxxType Proxy, CFunction))

upgradeCCon :: SimpleModuleSpec -> T.Text -> CCon Proxy -> State SimpleFunWriterState CCon'
upgradeCCon _ _ (CNullaryCon dcname) = pure $ CNullaryCon' dcname
upgradeCCon spec renderedType (CBitfieldCon cbitfield@(CBitfield _ _ underlyingType)) = do
  -- get the function for the underlying type
  uintFun <- toModuleCFunction spec (SftCType (CTypePrim (bfprimToPrim underlyingType)))
  -- add that to the bitfield con
  pure $ CBitfieldCon' cbitfield renderedType uintFun
upgradeCCon spec _ (CNormalCon conName isTuple fields) = do
  let upgradeField (rfName, fieldType') = do
        fieldFun <- toModuleCFunction spec (SftCType fieldType)
        pure (rfName, fieldType, fieldFun)
        where
          fieldType = hproject fieldType'
  CNormalCon' conName isTuple <$> traverse upgradeField fields

upgradeCxxCon ::
  SimpleModuleSpec -> SimpleCxxModuleSpec -> CxxCon Proxy -> State SimpleFunWriterState CxxCon'
upgradeCxxCon spec cxxSpec (CxxNormalCon conName fields) = do
  let upgradeField (rfName, fieldType) = do
        fieldFun <- toModuleCxxFunction spec cxxSpec (SftCxxType fieldType)
        pure (rfName, fieldType, fieldFun)
  CxxNormalCon' conName <$> traverse upgradeField fields

data SimpleModuleSpec = SimpleModuleSpec
  { smFunctionBaseName :: FunctionBaseName,
    smReturnType :: Maybe ParamType,
    smFunctionParams :: Bool -> ParamType -> [Param],
    -- C
    smPrimFunctionBody :: Prim Proxy -> [TapeElement],
    smCConFunctionBody :: CCon' -> [TapeElement],
    smEnumFunctionBody :: CEnum Proxy -> [TapeElement],
    smArrayOfElemFunctionBody :: CNat -> CType Proxy -> CFunction -> [TapeElement],
    smUnionFunctionBody :: CUnion Proxy -> M.Map (CCon Proxy) CFunction -> [TapeElement]
  }

data SimpleCxxModuleSpec = SimpleCxxModuleSpec
  { -- | Haskell lists become C++ vectors, so usually we only have to generate one C++ function.
    -- In certain cases (serialization) we need different functions to get different behavior.
    smTreatListsAsVectors :: Bool,
    smCxxConFunctionBody :: CxxCon' -> [TapeElement],
    smCxxArrayOfElemFunctionBody :: CNat -> CxxType Proxy -> CFunction -> [TapeElement],
    smCxxPrimFunctionBody :: CxxPrim Proxy -> [TapeElement],
    smCxxUnionFunctionBody :: CxxUnion Proxy -> M.Map (CxxOrCCon Proxy) CFunction -> [TapeElement],
    smTupleFunctionBody :: [CFunction] -> [TapeElement],
    smMapFunctionBody :: (CxxType Proxy, CFunction) -> (CxxType Proxy, CFunction) -> [TapeElement],
    smVectorFunctionBody :: CxxType Proxy -> CFunction -> [TapeElement]
  }

toSimpleModuleCFunctions ::
  SimpleModuleSpec -> [CEnum Proxy] -> [CStructOrUnion] -> [CType Proxy] -> [CFunction]
toSimpleModuleCFunctions spec cenums cstructsOrUnions topLevelTypes =
  getFunctionList $
    execState writeFunctions emptyFunWriterState
  where
    writeFunctions = do
      -- C primitives
      traverse_ (toModuleCFunction spec . SftCType . CTypePrim) allPrimTypes
      -- C enums
      traverse_ (toModuleCFunction spec . SftCType . CTypeEnum) cenums
      -- C structs/unions
      traverse_ toStructOrUnionIsEqualFunction cstructsOrUnions
      -- all top level types
      traverse_ (toModuleCFunction spec . SftCType) topLevelTypes
      where
        toStructOrUnionIsEqualFunction (CS cstruct) =
          toModuleCFunction spec (SftCType (CTypeStruct cstruct))
        toStructOrUnionIsEqualFunction (CU cunion) =
          toModuleCFunction spec (SftCType (CTypeUnion cunion))

toSimpleModuleCxxFunctions ::
  SimpleModuleSpec ->
  SimpleCxxModuleSpec ->
  [CEnum Proxy] ->
  [CStructOrUnion] ->
  [CxxStructOrUnion] ->
  [CxxType Proxy] ->
  [CFunction]
toSimpleModuleCxxFunctions spec cxxSpec cenums cstructsOrUnions cxxstructsOrUnions topLevelTypes =
  getFunctionList $ execState writeFunctions emptyFunWriterState
  where
    writeFunctions = do
      -- C primitives
      traverse_ (toModuleCFunction spec . SftCType . CTypePrim) allPrimTypes
      -- C++ primitives
      traverse_ (toModuleCxxFunction spec cxxSpec . SftCxxType . CxxTypePrim) allCxxPrims
      -- enums
      traverse_ (toModuleCFunction spec . SftCType . CTypeEnum) cenums
      -- C structs/unions
      traverse_ toStructOrUnionIsEqualFunction cstructsOrUnions
      -- C++ structs/unions
      traverse_ toCxxStructOrUnionIsEqualFunction cxxstructsOrUnions
      -- all other top-level types
      traverse_ (toModuleCxxFunction spec cxxSpec . SftCxxType) topLevelTypes
      where
        toStructOrUnionIsEqualFunction (CS cstruct) =
          toModuleCFunction spec (SftCType (CTypeStruct cstruct))
        toStructOrUnionIsEqualFunction (CU cunion) =
          toModuleCFunction spec (SftCType (CTypeUnion cunion))
        toCxxStructOrUnionIsEqualFunction (CxxS x) =
          toModuleCxxFunction spec cxxSpec (SftCxxType (CxxTypeStruct x))
        toCxxStructOrUnionIsEqualFunction (CxxU x) =
          toModuleCxxFunction spec cxxSpec (SftCxxType (CxxTypeUnion x))

toModuleCFunction :: SimpleModuleSpec -> SimpleCFunType -> State SimpleFunWriterState CFunction
toModuleCFunction spec cFunType = do
  state0 <- get
  case lookupCFun cFunType state0 of
    Just r -> pure r
    Nothing -> do
      body <- toCFunctionBody spec cFunType
      let fun =
            UnsafeCFunction
              { cfName = toSimpleCFunctionName (smFunctionBaseName spec) cFunType,
                cfInlineOverloadName = Nothing,
                cfReturnType = smReturnType spec,
                cfStaticLinkage = False,
                cfComment =
                  Just (Comment [fmt|C function with type {renderSimpleCFunType cFunType}.|]),
                cfParams = smFunctionParams spec (cparamIsUnused cFunType) $ case cFunType of
                  SftCType r -> ParamCxxType (CxxTypeCType r)
                  SftCUnionCon r -> ParamCUnionCon r,
                cfTape = body
              }
      state1 <- get
      put $ insertCFun cFunType fun state1
      pure fun

toCFunctionBody :: SimpleModuleSpec -> SimpleCFunType -> State SimpleFunWriterState [TapeElement]
-- C prim
toCFunctionBody spec (SftCType (CTypePrim cprim)) = pure $ smPrimFunctionBody spec cprim
-- C enum
toCFunctionBody spec (SftCType (CTypeEnum cenum)) = pure $ smEnumFunctionBody spec cenum
-- C struct
toCFunctionBody spec (SftCType (CTypeStruct cstruct@(CStruct _ ccon))) = do
  ccon' <- upgradeCCon spec (R.renderCStructType cstruct) ccon
  pure $ smCConFunctionBody spec ccon'
-- C Union
toCFunctionBody spec (SftCType (CTypeUnion cunion@CUnion {})) = do
  let toCUnionConFun ccon =
        (ccon,) <$> toModuleCFunction spec (SftCUnionCon (CUnionCon cunion ccon))
  conFuns <- traverse toCUnionConFun (V.toList (cuCons cunion))
  let conFunMap :: M.Map (CCon Proxy) CFunction
      conFunMap = M.fromListWith (error "got duplicate ccons") conFuns
  pure $ smUnionFunctionBody spec cunion conFunMap
-- C Union con
toCFunctionBody spec (SftCUnionCon unionCon@(CUnionCon _ ccon)) = do
  ccon' <- upgradeCCon spec (R.renderCUnionConType unionCon) ccon
  pure $ smCConFunctionBody spec ccon'
-- C array
toCFunctionBody spec (SftCType (CTypeArray cnat elemType' _)) = do
  elemFun <- toModuleCFunction spec (SftCType elemType)
  pure $ smArrayOfElemFunctionBody spec cnat elemType elemFun
  where
    elemType = hproject elemType'

toModuleCxxFunction ::
  SimpleModuleSpec ->
  SimpleCxxModuleSpec ->
  SimpleCxxFunType ->
  State SimpleFunWriterState CFunction
toModuleCxxFunction spec _ (SftCxxType (CxxTypeCType ctype)) =
  -- Call the C verion on C types.
  toModuleCFunction spec (SftCType ctype)
toModuleCxxFunction spec cxxSpec cxxFunType0 = do
  let -- Don't generate separate functions for lists and vectors.
      -- TODO(greg): There's got to be a better way to do this.
      cxxFunType
        | SftCxxType (CxxTypeVector _ x y) <- cxxFunType0 =
            SftCxxType
              (CxxTypeVector (IsHaskellList False) x y)
        | otherwise = cxxFunType0
  state0 <- get
  case lookupCxxFun cxxFunType state0 of
    Just r -> pure r
    Nothing -> do
      body <- toModuleCxxFunctionBody spec cxxSpec cxxFunType
      let fun =
            UnsafeCFunction
              { cfName = toSimpleCxxFunctionName (smFunctionBaseName spec) cxxFunType,
                cfInlineOverloadName = Nothing,
                cfReturnType = smReturnType spec,
                cfStaticLinkage = False,
                cfComment = Just (Comment (renderSimpleCxxFunType cxxFunType)),
                cfParams = smFunctionParams spec (cxxParamIsUnused cxxFunType) $ case cxxFunType of
                  SftCxxType r -> ParamCxxType r
                  SftCxxUnionCon r -> ParamCxxUnionCon r,
                cfTape = body
              }
      state1 <- get
      put $ insertCxxFun cxxFunType fun state1
      pure fun

toModuleCxxFunctionBody ::
  SimpleModuleSpec ->
  SimpleCxxModuleSpec ->
  SimpleCxxFunType ->
  State SimpleFunWriterState [TapeElement]
-- for any C type, call the C version
toModuleCxxFunctionBody spec _ (SftCxxType (CxxTypeCType ctype)) =
  cfTape <$> toModuleCFunction spec (SftCType ctype)
-- C++ prim
toModuleCxxFunctionBody _ cxxSpec (SftCxxType (CxxTypePrim cxxPrim)) =
  pure $ smCxxPrimFunctionBody cxxSpec cxxPrim
-- C++ struct
toModuleCxxFunctionBody spec cxxSpec (SftCxxType (CxxTypeStruct (CxxStruct _ cxxCon))) =
  smCxxConFunctionBody cxxSpec <$> upgradeCxxCon spec cxxSpec cxxCon
-- C++ Union con
toModuleCxxFunctionBody spec cxxSpec (SftCxxUnionCon cxxUnionCon@(CxxUnionCon _ con)) = do
  con' <- case con of
    CxxCon cxxCon -> CxxCon' <$> upgradeCxxCon spec cxxSpec cxxCon
    CCon ccon -> CCon' <$> upgradeCCon spec (R.renderCxxUnionConType cxxUnionCon) ccon
  pure $ case con' of
    CxxCon' cxxCon' -> smCxxConFunctionBody cxxSpec cxxCon'
    CCon' ccon' -> smCConFunctionBody spec ccon'
-- C++ Union
toModuleCxxFunctionBody spec cxxSpec (SftCxxType (CxxTypeUnion cxxUnion)) = do
  let toCxxUnionConFun ccon =
        (ccon,) <$> toModuleCxxFunction spec cxxSpec (SftCxxUnionCon (CxxUnionCon cxxUnion ccon))
  conFuns <- traverse toCxxUnionConFun (cxxuCons cxxUnion)
  let conFunMap :: M.Map (CxxOrCCon Proxy) CFunction
      conFunMap = M.fromListWith (error "got duplicate ccons") (V.toList conFuns)
  pure $ smCxxUnionFunctionBody cxxSpec cxxUnion conFunMap
-- C++ array
toModuleCxxFunctionBody spec cxxSpec (SftCxxType (CxxTypeArray cnat elemType _)) = do
  elemFun <- toModuleCxxFunction spec cxxSpec (SftCxxType elemType)
  pure $ smCxxArrayOfElemFunctionBody cxxSpec cnat elemType elemFun
toModuleCxxFunctionBody spec cxxSpec (SftCxxType (CxxTypeVector _ elemType _)) = do
  elemFunction <- toModuleCxxFunction spec cxxSpec (SftCxxType elemType)
  pure $ smVectorFunctionBody cxxSpec elemType elemFunction
toModuleCxxFunctionBody spec cxxSpec (SftCxxType (CxxTypeMap keyType valueType _)) = do
  keyFunction <- toModuleCxxFunction spec cxxSpec (SftCxxType keyType)
  valueFunction <- toModuleCxxFunction spec cxxSpec (SftCxxType valueType)
  pure $ smMapFunctionBody cxxSpec (keyType, keyFunction) (valueType, valueFunction)
toModuleCxxFunctionBody spec cxxSpec (SftCxxType (CxxTypeTuple fieldTypes)) = do
  fieldFunctions <- traverse (toModuleCxxFunction spec cxxSpec . SftCxxType) fieldTypes
  pure $ smTupleFunctionBody cxxSpec fieldFunctions

toSimpleCFunctionName :: FunctionBaseName -> SimpleCFunType -> Identifier
toSimpleCFunctionName (FunctionBaseName baseName) funType = Identifier [fmt|{baseName}{typeName}|]
  where
    typeName = toSimpleCFunName' funType

toSimpleCFunName' :: SimpleCFunType -> T.Text
toSimpleCFunName' (SftCType (CTypeArray cnat elemType _)) =
  let elemTypeName = toSimpleCFunName' (SftCType $ hproject elemType)
   in [fmt|Array{cnatValue cnat}{R.renderCNat cnat}Of{elemTypeName}|]
toSimpleCFunName' (SftCType (CTypePrim cprim)) = getPrimHaskellName cprim
toSimpleCFunName' (SftCType r) = R.renderCType r
toSimpleCFunName' (SftCUnionCon unionCon) = R.renderCUnionConType unionCon

toSimpleCxxFunctionName :: FunctionBaseName -> SimpleCxxFunType -> Identifier
toSimpleCxxFunctionName (FunctionBaseName baseName) funType =
  Identifier
    [fmt|{baseName}{typeName}|]
  where
    typeName = toSimpleCxxFunName' funType

toSimpleCxxFunName' :: SimpleCxxFunType -> T.Text
-- call the C version for all CTypes so they match
toSimpleCxxFunName' (SftCxxType (CxxTypeCType ctype)) = toSimpleCFunName' (SftCType ctype)
toSimpleCxxFunName' (SftCxxType (CxxTypeMap k v _)) =
  [fmt|StdMap{toSimpleCxxFunName' (SftCxxType k)}{toSimpleCxxFunName' (SftCxxType v)}|]
toSimpleCxxFunName' (SftCxxType (CxxTypeTuple xs)) =
  [fmt|StdTuple{T.intercalate "" (toSimpleCxxFunName' . SftCxxType <$> xs)}|]
toSimpleCxxFunName' (SftCxxType r@CxxTypeStruct {}) = R.renderCxxType r
toSimpleCxxFunName' (SftCxxType r@CxxTypeUnion {}) = R.renderCxxType r
toSimpleCxxFunName' (SftCxxType (CxxTypePrim (PrimString _))) = "StdString"
toSimpleCxxFunName' (SftCxxType (CxxTypePrim (PrimByteString _))) = "ByteString"
toSimpleCxxFunName' (SftCxxType (CxxTypeVector _ elemType _)) =
  [fmt|StdVector{toSimpleCxxFunName' (SftCxxType elemType)}|]
toSimpleCxxFunName' (SftCxxType (CxxTypeArray cnat elemType _)) =
  [fmt|StdArray{cnatValue cnat}{R.renderCNat cnat}Of{toSimpleCxxFunName' (SftCxxType elemType)}|]
toSimpleCxxFunName' (SftCxxUnionCon (CxxUnionCon union con)) =
  [fmt|{R.renderCxxUnionType union}{unDcName (cxxOrCConName con)}|]

-- helpers
cxxParamIsUnused :: SimpleCxxFunType -> Bool
cxxParamIsUnused (SftCxxType (CxxTypeCType ctype)) = cparamIsUnused (SftCType ctype)
cxxParamIsUnused (SftCxxType (CxxTypeArray cnat _ _)) = cnatValue cnat == 0
cxxParamIsUnused (SftCxxUnionCon (CxxUnionCon _ (CCon CNullaryCon {}))) = True
cxxParamIsUnused _ = False

cparamIsUnused :: SimpleCFunType -> Bool
cparamIsUnused (SftCType (CTypeArray cnat _ _)) = cnatValue cnat == 0
cparamIsUnused (SftCType (CTypeStruct (CStruct _ CNullaryCon {}))) = True
cparamIsUnused (SftCUnionCon (CUnionCon _ CNullaryCon {})) = True
cparamIsUnused _ = False
