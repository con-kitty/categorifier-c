{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Generate code which serialises/deserialises C and C++ structs to and from
-- a CBOR representation compatible with "Codec.Serialise".
module Categorifier.C.CTypes.Codegen.CborSerialisation
  ( DecodeOrEncode (..),
    toCborSerialisationModule,

    -- * for test generation
    toCborFunctionName',
  )
where

import qualified Barbies
import Categorifier.C.CTypes.Codegen.Helpers (CStructOrUnion (..), CxxStructOrUnion (..))
import Categorifier.C.CTypes.Codegen.Render.Render (renderParamType)
import qualified Categorifier.C.CTypes.Codegen.Render.Render as R
import Categorifier.C.CTypes.Codegen.Render.Utils (staticAssert)
import Categorifier.C.CTypes.DSL.CxxAst
  ( CExpr (..),
    CFunction (..),
    CTypeWithBackdoor (..),
    Comment (..),
    CxxModule (..),
    CxxTarget (..),
    ExternalType (..),
    Identifier (..),
    Include (..),
    Param (..),
    ParamType (..),
    StdFunc (..),
    TapeElement (..),
    ToCTypeWithBackdoor (..),
    dereference,
    staticCast,
    (!),
    (#!),
  )
import Categorifier.C.CTypes.DSL.FunctionWriter
  ( FunWriter,
    caseUnionTag,
    comment,
    cxxLambda,
    forLoop,
    force_,
    ifElse_,
    if_,
    increment,
    loopWithType,
    newDefaultCxxNamed,
    newDefaultNamed,
    ret,
    runFunWriter,
    unsafeAsIdentifier,
    unsafeNewNamed,
    (=:),
  )
import Categorifier.C.CTypes.Instances ()
import Categorifier.C.CTypes.KTypeRep (splitKTyConApp)
import qualified Categorifier.C.CTypes.Render as R
import Categorifier.C.CTypes.Types
import Categorifier.C.KTypes.C (C (..), toCxxTypeViaC)
import Categorifier.C.KTypes.KLiteral (KLiteral (..))
import Categorifier.C.Prim (IsPrimitive, allPrimTypes, getPrimHaskellName)
import Categorifier.C.Recursion (hproject)
import Control.Monad (zipWithM, zipWithM_)
import Control.Monad.Trans.RWS (tell)
import Control.Monad.Trans.State.Strict (State, execState, get, put)
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose (..))
import Data.Int (Int64)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Tree (Tree)
import Data.Typeable (typeRep, typeRepTyCon)
import qualified Data.Vector as V
import Data.Word (Word64, Word8)
import PyF (fmt)

data DecodeOrEncode = Decode | Encode deriving (Show)

-- | Make functions for each type plus functions
-- that encode only the fields of union constructors and structs
data CborFunType
  = CfCxxType (CxxType Proxy)
  | CfCUnionConFields (CUnionCon Proxy)
  | CfCxxUnionConFields (CxxUnionCon Proxy)
  | CfCStructConFields (CStruct Proxy)
  | CfCxxStructConFields (CxxStruct Proxy)
  deriving (Eq, Ord)

-- | Render the @CborFunType@ underlying type using the 'Kitty.CType.Render' module.
renderCfType :: CborFunType -> T.Text
renderCfType (CfCxxType r) = R.renderCxxType r
renderCfType (CfCUnionConFields r) = R.renderCUnionConType r
renderCfType (CfCxxUnionConFields r) = R.renderCxxUnionConType r
renderCfType (CfCStructConFields r) = R.renderCStructType r
renderCfType (CfCxxStructConFields r) = R.renderCxxStructType r

data WrittenFunctions = WrittenFunctions [CFunction] (M.Map CborFunType CFunction)

-- for test generation
toCborFunctionName' :: DecodeOrEncode -> CxxType Proxy -> T.Text
toCborFunctionName' doe = unIdentifier . toCborFunctionName doe . CfCxxType

toCborSerialisationModule ::
  DecodeOrEncode ->
  [CEnum Proxy] ->
  [CStructOrUnion] ->
  [CxxStructOrUnion] ->
  [CxxType Proxy] ->
  CxxModule
toCborSerialisationModule doe cenums cstructsOrUnions cxxstructsOrUnions topLevelTypes =
  CxxModule
    { moduleIncludes = includes,
      moduleDefines = [],
      moduleTypedefs = [],
      moduleUsingDecls = [],
      moduleFunctions = functions,
      moduleTypeLevelFunctions = []
    }
  where
    includes =
      [ IncludeSystemFile "cbor.h",
        IncludeModule CTypes,
        IncludeModule CxxTypes,
        case doe of
          Decode ->
            IncludeLocalFile "cbor_decoder.hpp"
          Encode ->
            IncludeLocalFile "cbor_encoder.hpp"
      ]
    WrittenFunctions functions _ = execState writeFunctions (WrittenFunctions [] mempty)
    toCborFunction' = toCborFunction doe . CfCxxType
    writeFunctions = do
      traverse_ (toCborFunction' . CxxTypeCType . CTypePrim) allPrimTypes
      traverse_ (toCborFunction' . CxxTypePrim) allCxxPrims
      traverse_ (toCborFunction' . CxxTypeCType . CTypeEnum) cenums
      traverse_ toStructOrUnionCborFunction cstructsOrUnions
      traverse_ toCxxStructOrUnionSerialiseFunction cxxstructsOrUnions
      traverse_ toCborFunction' topLevelTypes
      where
        toStructOrUnionCborFunction (CS cstruct) =
          toCborFunction' (CxxTypeCType (CTypeStruct cstruct))
        toStructOrUnionCborFunction (CU cunion) = toCborFunction' (CxxTypeCType (CTypeUnion cunion))
        toCxxStructOrUnionSerialiseFunction (CxxS x) = toCborFunction' (CxxTypeStruct x)
        toCxxStructOrUnionSerialiseFunction (CxxU x) = toCborFunction' (CxxTypeUnion x)

-- If this union is actually Maybe, is is serialised differently.
-- This function detects it and returns the type parameter if it is in fact Maybe.
toCMaybeType :: CUnion Proxy -> Maybe ((CUnionCon Proxy, CUnionCon Proxy), CType Proxy)
toCMaybeType cunion@(CUnion rep ccons _ _) = case (V.toList ccons, splitKTyConApp rep) of
  ([nothing, just], (tyCon, [_]))
    | tyCon /= typeRepTyCon (typeRep (Proxy @Maybe)) -> Nothing
    | CNullaryCon {} <- nothing,
      CNormalCon _ _ ((_, elemType) NE.:| []) <- just ->
        Just
          ((CUnionCon cunion nothing, CUnionCon cunion just), hproject elemType)
  _ -> Nothing

toCxxMaybeType :: CxxUnion Proxy -> Maybe ((CxxUnionCon Proxy, CxxUnionCon Proxy), CxxType Proxy)
toCxxMaybeType cxxUnion@(CxxUnion rep cxxCons _ _) = case (V.toList cxxCons, splitKTyConApp rep) of
  ([nothing, just], (tyCon, [_]))
    | tyCon /= typeRepTyCon (typeRep (Proxy @Maybe)) -> Nothing
    | CCon CNullaryCon {} <- nothing,
      CxxCon (CxxNormalCon _ ((_, elemType) NE.:| [])) <- just ->
        Just
          ((CxxUnionCon cxxUnion nothing, CxxUnionCon cxxUnion just), elemType)
  _ -> Nothing

isCxxTreeType :: CxxStruct Proxy -> Bool
isCxxTreeType (CxxStruct rep node) = case splitKTyConApp rep of
  (tyCon, [_])
    | tyCon /= typeRepTyCon (typeRep (Proxy @Tree)) -> False
    | CxxNormalCon _ (_ NE.:| [_]) <- node -> True
  _ -> False

isUnitType :: CStruct Proxy -> Bool
isUnitType (CStruct rep con) = case splitKTyConApp rep of
  (tyCon, [])
    | -- If the type rep isn't (), then it's not the unit type.
      tyCon /= typeRepTyCon (typeRep (Proxy @())) ->
        False
    | -- Since the rep is () we know it's the right type. Lets do a sanity check just for kicks:
      con == CNullaryCon (DcName {unDcName = "()"}) ->
        True
    | -- Sanity check failed, this should be impossible. If we get here then
      -- there is some gross misunderstanding and we shouldn't go on.
      otherwise ->
        error $ "unit type with unexpected con " <> show con
  _ -> False

toCborFunction :: DecodeOrEncode -> CborFunType -> State WrittenFunctions CFunction
toCborFunction doe cfType = do
  WrittenFunctions _ funMap0 <- get
  case M.lookup cfType funMap0 of
    Just r -> pure r
    Nothing -> do
      body <- case cfType of
        -- C Union con
        CfCUnionConFields unionCon@(CUnionCon _ con) ->
          toConFieldsSerialiseBody doe (ParamCUnionCon unionCon) (CCon con)
        -- C++ Union con
        CfCxxUnionConFields unionCon@(CxxUnionCon _ con) ->
          toConFieldsSerialiseBody doe (ParamCxxUnionCon unionCon) con
        -- C++ struct
        CfCxxType (CxxTypeStruct cxxStruct@(CxxStruct _ con)) -> do
          Identifier conFieldsFun <- cfName <$> toCborFunction doe (CfCxxStructConFields cxxStruct)
          pure $
            if isCxxTreeType cxxStruct -- special-case tree
              then toCxxTreeSerialiseBody doe conFieldsFun
              else toStructSerialiseBody doe conFieldsFun (CxxCon con)
        -- C tuple
        CfCxxType (CxxTypeCType (CTypeStruct (CStruct _ (CNormalCon _ YesIAmATuple fields)))) -> do
          typeFunctions <-
            traverse
              ( fmap (unIdentifier . cfName)
                  . toCborFunction doe
                  . CfCxxType
                  . CxxTypeCType
                  . hproject
                  . snd
              )
              (NE.toList fields)
          pure $ toCTupleFunctionBody doe typeFunctions
        -- C struct
        CfCxxType (CxxTypeCType (CTypeStruct cstruct@(CStruct _ ccon)))
          | -- special-case Unit
            isUnitType cstruct ->
              pure $ toUnitSerialiseBody doe
          | otherwise -> do
              Identifier conFieldsFun <- cfName <$> toCborFunction doe (CfCStructConFields cstruct)
              pure $ toStructSerialiseBody doe conFieldsFun (CCon ccon)
        -- C++ struct fields
        CfCxxStructConFields cxxStruct@(CxxStruct _ con) ->
          toConFieldsSerialiseBody doe (ParamCxxType (CxxTypeStruct cxxStruct)) (CxxCon con)
        -- C struct fields
        CfCStructConFields cstruct@(CStruct _ con) ->
          toConFieldsSerialiseBody
            doe
            (ParamCxxType (CxxTypeCType (CTypeStruct cstruct)))
            (CCon con)
        -- C Union
        CfCxxType (CxxTypeCType (CTypeUnion cunion@CUnion {}))
          | -- special-case Maybe
            Just (cons, elemType) <- toCMaybeType cunion -> do
              elemFun <- toCborFunction doe (CfCxxType (CxxTypeCType elemType))
              pure $ toCMaybeSerialiseBody doe cunion cons elemType elemFun
          | otherwise -> toUnionSerialiseBody doe cunion
        -- C++ union
        CfCxxType (CxxTypeUnion cxxUnion)
          | -- special-case Maybe
            Just (cons, elemType) <- toCxxMaybeType cxxUnion -> do
              elemFun <- toCborFunction doe (CfCxxType elemType)
              pure $ toCxxMaybeSerialiseBody doe cons elemType elemFun
          | otherwise -> toCxxUnionSerialiseBody doe cxxUnion
        -- C++ prim
        CfCxxType (CxxTypePrim cxxPrim) -> toCxxPrimFunctionBody doe cxxPrim
        -- C prim
        CfCxxType (CxxTypeCType (CTypePrim cprim)) -> pure $ toPrimFunctionBody doe cprim
        -- C enum
        CfCxxType (CxxTypeCType (CTypeEnum cenum)) -> pure $ toEnumFunctionBody doe cenum
        -- C array
        CfCxxType (CxxTypeCType (CTypeArray cnat elemType _)) ->
          arrayOfElemFunctionBody doe cnat (CxxTypeCType $ hproject elemType)
        -- C++ array
        CfCxxType (CxxTypeArray cnat elemType _) -> arrayOfElemFunctionBody doe cnat elemType
        -- C++ vector
        CfCxxType (CxxTypeVector ihl elemType _) -> do
          elemFunction <- toCborFunction doe (CfCxxType elemType)
          let Identifier elemFunName = cfName elemFunction
              elemTypeName = R.renderCxxType elemType
          pure $ case ihl of
            IsHaskellList True -> toListFunctionBody doe (elemTypeName, elemFunName)
            IsHaskellList False -> toVectorFunctionBody doe (elemTypeName, elemFunName)
        -- C++ map
        CfCxxType (CxxTypeMap keyType valueType _) -> do
          keyFunction <- toCborFunction doe (CfCxxType keyType)
          valueFunction <- toCborFunction doe (CfCxxType valueType)
          let Identifier keyFunName = cfName keyFunction
              Identifier valueFunName = cfName valueFunction
              keyTypeName = R.renderCxxType keyType
              valueTypeName = R.renderCxxType valueType
          pure $ toMapFunctionBody doe (keyTypeName, keyFunName) (valueTypeName, valueFunName)
        -- tuple
        CfCxxType (CxxTypeTuple types) -> do
          typeFunctions <-
            traverse
              (fmap (unIdentifier . cfName) . toCborFunction doe . CfCxxType)
              types
          pure $ toCxxTupleFunctionBody doe typeFunctions
      let fun =
            UnsafeCFunction
              { cfName = toCborFunctionName doe cfType,
                cfInlineOverloadName =
                  if shouldOverload cfType
                    then Just $ Identifier [fmt|HackyOverloaded{show doe}|]
                    else Nothing,
                cfReturnType = Just (toReturnType doe),
                cfStaticLinkage = False,
                cfComment = Just . Comment . renderCfType $ cfType,
                cfParams = toFunctionParams doe cfType,
                cfTape = body
              }
      WrittenFunctions funs funMap <- get
      put $ WrittenFunctions (fun : funs) (M.insert cfType fun funMap)
      pure fun

shouldOverload :: CborFunType -> Bool
-- don't overload any of the field helpers
shouldOverload (CfCUnionConFields _) = False
shouldOverload (CfCxxUnionConFields _) = False
shouldOverload (CfCStructConFields _) = False
shouldOverload (CfCxxStructConFields _) = False
-- exclude some types
shouldOverload (CfCxxType (CxxTypeCType CTypeArray {})) = False
shouldOverload (CfCxxType CxxTypeVector {}) = False
shouldOverload (CfCxxType CxxTypeMap {}) = False
-- the rest are ok
shouldOverload (CfCxxType (CxxTypeCType CTypeStruct {})) = True
shouldOverload (CfCxxType (CxxTypeCType CTypeUnion {})) = True
shouldOverload (CfCxxType (CxxTypeCType CTypeEnum {})) = True
shouldOverload (CfCxxType (CxxTypeCType CTypePrim {})) = True
shouldOverload (CfCxxType CxxTypeStruct {}) = True
shouldOverload (CfCxxType CxxTypeUnion {}) = True
shouldOverload (CfCxxType CxxTypeTuple {}) = True
shouldOverload (CfCxxType CxxTypeArray {}) = True
shouldOverload (CfCxxType CxxTypePrim {}) = True

toCborFunctionName :: DecodeOrEncode -> CborFunType -> Identifier
toCborFunctionName doe cfType = Identifier [fmt|{show doe}_{typeName}|]
  where
    toTypeName :: CborFunType -> T.Text
    toTypeName (CfCxxType (CxxTypeMap k v _)) =
      [fmt|StdMap_{toTypeName (CfCxxType k)}_{toTypeName (CfCxxType v)}|]
    toTypeName (CfCxxType (CxxTypeTuple xs)) =
      [fmt|StdTuple_{T.intercalate "_" (toTypeName . CfCxxType <$> xs)}|]
    toTypeName (CfCxxType (CxxTypeCType (CTypeArray cnat elemType _))) =
      let elemTypeName = toTypeName (CfCxxType (CxxTypeCType $ hproject elemType))
       in [fmt|Array_{cnatValue cnat}_{R.renderCNat cnat}_{elemTypeName}|]
    toTypeName (CfCxxType (CxxTypeCType (CTypePrim p))) = getPrimHaskellName p
    toTypeName (CfCxxType r@CxxTypeCType {}) = R.renderCxxType r
    toTypeName (CfCxxType r@CxxTypeStruct {}) = R.renderCxxType r
    toTypeName (CfCxxType r@CxxTypeUnion {}) = R.renderCxxType r
    toTypeName (CfCxxType (CxxTypePrim (PrimString _))) = "StdString"
    toTypeName (CfCxxType (CxxTypePrim (PrimByteString _))) = "ByteString"
    toTypeName (CfCxxType (CxxTypeVector (IsHaskellList False) elemType _)) =
      [fmt|StdVector_{toTypeName (CfCxxType elemType)}|]
    toTypeName (CfCxxType (CxxTypeVector (IsHaskellList True) elemType _)) =
      [fmt|StdVectorActuallyListThough_{toTypeName (CfCxxType elemType)}|]
    toTypeName (CfCxxType (CxxTypeArray cnat elemType _)) =
      [fmt|StdArray_{cnatValue cnat}_{R.renderCNat cnat}_{toTypeName (CfCxxType elemType)}|]
    toTypeName (CfCUnionConFields unionCon) =
      [fmt|CUnionConFields_{R.renderCUnionConType unionCon}|]
    toTypeName (CfCxxUnionConFields (CxxUnionCon union con)) =
      [fmt|CxxUnionConFields_{R.renderCxxUnionType union}_{unDcName (cxxOrCConName con)}|]
    toTypeName (CfCStructConFields cstruct) = [fmt|CStructConFields_{R.renderCStructType cstruct}|]
    toTypeName (CfCxxStructConFields cxxStruct) =
      [fmt|CxxStructConFields_{R.renderCxxStructType cxxStruct}|]
    typeName = toTypeName cfType

-- TODO(greg): special behavior for 0-length arrays?
arrayOfElemFunctionBody ::
  DecodeOrEncode -> CNat -> CxxType Proxy -> State WrittenFunctions [TapeElement]
arrayOfElemFunctionBody doe cnat elemType = do
  elemCborFun <- toCborFunction doe (CfCxxType elemType)
  let elemFunName = cfName elemCborFun
      -- don't need address for array of arrays
      maybeAddress = case elemType of
        CxxTypeCType CTypeArray {} -> id
        CxxTypeArray {} -> id
        _ -> TakeAddress
  pure . runFunWriter $ case doe of
    Decode -> do
      comment "make sure it's an array and get the length"
      if_ (Not (cborValueIsArray it)) $ do
        kittyWarning ["(array) type not an array"]
        ret (LiteralInt 1)
      length_ <- newDefaultNamed "length" (CTypeBackdoor "size_t")
      if_ (cborValueGetArrayLength (it, TakeAddress length_)) $ do
        kittyWarning ["error getting array length"]
        ret (LiteralInt 1)
      if_ (length_ :!= (LiteralInt . fromIntegral $ cnatValue cnat)) $ do
        kittyWarning
          [ "array length mismatch"
              :<< ", cbor length "
              :<< length_
              :<< ", type length "
              :<< (LiteralInt . fromIntegral $ cnatValue cnat)
          ]
        ifElse_ (cborValueToPretty (stdout, it)) (printf "\nerror printing pretty\n") (printf "\n")
        ret (LiteralInt 1)
      recursed <- newDefaultNamed "recursed" (CTypeBackdoor "CborValue")
      if_ (cborValueEnterContainer (it, TakeAddress recursed)) $ do
        kittyWarning ["error entering container"]
        ret (LiteralInt 1)
      comment "fields"
      loopWithType (CTypeBackdoor "size_t") length_ $ \k -> do
        debugDecode $ "getting array field #" :<< k
        if_ (elemFunName #! [TakeAddress recursed, maybeAddress (value ! k)]) $ do
          kittyWarning ["error getting array element " :<< k]
          ret (LiteralInt 1)
      comment "get out of container"
      ifElse_ (cborValueAtEnd (TakeAddress recursed)) (debugDecode "(array) value at end!") $ do
        kittyWarning ["(array) value not at end, something went wrong\n"]
        ret (LiteralInt 1)
      debugDecode "array got all fields, leaving container..."
      if_ (cborValueLeaveContainer (it, TakeAddress recursed)) $ do
        kittyWarning ["(array) error leaving container"]
        ret (LiteralInt 1)
      debugDecode "array got all fields, successfully left container\n"
      ret (LiteralInt 0)
    Encode -> do
      comment "enter container"
      array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
      err <-
        unsafeNewNamed
          "err"
          (CTypeBackdoor "CborError")
          ( cborEncoderCreateArray
              (encoder, TakeAddress array_encoder, LiteralInt . fromIntegral $ cnatValue cnat)
          )
      if_ (err :&& (err :!= cborErrorOutOfMemory)) $ do
        kittyWarning ["error creating array"]
        ret (LiteralInt 1)
      case cnatValue cnat of
        0 -> comment "zero fields"
        _ -> do
          comment "fields"
          loopWithType (CTypeBackdoor "size_t") (LiteralInt . fromIntegral $ cnatValue cnat) $
            \k -> do
              debugEncode ("encoding field #" :<< k)
              if_ (elemFunName #! [TakeAddress array_encoder, value ! k]) $ do
                kittyWarning ["error encoding vector element " :<< k]
                ret (LiteralInt 1)
      comment "close container"
      debugEncode "encoded all fields, closing container..."
      err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
      if_ (err :&& (err :!= cborErrorOutOfMemory)) $ do
        kittyWarning ["error encoding closing container"]
        ret (LiteralInt 1)
      debugEncode "encoded all fields, successfully left container\n"
      ret (LiteralInt 0)

-- I expect that nullary and bitfield cons won't get these functions generated for them.
-- When that happens this function will turn into "serialise normal con fields".
-- TODO(greg): Update this when we can test bitfields.
toConFieldsSerialiseBody ::
  DecodeOrEncode -> ParamType -> CxxOrCCon Proxy -> State WrittenFunctions [TapeElement]
toConFieldsSerialiseBody _ paramType (CCon (CNullaryCon _)) = pure . runFunWriter $ do
  comment [fmt|Nullary struct {renderParamType paramType} has no fields.|]
  if_ (numFields :!= LiteralInt 0) $ do
    kittyWarning ["nullary struct has no fields but num_fields is " :<< numFields]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
toConFieldsSerialiseBody Decode paramType (CCon (CBitfieldCon cbitfield)) =
  toNormalConDecodeBody IsBitfield paramType (bitfieldToCxxTypeFields cbitfield)
toConFieldsSerialiseBody Encode paramType (CCon (CBitfieldCon cbitfield)) =
  toNormalConEncodeBody paramType (bitfieldToCxxTypeFields cbitfield)
toConFieldsSerialiseBody Decode paramType (CCon (CNormalCon _ _isTuple fields)) =
  toNormalConDecodeBody NotBitfield paramType (fmap (CxxTypeCType . hproject) <$> fields)
toConFieldsSerialiseBody Encode paramType (CCon (CNormalCon _ _ fields)) =
  toNormalConEncodeBody paramType (fmap (CxxTypeCType . hproject) <$> fields)
toConFieldsSerialiseBody Decode paramType (CxxCon (CxxNormalCon _ fields)) =
  toNormalConDecodeBody NotBitfield paramType fields
toConFieldsSerialiseBody Encode paramType (CxxCon (CxxNormalCon _ fields)) =
  toNormalConEncodeBody paramType fields

bitfieldToCxxTypeFields :: CBitfield Proxy -> NE.NonEmpty (RfName, CxxType Proxy)
bitfieldToCxxTypeFields cbitfield = fmap (fmap boolToCxxType) boolFields
  where
    boolToCxxType :: Compose Proxy C Bool -> CxxType Proxy
    boolToCxxType = CxxTypeCType . CTypePrim . PrimBool . fmap unsafeC . getCompose
    boolFields :: NE.NonEmpty (RfName, Compose Proxy C Bool)
    boolFields = fromBitfieldPrim cbitfield'
    cbitfield' :: CBitfield (Compose Proxy C)
    cbitfield' = Barbies.bmapC @IsPrimitive (Compose . fmap kliteral) cbitfield

toUnitSerialiseBody :: DecodeOrEncode -> [TapeElement]
toUnitSerialiseBody Decode = runFunWriter $ do
  debugDecode "decoding ()"
  comment "Check for null type."
  if_ (Not (cborValueIsNull it)) $ do
    kittyWarning ["type not null, it's " :<< cborValueGetType it]
    ret (LiteralInt 1)
  comment "Advance."
  if_ (cborValueAdvance it :!= LiteralInt 0) $ do
    kittyWarning ["advance failed"]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
toUnitSerialiseBody Encode = runFunWriter $ do
  comment "Encoding ()"
  comment "Encode Null."
  err <- unsafeNewNamed "err" (CTypeBackdoor "CborError") (cborEncodeNull encoder)
  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error encoding null"]
    ret (LiteralInt 1)
  ret (LiteralInt 0)

toStructSerialiseBody :: DecodeOrEncode -> T.Text -> CxxOrCCon Proxy -> [TapeElement]
toStructSerialiseBody Decode conFunName _ = runFunWriter $ do
  debugDecode [fmt|decoding struct associated with fields {conFunName}|]
  comment "enter container"
  if_ (Not (cborValueIsArray it)) $ do
    kittyWarning ["type not array, it's " :<< cborValueGetType it]
    ret (LiteralInt 1)
  length_ <- newDefaultNamed "length" (CTypeBackdoor "size_t")
  if_ (cborValueGetArrayLength (it, TakeAddress length_) :!= LiteralInt 0) $ do
    kittyWarning ["error getting array length"]
    ret (LiteralInt 1)
  if_ (length_ :== LiteralInt 0) $ do
    kittyWarning ["struct cbor array is 0 length"]
    ret (LiteralInt 1)
  recursed <- newDefaultNamed "recursed" (CTypeBackdoor "CborValue")
  if_ (cborValueEnterContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
    kittyWarning ["error entering container"]
    ret (LiteralInt 1)
  comment "KITTY_WARNING(\"type is \" << cbor_value_get_type(&recursed));"
  comment "tag"
  if_ (Not (cborValueIsUnsignedInteger (TakeAddress recursed))) $ do
    kittyWarning ["tag is not unsigned integer, it is " :<< cborValueGetType (TakeAddress recursed)]
    ret (LiteralInt 1)
  tag <- unsafeNewNamed "tag" (CTypeBackdoor "uint64_t") (LiteralInt 0)
  if_ (cborValueGetUint64 (TakeAddress recursed, TakeAddress tag) :!= LiteralInt 0) $ do
    kittyWarning ["error getting tag"]
    ret (LiteralInt 1)
  if_ (tag :!= LiteralInt 0) $ do
    kittyWarning ["tag is not 0: " :<< tag]
    ret (LiteralInt 1)
  if_ (cborValueAdvance (TakeAddress recursed) :!= LiteralInt 0) $ do
    kittyWarning ["advance failed"]
    ret (LiteralInt 1)
  comment "call field decode function"
  if_ (Identifier conFunName #! [TakeAddress recursed, value, length_ :- LiteralInt 1]) $ do
    kittyWarning ["error calling field decode function"]
    ret (LiteralInt 1)
  comment "leave container"
  if_ (cborValueLeaveContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
    kittyWarning ["error leaving container"]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
toStructSerialiseBody Encode conFunName con = runFunWriter $ do
  comment [fmt|Encoding struct associated with fields {conFunName}|]
  numFields_ <-
    unsafeNewNamed
      "num_fields"
      (CTypeBackdoor "const size_t")
      (LiteralInt . fromIntegral $ conNumFields con)
  comment "enter container"
  array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
  err <-
    unsafeNewNamed
      "err"
      (CTypeBackdoor "CborError")
      (cborEncoderCreateArray (encoder, TakeAddress array_encoder, LiteralInt 1 :+ numFields_))
  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error creating array"]
    ret (LiteralInt 1)
  comment "tag"
  tag <- unsafeNewNamed "tag" (CTypeBackdoor "const uint64_t") (LiteralInt 0)
  err =: cborEncodeUint (TakeAddress array_encoder, tag)
  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error encoding tag"]
    ret (LiteralInt 1)
  if_ (Identifier conFunName #! [TakeAddress array_encoder, value, numFields_]) $
    kittyWarning ["error encoding fields"]
  comment "close container"
  err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error closing container"]
    ret (LiteralInt 1)
  ret (LiteralInt 0)

-- | One off data type for avoiding 'Bool' blindness in toNormalConDecodeBody.
data IsBitfield = IsBitfield | NotBitfield deriving (Eq)

toNormalConDecodeBody ::
  IsBitfield ->
  ParamType ->
  NE.NonEmpty (RfName, CxxType Proxy) ->
  State WrittenFunctions [TapeElement]
toNormalConDecodeBody isBitfield paramType fields = do
  let nfields = NE.length fields
      decoder =
        if isBitfield == IsBitfield
          then decodeBitfieldField Decode
          else decodeNormalField Decode "value"
  fields_ <- concat <$> zipWithM decoder [0 ..] (NE.toList fields)
  pure $
    runFunWriter $ do
      debugDecode [fmt|decoding fields of {renderParamType paramType}|]
      if_ ((LiteralInt . fromIntegral $ nfields) :!= numFields) $ do
        kittyWarning [[fmt|wrong number of fields, expected {nfields}, got |] :<< numFields]
        ret (LiteralInt 1)
      comment "decode fields"
      tell fields_
      ret (LiteralInt 0)

toNormalConEncodeBody ::
  ParamType -> NE.NonEmpty (RfName, CxxType Proxy) -> State WrittenFunctions [TapeElement]
toNormalConEncodeBody paramType fields = do
  fieldEncodes <- concat <$> zipWithM (encodeNormalField Encode "value") [0 ..] (NE.toList fields)
  let nfields = NE.length fields
  pure . runFunWriter $ do
    comment [fmt|Encoding fields of {renderParamType paramType}|]
    if_ (numFields :!= (LiteralInt . fromIntegral $ nfields)) $ do
      kittyWarning [[fmt|wrong number of fields, expected {nfields}, actual |] :<< numFields]
      ret (LiteralInt 1)
    tell fieldEncodes
    ret (LiteralInt 0)

-- | Encode/Decode a bitfield. Haskell Cbor does not encode a bitfield as a packed primitive but as
-- an array of bools. As such we need to encode/decode it the same way in cpp land.
decodeBitfieldField ::
  DecodeOrEncode ->
  Int ->
  (RfName, CxxType Proxy) ->
  State WrittenFunctions [TapeElement]
decodeBitfieldField doe k (rfName, fieldType) = do
  fieldFun <- toCborFunction doe (CfCxxType fieldType)
  let fieldName = sanitizedRfName rfName
      shownFieldType = R.renderCxxType fieldType
      desc = [fmt|field {k}: {fieldName}: {shownFieldType}|] :: T.Text
  pure . runFunWriter $ do
    comment
      [fmt|\
field {k}
member name: {fieldName}
type: {shownFieldType}|]
    debugDecode [fmt|decoding {desc}|]
    temp <- newDefaultNamed [fmt|temp_{k}|] (CTypeBackdoor "bool")
    ifElse_
      ((cfName fieldFun #! [it, TakeAddress temp]) :== LiteralInt 0)
      ((value :-> rfName) =: temp)
      ( do
          kittyWarning [[fmt|error decoding {desc}|]]
          ret (LiteralInt 1)
      )

decodeNormalField ::
  DecodeOrEncode ->
  T.Text ->
  Int ->
  (RfName, CxxType Proxy) ->
  State WrittenFunctions [TapeElement]
decodeNormalField doe value_ k (rfName, fieldType) = do
  fieldFun <- toCborFunction doe (CfCxxType fieldType)
  let fieldFunName = cfName fieldFun
      fieldName = sanitizedRfName rfName
      shownFieldType = R.renderCxxType fieldType
      desc = [fmt|field {k}: {fieldName}: {shownFieldType}|] :: T.Text
      maybeAddress = case fieldType of
        CxxTypeCType CTypeArray {} -> id
        CxxTypeArray {} -> id
        _ -> TakeAddress
  pure . runFunWriter $ do
    comment
      [fmt|\
field {k}
member name: {fieldName}
type: {shownFieldType}|]
    debugDecode [fmt|decoding {desc}|]
    if_
      ( fieldFunName #! [it, maybeAddress ((Ident . Identifier) value_ :-> rfName)]
          :!= LiteralInt
            0
      )
      $ do
        kittyWarning [[fmt|error decoding {desc}|]]
        ret (LiteralInt 1)

encodeNormalField ::
  DecodeOrEncode ->
  T.Text ->
  Int ->
  (RfName, CxxType Proxy) ->
  State WrittenFunctions [TapeElement]
encodeNormalField doe value_ k (rfName, fieldType) = do
  fieldFun <- toCborFunction doe (CfCxxType fieldType)
  let fieldFunName = cfName fieldFun
      shownFieldType = R.renderCxxType fieldType
      fieldName = sanitizedRfName rfName
      desc = [fmt|field {k}: {fieldName}: {shownFieldType}|] :: T.Text
  pure . runFunWriter $ do
    comment
      [fmt|\
field {k}
member name: {fieldName}
type: {shownFieldType}|]
    debugEncode [fmt|encoding {desc}|]
    if_ (fieldFunName #! [encoder, (Ident . Identifier) value_ :. rfName] :!= LiteralInt 0) $ do
      kittyWarning [[fmt|error encoding {desc}|]]
      ret (LiteralInt 1)

toUnionSerialiseBody :: DecodeOrEncode -> CUnion Proxy -> State WrittenFunctions [TapeElement]
toUnionSerialiseBody doe cunion = do
  --    conFunctions :: [CFunction]
  --    conFunctions = fmap toConFunction (cuCons cunion)
  --
  --    toConFunction :: CCon Proxy -> CFunction
  --    toConFunction memberCon =
  --      (toStructSerialiseFunctionWithNames doe conTypeName conFunName (CCon memberCon))
  --      --{cfStaticLinkage = True} -- TODO(greg): reenable after removing templates
  --      where
  --        conTypeName = R.renderCUnionConType cunion memberCon
  --        conFunName = Identifier [fmt|{show doe}<{conTypeName}>|]

  let typeName :: T.Text
      typeName = R.renderCUnionType cunion
  -- tagType :: T.Text
  -- tagType = R.renderCUnionTagType cunion
  --
  -- tagLiteral = R.renderEnumConLiteralWithTypeName tagType . cconName

  let toCUnionConFieldsFun k ccon = do
        fun <- toCborFunction doe (CfCUnionConFields (CUnionCon cunion ccon))
        pure (ccon, (k, fun))
  conFieldsFuns <- zipWithM toCUnionConFieldsFun [0 ..] (V.toList (cuCons cunion))
  let conFieldsFunMap :: M.Map (CCon Proxy) (Int, CFunction)
      conFieldsFunMap = M.fromListWith (error "got duplicate ccons") conFieldsFuns
  pure $ case doe of
    Decode ->
      let unionConCase recursed ccon cconExpr = case M.lookup ccon conFieldsFunMap of
            Nothing -> error $ "missing CCon " <> show ccon
            Just (_k, conFieldsFun) ->
              let conFieldsFunName = cfName conFieldsFun
                  maybeAddress = TakeAddress -- TODO(greg): not always
                  nfields = LiteralInt . fromIntegral $ cconNumFields ccon
               in runFunWriter
                    . if_
                      ( conFieldsFunName
                          #! [TakeAddress recursed, maybeAddress cconExpr, nfields]
                          :!= LiteralInt 0
                      )
                    $ ret (LiteralInt 1)
       in -- TODO(greg): this is missing part of the body for NormalCons

          runFunWriter $ do
            debugDecode [fmt|decoding {typeName}|]
            comment "enter container"
            if_ (Not (cborValueIsArray it)) $ do
              kittyWarning ["type not array, it's " :<< cborValueGetType it]
              ret (LiteralInt 1)
            length_ <- newDefaultNamed "length" (CTypeBackdoor "size_t")
            if_ (cborValueGetArrayLength (it, TakeAddress length_) :!= LiteralInt 0) $ do
              kittyWarning ["error getting array length"]
              ret (LiteralInt 1)
            recursed <- newDefaultNamed "recursed" (CTypeBackdoor "CborValue")
            if_ (cborValueEnterContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
              kittyWarning ["error entering container"]
              ret (LiteralInt 1)
            comment "KITTY_WARNING(\"type is \" << cbor_value_get_type(&recursed));"
            comment "tag"
            if_ (Not (cborValueIsUnsignedInteger (TakeAddress recursed))) $ do
              kittyWarning
                ["tag is not unsigned integer, it is " :<< cborValueGetType (TakeAddress recursed)]
              ret (LiteralInt 1)
            tag <- unsafeNewNamed "tag" (CTypeBackdoor "uint64_t") (LiteralInt 0)
            if_ (cborValueGetUint64 (TakeAddress recursed, TakeAddress tag) :!= LiteralInt 0) $ do
              kittyWarning ["error getting tag"]
              ret (LiteralInt 1)
            if_ (cborValueAdvance (TakeAddress recursed) :!= LiteralInt 0) $ do
              kittyWarning ["advance failed"]
              ret (LiteralInt 1)
            value
              :-> makeRfName (R.renderCUnionTagMember cunion)
              =: staticCast (CTypeBackdoor $ R.renderCUnionTagType cunion) tag
            comment "which constructor"
            tell [CUnionConSwitch (Ident (Identifier "value")) cunion (unionConCase recursed)]
            comment "leave container"
            if_ (cborValueLeaveContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
              kittyWarning ["error leaving container"]
              ret (LiteralInt 1)
            ret (LiteralInt 0)
    Encode ->
      let unionConCase :: CCon Proxy -> CExpr -> [TapeElement]
          unionConCase ccon cconExpr = case M.lookup ccon conFieldsFunMap of
            Nothing -> error $ "missing CCon " <> show ccon
            Just (k, conFieldsFun) ->
              -- [FunctionCall conFun [cconExpr] Nothing]
              let conFieldsFunName = cfName conFieldsFun
                  maybeAddress = id -- TODO(greg): not always
                  nfields = LiteralInt . fromIntegral $ cconNumFields ccon
               in runFunWriter $ do
                    comment [fmt|Encoding union constructor {unDcName (cconName ccon)}|]
                    numFields_ <-
                      unsafeNewNamed
                        "num_fields"
                        (CTypeBackdoor "const size_t")
                        (LiteralInt . fromIntegral $ cconNumFields ccon)
                    comment "enter container"
                    array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
                    err <-
                      unsafeNewNamed
                        "err"
                        (CTypeBackdoor "CborError")
                        ( cborEncoderCreateArray
                            (encoder, TakeAddress array_encoder, LiteralInt 1 :+ numFields_)
                        )
                    if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
                      kittyWarning ["error creating array"]
                      ret (LiteralInt 1)
                    comment "tag"
                    tag <-
                      unsafeNewNamed
                        "tag"
                        (CTypeBackdoor "const uint64_t")
                        (LiteralInt . fromIntegral $ k)
                    err =: cborEncodeUint (TakeAddress array_encoder, tag)
                    if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
                      kittyWarning ["error encoding tag"]
                      ret (LiteralInt 1)
                    if_
                      ( conFieldsFunName
                          #! [TakeAddress array_encoder, maybeAddress cconExpr, nfields]
                          :!= LiteralInt 0
                      )
                      $ do
                        kittyWarning
                          [ [fmt|\
error encoding union constructor fields {unDcName (cconName ccon)}|]
                          ]
                        ret (LiteralInt 1)
                    comment "close container"
                    err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
                    if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
                      kittyWarning ["error closing container"]
                      ret (LiteralInt 1)
          -- TODO(greg): NO!
          maybeAddress' = TakeAddress
       in CUnionConSwitch (maybeAddress' (Ident (Identifier "value"))) cunion unionConCase :
          runFunWriter (ret (LiteralInt 0))

toCxxUnionSerialiseBody :: DecodeOrEncode -> CxxUnion Proxy -> State WrittenFunctions [TapeElement]
toCxxUnionSerialiseBody doe cxxUnion = do
  let typeName = R.renderCxxUnionType cxxUnion
      toCxxUnionConFieldsFun ccon =
        (ccon,) <$> toCborFunction doe (CfCxxUnionConFields (CxxUnionCon cxxUnion ccon))
  conFieldsFuns <- traverse toCxxUnionConFieldsFun (cxxuCons cxxUnion)
  pure $ case doe of
    Decode ->
      let cases recursed =
            zip
              (T.pack . show <$> [(0 :: Int) ..])
              (toCase <$> V.toList conFieldsFuns)
            where
              toCase :: (CxxOrCCon Proxy, CFunction) -> [TapeElement]
              toCase (con, conFieldsFun) =
                let conFieldsFunName = cfName conFieldsFun
                 in runFunWriter $ do
                      con_value <-
                        newDefaultCxxNamed
                          "con_value"
                          (CTypeBackdoor $ R.renderCxxUnionConType (CxxUnionCon cxxUnion con))
                      if_
                        ( conFieldsFunName
                            #! [ TakeAddress recursed,
                                 TakeAddress con_value,
                                 LiteralInt . fromIntegral $ conNumFields con
                               ]
                        )
                        $ do
                          kittyWarning
                            [[fmt|error decoding union constructor {unDcName (cxxOrCConName con)}|]]
                          ret (LiteralInt 1)
                      value :-> #payload =: con_value
       in runFunWriter $ do
            debugDecode [fmt|decoding {typeName}|]
            comment "enter container"
            if_ (Not (cborValueIsArray it)) $ do
              kittyWarning ["type not array, it's " :<< cborValueGetType it]
              ret (LiteralInt 1)
            length_ <- newDefaultNamed "length" (CTypeBackdoor "size_t")
            if_ (cborValueGetArrayLength (it, TakeAddress length_) :!= LiteralInt 0) $ do
              kittyWarning ["error getting array length"]
              ret (LiteralInt 1)
            recursed <- newDefaultNamed "recursed" (CTypeBackdoor "CborValue")
            if_ (cborValueEnterContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
              kittyWarning ["error entering container"]
              ret (LiteralInt 1)
            comment "KITTY_WARNING(\"type is \" << cbor_value_get_type(&recursed));"
            comment "tag"
            if_ (Not (cborValueIsUnsignedInteger (TakeAddress recursed))) $ do
              kittyWarning
                ["tag is not unsigned integer, it is " :<< cborValueGetType (TakeAddress recursed)]
              ret (LiteralInt 1)
            tag <- unsafeNewNamed "tag" (CTypeBackdoor "uint64_t") (LiteralInt 0)
            if_ (cborValueGetUint64 (TakeAddress recursed, TakeAddress tag) :!= LiteralInt 0) $ do
              kittyWarning ["error getting tag"]
              ret (LiteralInt 1)
            if_ (cborValueAdvance (TakeAddress recursed) :!= LiteralInt 0) $ do
              kittyWarning ["advance failed"]
              ret (LiteralInt 1)
            comment "which constructor"
            tell
              [ SwitchCase
                  -- In this context, tag is an unsigned int unpacked by the serialiser So that's
                  -- not an ENUM and does not come with the kForcesSigned and kNumFields cases
                  Nothing
                  (Ident (Identifier "tag"))
                  (cases recursed)
                  (runFunWriter $ ret (LiteralInt 1))
              ]
            comment "leave container"
            if_ (cborValueLeaveContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
              kittyWarning ["error leaving container"]
              ret (LiteralInt 1)
            ret (LiteralInt 0)
    Encode ->
      let toCase :: Int -> (CxxOrCCon Proxy, CFunction) -> CExpr
          toCase k (con, conFieldsFun) =
            let conFieldsFunName = cfName conFieldsFun
                conName = unDcName (cxxOrCConName con)
                nfields = LiteralInt . fromIntegral $ conNumFields con
             in cxxLambda [unsafeAsIdentifier encoder] [CxxUnionCon cxxUnion con] $ \con_value -> do
                  comment conName
                  debugEncode [fmt|encoding {conName}|]
                  numFields_ <- unsafeNewNamed "num_fields" (CTypeBackdoor "const size_t") nfields
                  comment "enter container"
                  array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
                  err <-
                    unsafeNewNamed
                      "err"
                      (CTypeBackdoor "CborError")
                      ( cborEncoderCreateArray
                          (encoder, TakeAddress array_encoder, LiteralInt 1 :+ numFields_)
                      )
                  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
                    kittyWarning ["error creating array"]
                    ret (LiteralInt 1)
                  comment "tag"
                  tag <-
                    unsafeNewNamed "tag" (CTypeBackdoor "uint64_t") (LiteralInt . fromIntegral $ k)
                  err =: cborEncodeUint (TakeAddress array_encoder, tag)
                  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
                    kittyWarning ["error encoding tag"]
                    ret (LiteralInt 1)
                  comment "call constructor serializer"
                  if_ (conFieldsFunName #! [TakeAddress array_encoder, con_value, numFields_]) $
                    ret (LiteralInt 1)
                  comment "close container"
                  err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
                  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
                    kittyWarning ["error closing container"]
                    ret (LiteralInt 1)
                  ret (LiteralInt 0)
       in runFunWriter $ do
            let cases = zipWith toCase [0 ..] (V.toList conFieldsFuns)
            ret $ value :. #payload :. #match #! cases

toReturnType :: DecodeOrEncode -> ParamType
toReturnType Decode = decodeResultType
toReturnType Encode = encodeResultType

decodeResultType :: ParamType
decodeResultType = ParamCxxType (CxxTypeCType (CTypePrim (PrimInt32 Proxy)))

encodeResultType :: ParamType
encodeResultType = ParamCxxType (CxxTypeCType (CTypePrim (PrimInt32 Proxy)))

cborValue :: ParamType
cborValue = ParamExternalType (ExternalType "CborValue")

cborEncoder :: ParamType
cborEncoder = ParamExternalType (ExternalType "CborEncoder")

toFunctionParams :: DecodeOrEncode -> CborFunType -> [Param]
toFunctionParams Decode t =
  [ Param {pType = cborValue, pId = Identifier "it", pUnused = paramIsUnused t, pMutable = True},
    Param
      { pType = case t of
          CfCxxType r -> ParamCxxType r
          CfCUnionConFields r -> ParamCUnionCon r
          CfCxxUnionConFields r -> ParamCxxUnionCon r
          CfCStructConFields r -> ParamCxxType (CxxTypeCType (CTypeStruct r))
          CfCxxStructConFields r -> ParamCxxType (CxxTypeStruct r),
        pId = Identifier "value",
        pUnused = paramIsUnused t,
        pMutable = True
      }
  ]
    <> case t of
      CfCxxType _ -> []
      CfCUnionConFields _ -> [lengthParam]
      CfCxxUnionConFields _ -> [lengthParam]
      CfCStructConFields _ -> [lengthParam]
      CfCxxStructConFields _ -> [lengthParam]
  where
    -- length param is passed to union constructors
    lengthParam =
      Param
        { pType = ParamCxxType (CxxTypeCType (CTypePrim (PrimWord64 Proxy))),
          pId = Identifier "num_fields",
          pUnused = paramIsUnused' t,
          pMutable = False
        }
toFunctionParams Encode t =
  [ Param
      { pType = cborEncoder,
        pId = Identifier "encoder",
        pUnused = paramIsUnused t,
        pMutable = True
      },
    Param
      { pType = case t of
          CfCxxType r -> ParamCxxType r
          CfCUnionConFields r -> ParamCUnionCon r
          CfCxxUnionConFields r -> ParamCxxUnionCon r
          CfCStructConFields r -> ParamCxxType (CxxTypeCType (CTypeStruct r))
          CfCxxStructConFields r -> ParamCxxType (CxxTypeStruct r),
        pId = Identifier "value",
        pUnused = paramIsUnused' t,
        pMutable = False
      }
  ]
    <> case t of
      CfCxxType _ -> []
      CfCUnionConFields _ -> [lengthParam]
      CfCxxUnionConFields _ -> [lengthParam]
      CfCStructConFields _ -> [lengthParam]
      CfCxxStructConFields _ -> [lengthParam]
  where
    -- length param is passed to union constructors
    lengthParam =
      Param
        { pType = ParamCxxType (CxxTypeCType (CTypePrim (PrimWord64 Proxy))),
          pId = Identifier "num_fields",
          pUnused = paramIsUnused' t,
          pMutable = False
        }

paramIsUnused :: CborFunType -> Bool
paramIsUnused (CfCxxType (CxxTypeCType (CTypeStruct (CStruct _ CNullaryCon {})))) = True
paramIsUnused (CfCUnionConFields (CUnionCon _ CNullaryCon {})) = True
paramIsUnused (CfCxxUnionConFields (CxxUnionCon _ (CCon CNullaryCon {}))) = True
paramIsUnused (CfCStructConFields (CStruct _ CNullaryCon {})) = True
paramIsUnused _ = False

paramIsUnused' :: CborFunType -> Bool
paramIsUnused' (CfCxxType (CxxTypeCType (CTypeArray cnat _ _))) | cnatValue cnat == 0 = True
paramIsUnused' t = paramIsUnused t

-- TODO: arguments of the function, wrap them
it :: CExpr
it = Ident (Identifier "it")

value :: CExpr
value = Ident (Identifier "value")

encoder :: CExpr
encoder = Ident (Identifier "encoder")

numFields :: CExpr
numFields = Ident (Identifier "num_fields")

-- * Cbor functions

cborValueIsArray :: CExpr -> CExpr
cborValueIsArray v = Identifier "cbor_value_is_array" #! [v]

cborValueIsUnsignedInteger :: CExpr -> CExpr
cborValueIsUnsignedInteger v = Identifier "cbor_value_is_unsigned_integer" #! [v]

cborValueGetType :: CExpr -> CExpr
cborValueGetType v = Identifier "cbor_value_get_type" #! [v]

cborValueGetArrayLength :: (CExpr, CExpr) -> CExpr
cborValueGetArrayLength (a, b) = Identifier "cbor_value_get_array_length" #! [a, b]

cborValueEnterContainer :: (CExpr, CExpr) -> CExpr
cborValueEnterContainer (a, b) = Identifier "cbor_value_enter_container" #! [a, b]

cborValueLeaveContainer :: (CExpr, CExpr) -> CExpr
cborValueLeaveContainer (a, b) = Identifier "cbor_value_leave_container" #! [a, b]

cborEncoderCloseContainerChecked :: (CExpr, CExpr) -> CExpr
cborEncoderCloseContainerChecked (a, b) =
  Identifier "cbor_encoder_close_container_checked" #! [a, b]

cborValueGetUint64 :: (CExpr, CExpr) -> CExpr
cborValueGetUint64 (a, b) = Identifier "cbor_value_get_uint64" #! [a, b]

cborValueAdvance :: CExpr -> CExpr
cborValueAdvance v = Identifier "cbor_value_advance" #! [v]

cborEncoderCreateArray :: (CExpr, CExpr, CExpr) -> CExpr
cborEncoderCreateArray (a, b, c) = Identifier "cbor_encoder_create_array" #! [a, b, c]

cborEncodeUint :: (CExpr, CExpr) -> CExpr
cborEncodeUint (a, b) = Identifier "cbor_encode_uint" #! [a, b]

cborEncodeNull :: CExpr -> CExpr
cborEncodeNull v = Identifier "cbor_encode_null" #! [v]

cborValueIsNull :: CExpr -> CExpr
cborValueIsNull v = Identifier "cbor_value_is_null" #! [v]

cborTypeToString :: CExpr -> CExpr
cborTypeToString v = Identifier "CborTypeToString" #! [v]

cborValueGetHalfFloat :: (CExpr, CExpr) -> CExpr
cborValueGetHalfFloat (a, b) = Identifier "cbor_value_get_half_float" #! [a, b]

cborValueIsHalfFloat :: CExpr -> CExpr
cborValueIsHalfFloat v = Identifier "cbor_value_is_half_float" #! [v]

cborValueGetDouble :: (CExpr, CExpr) -> CExpr
cborValueGetDouble (a, b) = Identifier "cbor_value_get_double" #! [a, b]

cborValueIsDouble :: CExpr -> CExpr
cborValueIsDouble v = Identifier "cbor_value_is_double" #! [v]

cborValueIsFloat :: CExpr -> CExpr
cborValueIsFloat v = Identifier "cbor_value_is_float" #! [v]

cborValueGetFloat :: (CExpr, CExpr) -> CExpr
cborValueGetFloat (a, b) = Identifier "cbor_value_get_float" #! [a, b]

cborValueIsBoolean :: CExpr -> CExpr
cborValueIsBoolean v = Identifier "cbor_value_is_boolean" #! [v]

cborValueGetBoolean :: (CExpr, CExpr) -> CExpr
cborValueGetBoolean (a, b) = Identifier "cbor_value_get_boolean" #! [a, b]

cborValueIsInteger :: CExpr -> CExpr
cborValueIsInteger v = Identifier "cbor_value_is_integer" #! [v]

cbor_value_get_int64_checked :: (CExpr, CExpr) -> CExpr
cbor_value_get_int64_checked (a, b) = Identifier "cbor_value_get_int64_checked" #! [a, b]

cborEncodeBoolean :: (CExpr, CExpr) -> CExpr
cborEncodeBoolean (a, b) = Identifier "cbor_encode_boolean" #! [a, b]

cborEncodeDouble :: (CExpr, CExpr) -> CExpr
cborEncodeDouble (a, b) = Identifier "cbor_encode_double" #! [a, b]

cborEncodeFloat :: (CExpr, CExpr) -> CExpr
cborEncodeFloat (a, b) = Identifier "cbor_encode_float" #! [a, b]

cborEncodeInt :: (CExpr, CExpr) -> CExpr
cborEncodeInt (a, b) = Identifier "cbor_encode_int" #! [a, b]

cborValueIsTextString :: CExpr -> CExpr
cborValueIsTextString v = Identifier "cbor_value_is_text_string" #! [v]

cborValueGetStringLength :: (CExpr, CExpr) -> CExpr
cborValueGetStringLength (a, b) = Identifier "cbor_value_get_string_length" #! [a, b]

cborValueCopyTextString :: (CExpr, CExpr, CExpr, CExpr) -> CExpr
cborValueCopyTextString (a, b, c, d) = Identifier "cbor_value_copy_text_string" #! [a, b, c, d]

cborEncodeTextString :: (CExpr, CExpr, CExpr) -> CExpr
cborEncodeTextString (a, b, c) = Identifier "cbor_encode_text_string" #! [a, b, c]

cborValueToPretty :: (CExpr, CExpr) -> CExpr
cborValueToPretty (a, b) = Identifier "cbor_value_to_pretty" #! [a, b]

cborValueIsMap :: CExpr -> CExpr
cborValueIsMap v = Identifier "cbor_value_is_map" #! [v]

cborValueAtEnd :: CExpr -> CExpr
cborValueAtEnd v = Identifier "cbor_value_at_end" #! [v]

cborEncoderCreateMap :: (CExpr, CExpr, CExpr) -> CExpr
cborEncoderCreateMap (a, b, c) = Identifier "cbor_encoder_create_map" #! [a, b, c]

-- Utils

kittyWarning :: [CExpr] -> FunWriter ()
kittyWarning args = force_ $ Identifier "KITTY_WARNING" #! [foldl1 (:<<) args]

printf :: CExpr -> FunWriter ()
printf a = force_ $ Identifier "printf" #! [a]

stdout :: CExpr
stdout = Ident (Identifier "stdout")

stdNumericLimitsHasQuietNaN :: CType Proxy -> CExpr
stdNumericLimitsHasQuietNaN t =
  Ident (Identifier [fmt|std::numeric_limits<{R.renderCType t}>::has_quiet_NaN|])

stdNumericLimitsQuietNaN :: CType Proxy -> CExpr
stdNumericLimitsQuietNaN t =
  Ident (Identifier [fmt|std::numeric_limits<{R.renderCType t}>::quiet_NaN|])

cborIndefiniteLength :: CExpr
cborIndefiniteLength = Ident (Identifier "CborIndefiniteLength")

cborErrorOutOfMemory :: CExpr
cborErrorOutOfMemory = Ident (Identifier "CborErrorOutOfMemory")

stdPair :: (ToCTypeWithBackdoor a, ToCTypeWithBackdoor b) => a -> b -> T.Text
stdPair t0 t1 =
  [fmt|std::pair<{R.renderCTypeWithBackdoor (toCTypeWithBackdoor t0)}, \
{R.renderCTypeWithBackdoor (toCTypeWithBackdoor t1)}>|]

debugDecode :: CExpr -> FunWriter ()
debugDecode s = force_ $ Ident (Identifier "DEBUG_DECODE") #! [s]

debugEncode :: CExpr -> FunWriter ()
debugEncode x = force_ $ Identifier "DEBUG_ENCODE" #! [x]

toEnumFunctionBody :: DecodeOrEncode -> CEnum Proxy -> [TapeElement]
toEnumFunctionBody Decode cenum@CEnum {ceCons = cons} = runFunWriter $ do
  comment "make sure it's an array with correct length (one more than outputs)"
  if_ (Not (cborValueIsArray it)) $ do
    kittyWarning ["type not array, it's ", cborValueGetType it]
    ret (LiteralInt 1)
  length_ <- newDefaultNamed "length" (CTypeBackdoor "size_t")
  if_ (cborValueGetArrayLength (it, TakeAddress length_) :!= LiteralInt 0) $ do
    kittyWarning ["error getting array length"]
    ret (LiteralInt 1)
  if_ (length_ :!= LiteralInt 1) $ do
    kittyWarning ["vec size not 1, it's ", length_]
    ret (LiteralInt 1)
  recursed <- newDefaultNamed "recursed" (CTypeBackdoor "CborValue")
  if_ (cborValueEnterContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
    kittyWarning ["error entering container"]
    ret (LiteralInt 1)
  comment "KITTY_WARNING(\"type is \" << cbor_value_get_type(&recursed));"
  comment "tag"
  if_ (Not (cborValueIsUnsignedInteger (TakeAddress recursed))) $ do
    kittyWarning ["tag is not unsigned integer, it is " :<< cborValueGetType (TakeAddress recursed)]
    ret (LiteralInt 1)
  tag <- unsafeNewNamed "tag" (CTypeBackdoor "uint64_t") (LiteralInt 0)
  if_ (cborValueGetUint64 (TakeAddress recursed, TakeAddress tag) :!= LiteralInt 0) $ do
    kittyWarning ["error getting tag"]
    ret (LiteralInt 1)
  comment "value"
  if_ (tag :>= (LiteralInt . fromIntegral $ length cons)) $ do
    kittyWarning ["tag is out of range: " :<< tag]
    ret (LiteralInt 1)
  dereference value =: staticCast (CTypeEnum' cenum) tag
  if_ (cborValueAdvance (TakeAddress recursed) :!= LiteralInt 0) $ do
    kittyWarning ["advance failed"]
    ret (LiteralInt 1)
  comment "leave container"
  if_ (cborValueLeaveContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
    kittyWarning ["error leaving container"]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
toEnumFunctionBody Encode _cenum = runFunWriter $ do
  comment "create container"
  array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
  err <-
    unsafeNewNamed "err" (CTypeBackdoor "CborError") $
      cborEncoderCreateArray (encoder, TakeAddress array_encoder, LiteralInt 1)
  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error creating array"]
    ret (LiteralInt 1)
  comment "tag"
  tag <-
    unsafeNewNamed "tag" (CTypeBackdoor "const uint64_t") $
      staticCast (CTypePrim' (PrimWord64 Proxy)) value
  err =: cborEncodeUint (TakeAddress array_encoder, tag)
  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error getting tag"]
    ret (LiteralInt 1)
  comment "leave container"
  err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error closing container"]
    ret (LiteralInt 1)
  ret (LiteralInt 0)

toCxxPrimFunctionBody :: DecodeOrEncode -> CxxPrim Proxy -> State WrittenFunctions [TapeElement]
toCxxPrimFunctionBody Decode (PrimString _) = pure . runFunWriter $ do
  if_ (Not (cborValueIsTextString it)) $ do
    kittyWarning ["type not string, it's " :<< cborTypeToString (cborValueGetType it)]
    ret (LiteralInt 1)
  comment "length"
  length_ <- unsafeNewNamed "length" (CTypeBackdoor "size_t") (LiteralInt 0)
  if_ (cborValueGetStringLength (it, TakeAddress length_) :!= LiteralInt 0) $ do
    kittyWarning ["error getting string length"]
    ret (LiteralInt 1)
  force_ $ value :-> #resize #! [length_]
  comment "copy"
  if_
    ( cborValueCopyTextString
        (it, TakeAddress (dereference value ! LiteralInt 0), TakeAddress length_, it)
        :!= LiteralInt 0
    )
    $ do
      kittyWarning ["error copying string"]
      ret (LiteralInt 1)
  comment "no advance needed"
  ret (LiteralInt 0)
toCxxPrimFunctionBody Encode (PrimString _) = pure . runFunWriter $ do
  err <-
    unsafeNewNamed
      "err"
      (CTypeBackdoor "CborError")
      (cborEncodeTextString (encoder, value :. #c_str #! [], value :. #size #! []))
  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["encode: error encoding string (" :<< value :<< "): " :<< err]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
toCxxPrimFunctionBody doe (PrimByteString _) = do
  let elemType = toCxxTypeViaC (Proxy @Word8)
  elemFunction <- toCborFunction doe (CfCxxType elemType)
  let Identifier elemFunName = cfName elemFunction
      elemTypeName = R.renderCxxType elemType
  pure $ toVectorFunctionBody doe (elemTypeName, elemFunName)

decodeInt64FunName :: T.Text
decodeInt64FunName = toCborFunctionName' Decode (toCxxTypeViaC (Proxy @Int64))

decodeUInt64FunName :: T.Text
decodeUInt64FunName = toCborFunctionName' Decode (toCxxTypeViaC (Proxy @Word64))

toPrimFunctionBody :: DecodeOrEncode -> Prim Proxy -> [TapeElement]
toPrimFunctionBody Decode (PrimDouble _) = runFunWriter $ do
  comment "Normal path, when it is actually a double."
  if_ (cborValueIsDouble it) $ do
    if_ (cborValueGetDouble (it, value) :!= LiteralInt 0) $ do
      kittyWarning ["decode: error getting double"]
      ret (LiteralInt 1)
    if_ (cborValueAdvance it :!= LiteralInt 0) $ do
      kittyWarning ["decode: advance failed"]
      ret (LiteralInt 1)
    ret (LiteralInt 0)
  comment "If it's not a double, it might be a NaN which"
  comment "is encoded as a half float of value 0x7e00"
  if_ (cborValueIsHalfFloat it) $ do
    half_float_val <- unsafeNewNamed "half_float_val" (CTypePrim' (PrimWord16 Proxy)) (LiteralInt 0)
    if_ (cborValueGetHalfFloat (it, TakeAddress half_float_val) :!= LiteralInt 0) $ do
      kittyWarning ["error decoding half float"]
      ret (LiteralInt 1)
    -- TODO: the constant here is weird
    if_ (half_float_val :== Ident (Identifier "0x7e00")) $ do
      staticAssert
        (stdNumericLimitsHasQuietNaN (CTypePrim (PrimDouble Proxy)))
        "cbor NaN decoding requires std::numeric_limits::has_quiet_NaN"
      dereference value =: (stdNumericLimitsQuietNaN (CTypePrim (PrimDouble Proxy)) #! [])
      if_ (cborValueAdvance it :!= LiteralInt 0) $ do
        kittyWarning ["decode: advance failed"]
        ret (LiteralInt 1)
      ret (LiteralInt 0)
    kittyWarning ["decode: half float is NOT a NaN"]
    ret (LiteralInt 1)
  kittyWarning
    ["decode: type not double or half float, it's " :<< cborTypeToString (cborValueGetType it)]
  ret (LiteralInt 1)
toPrimFunctionBody Decode (PrimFloat _) = runFunWriter $ do
  if_ (Not (cborValueIsFloat it)) $ do
    kittyWarning ["decode: type not float, it's " :<< cborTypeToString (cborValueGetType it)]
    ret (LiteralInt 1)
  if_ (cborValueGetFloat (it, value) :!= LiteralInt 0) $ do
    kittyWarning ["decode: error getting float"]
    ret (LiteralInt 1)
  if_ (cborValueAdvance it :!= LiteralInt 0) $ do
    kittyWarning ["decode: advance failed"]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
toPrimFunctionBody Decode (PrimBool _) = runFunWriter $ do
  if_ (Not (cborValueIsBoolean it)) $ do
    kittyWarning ["decode: type not bool, it's " :<< cborTypeToString (cborValueGetType it)]
    ret (LiteralInt 1)
  if_ (cborValueGetBoolean (it, value) :!= LiteralInt 0) $ do
    kittyWarning ["decode: error getting bool"]
    ret (LiteralInt 1)
  if_ (cborValueAdvance it :!= LiteralInt 0) $ do
    kittyWarning ["decode: advance failed"]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
toPrimFunctionBody Decode (PrimInt64 _) = runFunWriter $ do
  if_ (Not (cborValueIsInteger it)) $ do
    kittyWarning ["decode: type not int64, it's " :<< cborTypeToString (cborValueGetType it)]
    ret (LiteralInt 1)
  if_ (cbor_value_get_int64_checked (it, value) :!= LiteralInt 0) $ do
    kittyWarning ["decode: error getting int64"]
    ret (LiteralInt 1)
  if_ (cborValueAdvance it :!= LiteralInt 0) $ do
    kittyWarning ["decode: advance failed"]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
toPrimFunctionBody Decode (PrimInt32 _) = runFunWriter $ do
  y <- newDefaultNamed "y" (CTypeBackdoor "int64_t")
  status <-
    unsafeNewNamed
      "status"
      (CTypeBackdoor "int")
      (Ident (Identifier decodeInt64FunName) #! [it, TakeAddress y])
  if_ (status :!= LiteralInt 0) $ ret status
  dereference value =: staticCast (CTypePrim' (PrimInt32 Proxy)) y
  ret (LiteralInt 0)
toPrimFunctionBody Decode (PrimInt16 _) = runFunWriter $ do
  y <- newDefaultNamed "y" (CTypeBackdoor "int64_t")
  status <-
    unsafeNewNamed
      "status"
      (CTypeBackdoor "int")
      (Ident (Identifier decodeInt64FunName) #! [it, TakeAddress y])
  if_ (status :!= LiteralInt 0) $ ret status
  dereference value =: staticCast (CTypePrim' (PrimInt16 Proxy)) y
  ret (LiteralInt 0)
toPrimFunctionBody Decode (PrimInt8 _) = runFunWriter $ do
  y <- newDefaultNamed "y" (CTypeBackdoor "int64_t")
  status <-
    unsafeNewNamed
      "status"
      (CTypeBackdoor "int")
      (Ident (Identifier decodeInt64FunName) #! [it, TakeAddress y])
  if_ (status :!= LiteralInt 0) $ ret status
  dereference value =: staticCast (CTypePrim' (PrimInt8 Proxy)) y
  ret (LiteralInt 0)
toPrimFunctionBody Decode (PrimWord64 _) = runFunWriter $ do
  if_ (Not (cborValueIsUnsignedInteger it)) $ do
    kittyWarning ["decode: type not uint64, it's " :<< cborTypeToString (cborValueGetType it)]
    ret (LiteralInt 1)
  if_ (cborValueGetUint64 (it, value) :!= LiteralInt 0) $ do
    kittyWarning ["decode: error getting uint64"]
    ret (LiteralInt 1)
  if_ (cborValueAdvance it :!= LiteralInt 0) $ do
    kittyWarning ["decode: advance failed"]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
toPrimFunctionBody Decode (PrimWord32 _) = runFunWriter $ do
  y <- newDefaultNamed "y" (CTypeBackdoor "uint64_t")
  status <-
    unsafeNewNamed
      "status"
      (CTypeBackdoor "int")
      (Ident (Identifier decodeUInt64FunName) #! [it, TakeAddress y])
  if_ (status :!= LiteralInt 0) $ ret status
  dereference value =: staticCast (CTypePrim' (PrimWord32 Proxy)) y
  ret (LiteralInt 0)
toPrimFunctionBody Decode (PrimWord16 _) = runFunWriter $ do
  y <- newDefaultNamed "y" (CTypeBackdoor "uint64_t")
  status <-
    unsafeNewNamed
      "status"
      (CTypeBackdoor "int")
      (Ident (Identifier decodeUInt64FunName) #! [it, TakeAddress y])
  if_ (status :!= LiteralInt 0) $ ret status
  dereference value =: staticCast (CTypePrim' (PrimWord16 Proxy)) y
  ret (LiteralInt 0)
toPrimFunctionBody Decode (PrimWord8 _) = runFunWriter $ do
  y <- newDefaultNamed "y" (CTypeBackdoor "uint64_t")
  status <-
    unsafeNewNamed
      "status"
      (CTypeBackdoor "int")
      (Ident (Identifier decodeUInt64FunName) #! [it, TakeAddress y])
  if_ (status :!= LiteralInt 0) $ ret status
  dereference value =: staticCast (CTypePrim' (PrimWord8 Proxy)) y
  ret (LiteralInt 0)
toPrimFunctionBody Encode (PrimDouble _) = runFunWriter $ do
  err <- unsafeNewNamed "err" (CTypeBackdoor "CborError") (cborEncodeDouble (encoder, value))
  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["encode: error encoding double (" :<< value :<< "): " :<< err]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
toPrimFunctionBody Encode (PrimFloat _) = runFunWriter $ do
  err <- unsafeNewNamed "err" (CTypeBackdoor "CborError") (cborEncodeFloat (encoder, value))
  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["encode: error encoding float (" :<< value :<< "): " :<< err]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
toPrimFunctionBody Encode (PrimBool _) = runFunWriter $ do
  err <- unsafeNewNamed "err" (CTypeBackdoor "CborError") (cborEncodeBoolean (encoder, value))
  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["encode: error encoding bool (" :<< value :<< "): " :<< err]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
toPrimFunctionBody Encode (PrimInt64 _) = runFunWriter $ do
  err <- unsafeNewNamed "err" (CTypeBackdoor "CborError") (cborEncodeInt (encoder, value))
  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["encode: error encoding int (" :<< value :<< "): " :<< err]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
-- TODO(greg): this version doesn't have a static_cast
toPrimFunctionBody Encode (PrimInt32 _) = toPrimFunctionBody Encode (PrimInt64 Proxy)
-- TODO(greg): this version doesn't have a static_cast
toPrimFunctionBody Encode (PrimInt16 _) = toPrimFunctionBody Encode (PrimInt64 Proxy)
-- TODO(greg): this version doesn't have a static_cast
toPrimFunctionBody Encode (PrimInt8 _) = toPrimFunctionBody Encode (PrimInt64 Proxy)
toPrimFunctionBody Encode (PrimWord64 _) = runFunWriter $ do
  err <- unsafeNewNamed "err" (CTypeBackdoor "CborError") (cborEncodeUint (encoder, value))
  if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["encode: error encoding uint64_t (" :<< value :<< "): " :<< err]
    ret (LiteralInt 1)
  ret (LiteralInt 0)
-- TODO(greg): this version doesn't have a static_cast
toPrimFunctionBody Encode (PrimWord32 _) = toPrimFunctionBody Encode (PrimWord64 Proxy)
-- TODO(greg): this version doesn't have a static_cast
toPrimFunctionBody Encode (PrimWord16 _) = toPrimFunctionBody Encode (PrimWord64 Proxy)
-- TODO(greg): this version doesn't have a static_cast
toPrimFunctionBody Encode (PrimWord8 _) = toPrimFunctionBody Encode (PrimWord64 Proxy)

toVectorFunctionBody :: DecodeOrEncode -> (T.Text, T.Text) -> [TapeElement]
toVectorFunctionBody Decode (elemType, Identifier -> elemFunName) = runFunWriter $ do
  force_ $ value :-> #clear #! []
  comment "make sure it's an array and get the length"
  if_ (Not (cborValueIsArray it)) $ do
    kittyWarning ["error decoding vector: type not an array"]
    ret (LiteralInt 1)
  length_ <- newDefaultNamed "length" (CTypeBackdoor "size_t")
  if_ (cborValueGetArrayLength (it, TakeAddress length_)) $ do
    kittyWarning ["error getting array length"]
    ret (LiteralInt 1)
  recursed <- newDefaultNamed "recursed" (CTypeBackdoor "CborValue")
  if_ (cborValueEnterContainer (it, TakeAddress recursed)) $ do
    kittyWarning ["error decoding vector: error entering container"]
    ret (LiteralInt 1)
  comment "fields"
  forLoop
    (unsafeNewNamed "k" (CTypeBackdoor "int") (LiteralInt 0))
    (\_k -> Not (cborValueAtEnd (TakeAddress recursed)))
    increment
    $ \k -> do
      debugDecode $ "getting vector field #" :<< k
      val <- newDefaultNamed "val" (CTypeBackdoor elemType)
      if_ (elemFunName #! [TakeAddress recursed, TakeAddress val]) $ do
        kittyWarning ["error getting vector element " :<< k]
        ret (LiteralInt 1)
      force_ $ value :-> #push_back #! [val]
  comment "get out of container"
  ifElse_
    (cborValueAtEnd (TakeAddress recursed))
    (debugDecode "vector value at end!")
    ( do
        kittyWarning ["vector value not at end, something went wrong"]
        ret (LiteralInt 1)
    )
  debugDecode "vector got all fields, leaving container..."
  if_ (cborValueLeaveContainer (it, TakeAddress recursed)) $ do
    kittyWarning ["error leaving container"]
    ret (LiteralInt 1)
  debugDecode "vector got all fields, successfully left container\n"
  ret (LiteralInt 0)
toVectorFunctionBody Encode (_elemType, Identifier -> elemFunName) = runFunWriter $ do
  comment "enter container"
  array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
  err <-
    unsafeNewNamed
      "err"
      (CTypeBackdoor "CborError")
      (cborEncoderCreateArray (encoder, TakeAddress array_encoder, value :. #size #! []))
  if_ (err :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error creating array"]
    ret (LiteralInt 1)
  comment "fields"
  loopWithType (CTypeBackdoor "size_t") (value :. #size #! []) $ \k -> do
    debugEncode ("encoding field #" :<< k)
    if_ (elemFunName #! [TakeAddress array_encoder, value ! k]) $ do
      kittyWarning ["error encoding vector element " :<< k]
      ret (LiteralInt 1)
  comment "close container"
  debugEncode "encoded all fields, closing container..."
  err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
  if_ (err :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error encoding closing container"]
    ret (LiteralInt 1)
  debugEncode "encoded all fields, successfully left container\n"
  ret (LiteralInt 0)

toListFunctionBody :: DecodeOrEncode -> (T.Text, T.Text) -> [TapeElement]
toListFunctionBody Decode (elemType, Identifier -> elemFunName) = runFunWriter $ do
  force_ $ value :-> #clear #! []
  comment "make sure it's an array with indefinite length"
  if_ (Not (cborValueIsArray it)) $ do
    kittyWarning ["error decoding list: type not an array"]
    ret (LiteralInt 1)
  recursed <- newDefaultNamed "recursed" (CTypeBackdoor "CborValue")
  if_ (cborValueEnterContainer (it, TakeAddress recursed)) $ do
    kittyWarning ["error decoding list: error entering container"]
    ret (LiteralInt 1)
  comment "fields"
  forLoop
    (unsafeNewNamed "k" (CTypeBackdoor "int") (LiteralInt 0))
    (\_k -> Not (cborValueAtEnd (TakeAddress recursed)))
    increment
    $ \k -> do
      debugDecode $ "getting list element " :<< k
      val <- newDefaultNamed "val" (CTypeBackdoor elemType)
      if_ (elemFunName #! [TakeAddress recursed, TakeAddress val]) $ do
        kittyWarning ["error getting list element " :<< k]
        ret (LiteralInt 1)
      force_ $ value :-> #push_back #! [val]
  comment "get out of container"
  ifElse_ (cborValueAtEnd (TakeAddress recursed)) (debugDecode "list value at end!") $ do
    kittyWarning ["list value not at end, something went wrong"]
    ret (LiteralInt 1)
  debugDecode "list got all fields, leaving container..."
  if_ (cborValueLeaveContainer (it, TakeAddress recursed)) $ do
    kittyWarning ["error leaving container"]
    ret (LiteralInt 1)
  debugDecode "list got all fields, successfully left container\n"
  ret (LiteralInt 0)
toListFunctionBody Encode (_elemType, Identifier -> elemFunName) = runFunWriter $ do
  comment "create and enter container of indefinite length"
  array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
  err <- newDefaultNamed "err" (CTypeBackdoor "CborError")
  ifElse_
    (value :. #size #! [] :== LiteralInt 0)
    (err =: cborEncoderCreateArray (encoder, TakeAddress array_encoder, LiteralInt 0))
    (err =: cborEncoderCreateArray (encoder, TakeAddress array_encoder, cborIndefiniteLength))
  if_ (err :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error creating array"]
    ret (LiteralInt 1)
  comment "fields"
  loopWithType (CTypeBackdoor "size_t") (value :. #size #! []) $ \k -> do
    debugEncode ("encoding field #" :<< k)
    if_ (elemFunName #! [TakeAddress array_encoder, value ! k]) $ do
      kittyWarning ["error encoding vector element " :<< k]
      ret (LiteralInt 1)
  comment "close container"
  debugEncode "encoded all fields, closing container..."
  err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
  if_ (err :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error encoding closing container"]
    ret (LiteralInt 1)
  debugEncode "encoded all fields, successfully left container\n"
  ret (LiteralInt 0)

toMapFunctionBody :: DecodeOrEncode -> (T.Text, T.Text) -> (T.Text, T.Text) -> [TapeElement]
toMapFunctionBody
  Decode
  (CTypeBackdoor -> keyType, Ident . Identifier -> keyFun)
  (CTypeBackdoor -> valueType, Ident . Identifier -> valueFun) =
    runFunWriter $ do
      comment
        [fmt|\
if (cbor_value_to_pretty(stdout, it)) {{
  printf("\\nerror printing pretty\\n");
  return 1;
}}
printf("\\n");
|]
      force_ $ value :-> #clear #! []
      comment "make sure it's an array with correct length (one more than outputs)"
      if_ (Not (cborValueIsMap it)) $ do
        kittyWarning ["type not a map"]
        ret (LiteralInt 1)
      recursed <- newDefaultNamed "recursed" (CTypeBackdoor "CborValue")
      if_ (cborValueEnterContainer (it, TakeAddress recursed)) $ do
        kittyWarning ["(map) error entering container"]
        ret (LiteralInt 1)
      comment "fields"
      -- TODO: k is useless here ;)
      forLoop
        (unsafeNewNamed "k" (CTypeBackdoor "int") (LiteralInt 0))
        (\_k -> Not (cborValueAtEnd (TakeAddress recursed)))
        increment
        $ \_ -> do
          comment "key"
          key <- newDefaultNamed "key" keyType
          if_ (keyFun #! [TakeAddress recursed, TakeAddress key]) $ do
            kittyWarning ["error decoding map key"]
            ret (LiteralInt 1)
          comment "std::cout << \"got map key \"\" << key << \"\"\" << std::endl;"
          comment "value"
          val <- newDefaultNamed "val" valueType
          if_ (valueFun #! [TakeAddress recursed, TakeAddress val]) $ do
            kittyWarning ["error decoding map value"]
            ret (LiteralInt 1)
          comment
            [fmt|\
std::cout << \"got map key \"\" << key << \"\"\" << std::endl;
type = cbor_value_get_type(&recursed);
std::cout << \"value type is \" << cbor_type(type) << std::endl;
KITTY_ASSERT_MESSAGE(cbor_value_is_integer(&recursed), \"type not int64_t\");|]
          pair <-
            unsafeNewNamed
              "pair"
              (CTypeBackdoor $ stdPair keyType valueType)
              (Ident (Identifier (stdPair keyType valueType)) #! [key, val])
          force_ $ value :-> #insert #! [pair]
      comment "get out of container"
      ifElse_ (cborValueAtEnd (TakeAddress recursed)) (debugDecode "(map) value at end!\n") $ do
        kittyWarning ["(map) value not at end, something went wrong"]
        ret (LiteralInt 1)
      debugDecode "got all map fields, leaving container...\n"
      if_ (cborValueLeaveContainer (it, TakeAddress recursed)) $ do
        kittyWarning ["(map) error leaving container"]
        ret (LiteralInt 1)
      debugDecode "map got all fields, successfully left container\n"
      ret (LiteralInt 0)
toMapFunctionBody
  Encode
  (keyType, Ident . Identifier -> keyFun)
  (valueType, Identifier -> valueFun) =
    runFunWriter $ do
      comment "enter container"
      array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
      err <-
        unsafeNewNamed
          "err"
          (CTypeBackdoor "CborError")
          (cborEncoderCreateMap (encoder, TakeAddress array_encoder, value :. #size #! []))
      comment "CborError err = cbor_encoder_create_array(encoder, &array_encoder, value.size());"
      if_ (err :&& (err :!= cborErrorOutOfMemory)) $ do
        kittyWarning ["error creating map"]
        ret (LiteralInt 1)
      comment "fields"
      let it_t = CTypeBackdoor [fmt|typename std::map<{keyType}, {valueType}>::const_iterator|]
      it_ <- unsafeNewNamed "it" it_t (value :. #begin #! [])
      forLoop
        (unsafeNewNamed "k" (CTypeBackdoor "int") (LiteralInt 0))
        (\_k -> it :!= (value :. #end #! []))
        (\k -> increment it_ >> increment k)
        $ \k -> do
          debugEncode ("encoding map key " :<< k)
          if_ (keyFun #! [TakeAddress array_encoder, it_ :-> #first]) $ do
            kittyWarning ["error encoding map key " :<< k]
            ret (LiteralInt 1)
          debugEncode "encoding map value"
          if_ (valueFun #! [TakeAddress array_encoder, it_ :-> #second]) $ do
            kittyWarning ["error encoding map value"]
            ret (LiteralInt 1)
      comment "close container"
      debugEncode "encoded all fields, closing container..."
      err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
      if_ (err :&& (err :!= cborErrorOutOfMemory)) $ do
        kittyWarning ["error encoding closing container"]
        ret (LiteralInt 1)
      debugEncode "encoded all fields, successfully left container\n"
      ret (LiteralInt 0)

-- toPrimFunctionBody Encode _cenum = [fmt|\
-- comment "create container"
-- array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
-- err <- unsafeNewNamed "err" (CTypeBackdoor "CborError")
--    (cborEncoderCreateArray(encoder, TakeAddress array_encoder, 1))
-- if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
--   kittyWarning ["error creating array"]
--   ret (LiteralInt 1)
--
-- comment "tag"
-- const uint64_t tag = static_cast<uint64_t>(value)
-- err =: cborEncodeUint(TakeAddress array_encoder, tag)
-- if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
--   kittyWarning ["error getting tag"]
--   ret (LiteralInt 1)
--
-- comment "leave container"
-- err =: cborEncoderCloseContainerChecked(encoder, TakeAddress array_encoder)
-- if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
--   kittyWarning ["error closing container"]
--   ret (LiteralInt 1)
--
-- ret (LiteralInt 0)

-- | ]
toCxxTupleFunctionBody :: DecodeOrEncode -> [T.Text] -> [TapeElement]
toCxxTupleFunctionBody _ [] = error "tuple with 0 elements"
toCxxTupleFunctionBody _ [_] = error "tuple with 1 element"
toCxxTupleFunctionBody Decode tupleFunctions = runFunWriter $ do
  comment [fmt|make sure it's an array with length {n}|]
  if_ (Not (cborValueIsArray it)) $ do
    kittyWarning ["tuple type not an array"]
    ret (LiteralInt 1)
  length_ <- newDefaultNamed "length" (CTypeBackdoor "size_t")
  if_ (cborValueGetArrayLength (it, TakeAddress length_)) $ do
    kittyWarning ["error getting array length"]
    ret (LiteralInt 1)
  if_ (length_ :!= (LiteralInt . fromIntegral $ n)) $ do
    kittyWarning
      ["tuple length mismatch" :<< ", cbor length " :<< length_ :<< [fmt|, expected {n}|]]
    ifElse_ (cborValueToPretty (stdout, it)) (printf "\nerror printing pretty\n") (printf "\n")
    ret (LiteralInt 1)
  recursed <- newDefaultNamed "recursed" (CTypeBackdoor "CborValue")
  if_ (cborValueEnterContainer (it, TakeAddress recursed)) $ do
    kittyWarning ["tuple error entering container"]
    ret (LiteralInt 1)
  fieldDecodes recursed
  comment "get out of container"
  ifElse_ (cborValueAtEnd (TakeAddress recursed)) (debugDecode "tuple value at end!\n") $ do
    kittyWarning ["tuple value not at end, something went wrong"]
    ret (LiteralInt 1)
  debugDecode "tuple got all fields, leaving container...\n"
  if_ (cborValueLeaveContainer (it, TakeAddress recursed)) $ do
    kittyWarning ["error leaving container"]
    ret (LiteralInt 1)
  debugDecode "tuple got all fields, successfully left container\n"
  ret (LiteralInt 0)
  where
    n = length tupleFunctions
    fieldDecodes :: CExpr -> FunWriter ()
    fieldDecodes recursed = case tupleFunctions of
      [Identifier -> f0, Identifier -> f1] -> do
        comment "first"
        if_ (f0 #! [TakeAddress recursed, TakeAddress (value :-> #first)]) $ do
          kittyWarning ["error decoding pair first"]
          ret (LiteralInt 1)
        comment "second"
        if_ (f1 #! [TakeAddress recursed, TakeAddress (value :-> #second)]) $ do
          kittyWarning ["error decoding pair second"]
          ret (LiteralInt 1)
      _ ->
        let toDecode :: Int -> T.Text -> FunWriter ()
            toDecode k (Identifier -> f) = do
              comment "first"
              if_ (f #! [TakeAddress recursed, TakeAddress (StdGet k #! [dereference value])]) $ do
                kittyWarning [[fmt|error decoding tuple element {k}|]]
                ret (LiteralInt 1)
         in zipWithM_ toDecode [0 ..] tupleFunctions
toCxxTupleFunctionBody Encode tupleFunctions = runFunWriter $ do
  comment "enter container"
  array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
  err <-
    unsafeNewNamed
      "err"
      (CTypeBackdoor "CborError")
      (cborEncoderCreateArray (encoder, TakeAddress array_encoder, n))
  if_ (err :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error creating array"]
    ret (LiteralInt 1)
  comment "fields"
  encodeFields array_encoder
  comment "close container"
  debugEncode "encoded all fields, closing container..."
  err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
  if_ (err :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error encoding closing container"]
    ret (LiteralInt 1)
  debugEncode "encoded all fields, successfully left container\n"
  ret (LiteralInt 0)
  where
    n = LiteralInt . fromIntegral $ length tupleFunctions
    encodeFields :: CExpr -> FunWriter ()
    encodeFields array_encoder = case tupleFunctions of
      [Identifier -> f0, Identifier -> f1] -> do
        debugEncode "encoding first"
        if_ (f0 #! [TakeAddress array_encoder, value :. #first]) $ do
          kittyWarning ["error encoding pair first element"]
          ret (LiteralInt 1)
        debugEncode "encoding second"
        if_ (f1 #! [TakeAddress array_encoder, value :. #second]) $ do
          kittyWarning ["error encoding pair second element"]
          ret (LiteralInt 1)
      _ ->
        let toEncode :: Int -> T.Text -> FunWriter ()
            toEncode k (Identifier -> f) = do
              debugEncode "encoding tuple element {k}"
              if_ (f #! [TakeAddress array_encoder, StdGet k #! [value]]) $ do
                kittyWarning ["error encoding tuple element {k}"]
                ret (LiteralInt 1)
         in zipWithM_ toEncode [0 ..] tupleFunctions

toCTupleFunctionBody :: DecodeOrEncode -> [T.Text] -> [TapeElement]
toCTupleFunctionBody _ [] = error "tuple with 0 elements"
toCTupleFunctionBody _ [_] = error "tuple with 1 element"
toCTupleFunctionBody Decode tupleFunctions = runFunWriter $ do
  comment [fmt|make sure it's an array with length {n}|]
  if_ (Not (cborValueIsArray it)) $ do
    kittyWarning ["tuple type not an array"]
    ret (LiteralInt 1)
  length_ <- newDefaultNamed "length" (CTypeBackdoor "size_t")
  if_ (cborValueGetArrayLength (it, TakeAddress length_)) $ do
    kittyWarning ["error getting array length"]
    ret (LiteralInt 1)
  if_ (length_ :!= (LiteralInt . fromIntegral $ n)) $ do
    kittyWarning
      ["tuple length mismatch" :<< ", cbor length " :<< length_ :<< [fmt|, expected {n}|]]
    ifElse_ (cborValueToPretty (stdout, it)) (printf "\nerror printing pretty\n") (printf "\n")
    ret (LiteralInt 1)
  recursed <- newDefaultNamed "recursed" (CTypeBackdoor "CborValue")
  if_ (cborValueEnterContainer (it, TakeAddress recursed)) $ do
    kittyWarning ["tuple error entering container"]
    ret (LiteralInt 1)
  fieldDecodes recursed
  comment "get out of container"
  ifElse_ (cborValueAtEnd (TakeAddress recursed)) (debugDecode "tuple value at end!\n") $ do
    kittyWarning ["tuple value not at end, something went wrong"]
    ret (LiteralInt 1)
  debugDecode "tuple got all fields, leaving container...\n"
  if_ (cborValueLeaveContainer (it, TakeAddress recursed)) $ do
    kittyWarning ["error leaving container"]
    ret (LiteralInt 1)
  debugDecode "tuple got all fields, successfully left container\n"
  ret (LiteralInt 0)
  where
    n = length tupleFunctions
    fieldDecodes :: CExpr -> FunWriter ()
    fieldDecodes recursed =
      let toDecode :: (Int, RfName, T.Text) -> FunWriter ()
          toDecode (k, fieldName, Identifier -> f) = do
            comment "first"
            if_ (f #! [TakeAddress recursed, TakeAddress (dereference value :. fieldName)]) $ do
              kittyWarning [[fmt|error decoding tuple element {k}|]]
              ret (LiteralInt 1)
       in traverse_ toDecode (zip3 [0 ..] R.infiniteTupleFieldNames tupleFunctions)
toCTupleFunctionBody Encode tupleFunctions = runFunWriter $ do
  comment "enter container"
  array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
  err <-
    unsafeNewNamed
      "err"
      (CTypeBackdoor "CborError")
      (cborEncoderCreateArray (encoder, TakeAddress array_encoder, n))
  if_ (err :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error creating array"]
    ret (LiteralInt 1)
  comment "fields"
  encodeFields array_encoder
  comment "close container"
  debugEncode "encoded all fields, closing container..."
  err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
  if_ (err :&& (err :!= cborErrorOutOfMemory)) $ do
    kittyWarning ["error encoding closing container"]
    ret (LiteralInt 1)
  debugEncode "encoded all fields, successfully left container\n"
  ret (LiteralInt 0)
  where
    n = LiteralInt . fromIntegral $ length tupleFunctions
    encodeFields :: CExpr -> FunWriter ()
    encodeFields array_encoder =
      let toEncode :: (Int, RfName, T.Text) -> FunWriter ()
          toEncode (k, fieldName, Identifier -> f) = do
            debugEncode [fmt|encoding tuple element {k}|]
            if_ (f #! [TakeAddress array_encoder, value :. fieldName]) $ do
              kittyWarning [LiteralString [fmt|error encoding tuple element {k}|]]
              ret (LiteralInt 1)
       in traverse_ toEncode (zip3 [0 ..] R.infiniteTupleFieldNames tupleFunctions)

toCMaybeSerialiseBody ::
  DecodeOrEncode ->
  CUnion Proxy ->
  (CUnionCon Proxy, CUnionCon Proxy) ->
  CType Proxy ->
  CFunction ->
  [TapeElement]
toCMaybeSerialiseBody doe cunion (CUnionCon _ nothing, CUnionCon _ just) elemType elemFun =
  let elemTypeName = R.renderCType elemType
      -- nothingTypeName = R.renderCUnionConType nothing
      -- justTypeName = R.renderCUnionConType just
      elemFunName = cfName elemFun
   in runFunWriter $ case doe of
        Decode -> do
          debugDecode [fmt|decoding Maybe {elemTypeName}|]
          comment "enter container"
          if_ (Not (cborValueIsArray it)) $ do
            kittyWarning ["type not array, it's " :<< cborValueGetType it]
            ret (LiteralInt 1)
          length_ <- newDefaultNamed "length" (CTypeBackdoor "size_t")
          if_ (cborValueGetArrayLength (it, TakeAddress length_) :!= LiteralInt 0) $ do
            kittyWarning ["error getting array length"]
            ret (LiteralInt 1)
          recursed <- newDefaultNamed "recursed" (CTypeBackdoor "CborValue")
          if_ (cborValueEnterContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
            kittyWarning ["error entering container"]
            ret (LiteralInt 1)
          ifElse_
            (length_ :== LiteralInt 0)
            ( value :-> makeRfName (R.renderCUnionTagMember cunion)
                =: Ident
                  (Identifier $ R.renderCUnionTagLiteral cunion nothing)
            )
            ( ifElse_
                (length_ :== LiteralInt 1)
                ( do
                    value :-> makeRfName (R.renderCUnionTagMember cunion)
                      =: Ident
                        (Identifier $ R.renderCUnionTagLiteral cunion just)
                    comment
                      "don't use decode<a> because that breaks for decode<T1, T2> like in std::pair"
                    if_
                      ( elemFunName
                          #! [ TakeAddress recursed,
                               TakeAddress $
                                 value
                                   :-> makeRfName (R.renderCUnionMemberName just)
                                   :. #unnamed_field_0
                             ]
                          :!= LiteralInt 0
                      )
                      $ do
                        kittyWarning ["error decoding Just"]
                        ret (LiteralInt 1)
                )
                ( kittyWarning
                    [ "error decoding Maybe<a>: array length should be 0 or 1 but it is "
                        :<< length_
                    ]
                )
            )
          comment "leave container"
          if_ (cborValueLeaveContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
            kittyWarning ["error leaving container"]
            ret (LiteralInt 1)
          ret (LiteralInt 0)
        Encode ->
          caseUnionTag
            cunion
            (value :. makeRfName (R.renderCUnionTagMember cunion))
            ( \tagLiteral -> do
                let asJust = makeRfName $ R.renderCUnionMemberName just
                if
                    | tagLiteral == R.renderCUnionTagLiteral cunion nothing -> do
                        comment "constructor 0:"
                        comment "enter container"
                        array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
                        err <-
                          unsafeNewNamed
                            "err"
                            (CTypeBackdoor "CborError")
                            ( cborEncoderCreateArray
                                (encoder, TakeAddress array_encoder, LiteralInt 0)
                            )
                        if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
                          kittyWarning ["error creating array"]
                          ret (LiteralInt 1)
                        comment "close container"
                        err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
                        if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
                          kittyWarning ["error closing container"]
                          ret (LiteralInt 1)
                        ret (LiteralInt 0)
                    | tagLiteral == R.renderCUnionTagLiteral cunion just -> do
                        comment "constructor 1:"
                        comment "enter container"
                        array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
                        err <-
                          unsafeNewNamed
                            "err"
                            (CTypeBackdoor "CborError")
                            ( cborEncoderCreateArray
                                (encoder, TakeAddress array_encoder, LiteralInt 1)
                            )
                        if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
                          kittyWarning ["error creating array"]
                          ret (LiteralInt 1)
                        if_
                          ( elemFunName
                              #! [TakeAddress array_encoder, value :. asJust :. #unnamed_field_0]
                              :!= LiteralInt 0
                          )
                          $ do
                            kittyWarning ["error encoding MaybeJust<a>"]
                            ret (LiteralInt 1)
                        comment "close container"
                        err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
                        if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
                          kittyWarning ["error closing container"]
                          ret (LiteralInt 1)
                        ret (LiteralInt 0)
                    | otherwise -> error [fmt|not all cases are matched|] -- TODO: this is ugly
            )
            ( do
                kittyWarning ["Maybe hit default case"]
                ret (LiteralInt 1)
            )

toCxxMaybeSerialiseBody ::
  DecodeOrEncode ->
  (CxxUnionCon Proxy, CxxUnionCon Proxy) ->
  CxxType Proxy ->
  CFunction ->
  [TapeElement]
toCxxMaybeSerialiseBody doe (nothing, just) elemType elemFun =
  let elemTypeName = R.renderCxxType elemType
      elemFunName = Ident $ cfName elemFun
   in case doe of
        Decode -> runFunWriter $ do
          debugDecode [fmt|decoding Maybe {elemTypeName}|]
          comment "enter container"
          if_ (Not (cborValueIsArray it)) $ do
            kittyWarning ["type not array, it's " :<< cborValueGetType it]
            ret (LiteralInt 1)
          length_ <- newDefaultNamed "length" (CTypeBackdoor "size_t")
          if_ (cborValueGetArrayLength (it, TakeAddress length_) :!= LiteralInt 0) $ do
            kittyWarning ["error getting array length"]
            ret (LiteralInt 1)
          recursed <- newDefaultNamed "recursed" (CTypeBackdoor "CborValue")
          if_ (cborValueEnterContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
            kittyWarning ["error entering container"]
            ret (LiteralInt 1)
          ifElse_
            (length_ :== LiteralInt 0)
            ( do
                aNothing <-
                  newDefaultCxxNamed "nothing" $ CTypeBackdoor $ R.renderCxxUnionConType nothing
                value :-> #payload =: aNothing
            )
            ( ifElse_
                (length_ :== LiteralInt 1)
                ( do
                    aJust <- newDefaultNamed "just" (CTypeBackdoor $ R.renderCxxUnionConType just)
                    comment
                      [fmt|don't use decode<a> because that breaks for decode<T1, T2> \
like in std::pair|]
                    if_
                      ( elemFunName
                          #! [ TakeAddress recursed,
                               TakeAddress (Ident (Identifier "just") :. #unnamed_field_0)
                             ]
                          :!= LiteralInt 0
                      )
                      $ do
                        kittyWarning ["error decoding Just"]
                        ret (LiteralInt 1)
                    value :-> #payload =: aJust
                )
                ( kittyWarning
                    [ "error decoding Maybe<a>: array length should be 0 or 1 but it is "
                        :<< length_
                    ]
                )
            )
          comment "leave container"
          if_ (cborValueLeaveContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
            kittyWarning ["error leaving container"]
            ret (LiteralInt 1)
          ret (LiteralInt 0)
        Encode ->
          runFunWriter . ret $
            value
              :. #payload
              :. #match
              #! [
                   -- TODO: const reference for arguments
                   cxxLambda [unsafeAsIdentifier encoder] [nothing] $ \(_ :: CExpr) -> do
                     comment "constructor 0:"
                     comment "enter container"
                     array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
                     err <-
                       unsafeNewNamed
                         "err"
                         (CTypeBackdoor "CborError")
                         (cborEncoderCreateArray (encoder, TakeAddress array_encoder, LiteralInt 0))
                     if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
                       kittyWarning ["error creating array"]
                       ret (LiteralInt 1)
                     comment "close container"
                     err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
                     if_ (err :!= LiteralInt 0 :&& (err :!= cborErrorOutOfMemory)) $ do
                       kittyWarning ["error closing container"]
                       ret (LiteralInt 1)
                     ret (LiteralInt 0),
                   -- TODO: const reference for arguments
                   cxxLambda [unsafeAsIdentifier encoder] [just] $ \aJust -> do
                     comment "constructor 1:"
                     comment "\" ++ show con1"
                     comment "enter container"
                     array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
                     err <-
                       unsafeNewNamed
                         "err"
                         (CTypeBackdoor "CborError")
                         (cborEncoderCreateArray (encoder, TakeAddress array_encoder, LiteralInt 1))
                     if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
                       kittyWarning ["error creating array"]
                       ret (LiteralInt 1)
                     if_
                       ( elemFunName
                           #! [TakeAddress array_encoder, aJust :. #unnamed_field_0]
                           :!= LiteralInt 0
                       )
                       $ do
                         kittyWarning ["error encoding MaybeJust<a>"]
                         ret (LiteralInt 1)
                     comment "close container"
                     err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
                     if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
                       kittyWarning ["error closing container"]
                       ret (LiteralInt 1)
                     ret (LiteralInt 0)
                 ]

toCxxTreeSerialiseBody :: DecodeOrEncode -> T.Text -> [TapeElement]
toCxxTreeSerialiseBody doe conFieldsFunName =
  let nfields = LiteralInt 2
      fieldsFun = Ident (Identifier conFieldsFunName)
   in case doe of
        Decode -> runFunWriter $ do
          debugDecode [fmt|decoding Tree associated with field function {conFieldsFunName}|]
          comment "enter container"
          if_ (Not (cborValueIsArray it)) $ do
            kittyWarning ["type not array, it's " :<< cborValueGetType it]
            ret (LiteralInt 1)
          length_ <- newDefaultNamed "length" (CTypeBackdoor "size_t")
          if_ (cborValueGetArrayLength (it, TakeAddress length_) :!= LiteralInt 0) $ do
            kittyWarning ["error getting array length"]
            ret (LiteralInt 1)
          if_ (length_ :!= nfields) $ do
            kittyWarning ["Tree should have nfields fields but it has " :<< length_]
            ret (LiteralInt 1)
          recursed <- newDefaultNamed "recursed" (CTypeBackdoor "CborValue")
          if_ (cborValueEnterContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
            kittyWarning ["error entering container"]
            ret (LiteralInt 1)
          comment "KITTY_WARNING(\"type is \" :<< cbor_value_get_type(&recursed));"
          if_ (fieldsFun #! [TakeAddress recursed, value, nfields] :!= LiteralInt 0) $ do
            kittyWarning ["error decoding tree fields"]
            ret (LiteralInt 1)
          comment "leave container"
          if_ (cborValueLeaveContainer (it, TakeAddress recursed) :!= LiteralInt 0) $ do
            kittyWarning ["error leaving container"]
            ret (LiteralInt 1)
          ret (LiteralInt 0)
        Encode -> runFunWriter $ do
          comment "enter container"
          array_encoder <- newDefaultNamed "array_encoder" (CTypeBackdoor "CborEncoder")
          err <-
            unsafeNewNamed
              "err"
              (CTypeBackdoor "CborError")
              (cborEncoderCreateArray (encoder, TakeAddress array_encoder, nfields))
          if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
            kittyWarning ["error creating array"]
            ret (LiteralInt 1)
          if_ (fieldsFun #! [TakeAddress array_encoder, value, nfields] :!= LiteralInt 0) $ do
            kittyWarning ["error encoding tree fields"]
            ret (LiteralInt 1)
          comment "close container"
          err =: cborEncoderCloseContainerChecked (encoder, TakeAddress array_encoder)
          if_ ((err :!= LiteralInt 0) :&& (err :!= cborErrorOutOfMemory)) $ do
            kittyWarning ["error closing container"]
            ret (LiteralInt 1)
          ret (LiteralInt 0)
