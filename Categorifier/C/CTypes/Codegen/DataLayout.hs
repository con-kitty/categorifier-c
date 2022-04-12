{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

module Categorifier.C.CTypes.Codegen.DataLayout
  ( toDataLayoutModule,
    toDataLayoutFunctionName, -- for TryDataLayouts
  )
where

import Categorifier.C.CTypes.Codegen.Helpers (CStructOrUnion (..))
import Categorifier.C.CTypes.Codegen.Render.Utils
  ( staticAssert,
    staticAssertTriviallyCopyable,
    staticAssertTriviallyCopyable',
  )
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
    SystemLib (Def),
    staticCast,
    (!),
    (#!),
  )
import Categorifier.C.CTypes.DSL.FunctionWriter
  ( FunWriter,
    cexprFunctionFromParams,
    comment,
    if_,
    newDefaultNamed,
    ret,
    runFunWriter,
    (=:),
  )
import qualified Categorifier.C.CTypes.Render as R
import Categorifier.C.CTypes.Types
import Categorifier.C.Prim (allPrimTypes, getPrimCName, getPrimHaskellName)
import Categorifier.C.Recursion (hproject)
import Control.Monad (zipWithM_)
import Data.Foldable (for_)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import PyF (fmt)

toDataLayoutModule :: [CEnum Proxy] -> [CStructOrUnion] -> CxxModule
toDataLayoutModule cenums cstructsOrUnions =
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
      [ IncludeSystemLib Def, -- For offsetof.
        IncludeSystemFile "endian.h",
        IncludeSystemFile "json/json.h",
        IncludeHeavisoftFile "avionics/flight_computer/kitty_protos/type_name.hpp",
        IncludeModule CTypes
      ]
    functions :: [CFunction]
    functions =
      endiannessStringFunction :
      arrayDataLayoutFunction :
      (toPrimDataLayoutFunction <$> allPrimTypes)
        <> (toEnumDataLayoutFunction <$> cenums)
        <> concatMap toStructOrUnionDataLayoutFunction cstructsOrUnions
      where
        toStructOrUnionDataLayoutFunction (CS cstruct) = [toStructDataLayoutFunction cstruct]
        toStructOrUnionDataLayoutFunction (CU cunion) = toUnionDataLayoutFunctions cunion

toDataLayoutFunctionName :: CType f -> Identifier
toDataLayoutFunctionName t@(CTypeStruct _) = Identifier [fmt|DataLayout{R.renderCType t}|]
toDataLayoutFunctionName t@(CTypeEnum _) = Identifier [fmt|DataLayout{R.renderCType t}|]
toDataLayoutFunctionName t@(CTypeUnion _) = Identifier [fmt|DataLayout{R.renderCType t}|]
toDataLayoutFunctionName (CTypePrim prim) = Identifier [fmt|DataLayout{getPrimHaskellName prim}|]
-- For arrays, all calls are the same because they are called recursively.
toDataLayoutFunctionName CTypeArray {} = Identifier [fmt|ArrayDataLayout|]

toDataLayoutFunctionCall :: T.Text -> CType f -> (CExpr, Maybe (FunWriter ()))
toDataLayoutFunctionCall fakeFieldValue t@(CTypeArray nat elemType _) =
  (funCall, Just staticAsserts)
  where
    funCall = funName #! [n, totalSize, elemFunCall]
    funName = toDataLayoutFunctionName t
    (elemFunCall, melemStaticAssert) =
      toDataLayoutFunctionCall [fmt|({fakeFieldValue})[0]|] $ hproject elemType
    n = Ident (Identifier $ R.renderCNat nat)
    totalSize = sizeof fakeFieldValue
    staticAsserts = case melemStaticAssert of
      Nothing -> theStaticAssert
      Just elemStaticAssert -> do
        theStaticAssert
        elemStaticAssert
      where
        -- TODO: this is a nightmare ;)
        -- The c version looks like
        -- staticAssert (n == 0) && sizeof fakeFieldValue == 1 ||
        --    sizeof fakeFieldValue == n * sizeof elemType
        theStaticAssert =
          staticAssert
            ( (n :== LiteralInt 0 :&& sizeof fakeFieldValue :== LiteralInt 1)
                :|| (sizeof fakeFieldValue :== n :* sizeof (R.renderCType $ hproject elemType))
            )
            "array size mismatch"
toDataLayoutFunctionCall _ t = (toDataLayoutFunctionName t #! [], Nothing)

toStructDataLayoutFunction :: CStruct Proxy -> CFunction
toStructDataLayoutFunction cstruct =
  toStructDataLayoutFunctionWithNames
    (R.renderCStructType cstruct)
    (toDataLayoutFunctionName (CTypeStruct cstruct))
    (csCon cstruct)

toStructDataLayoutFunctionWithNames :: T.Text -> Identifier -> CCon Proxy -> CFunction
toStructDataLayoutFunctionWithNames typeName functionName ccon =
  UnsafeCFunction
    { cfName = functionName,
      cfInlineOverloadName = Nothing,
      cfReturnType = Just (ParamExternalType jsonValue),
      cfParams = [],
      cfStaticLinkage = False,
      cfComment = Nothing,
      cfTape = runFunWriter $ do
        comment "make sure it's trivially copyable"
        staticAssertTriviallyCopyable' typeName
        retValue <- newDefaultNamed "ret" (CTypeBackdoor "Json::Value")
        toConDataLayoutBody typeName ccon retValue
        ret retValue
    }

toConDataLayoutBody :: T.Text -> CCon Proxy -> CExpr -> FunWriter ()
toConDataLayoutBody typeName (CNullaryCon _) retValue = do
  retValue ! "Struct" ! "ctype" =: LiteralString typeName
  retValue ! "Struct" ! "size" =: sizeof typeName
  comment "Nullary struct has no fields."
  retValue ! "Struct" ! "fields" =: Ident (Identifier "Json::arrayValue")
toConDataLayoutBody typeName (CNormalCon _ _ fields) retValue = do
  retValue ! "Struct" ! "ctype" =: LiteralString typeName
  retValue ! "Struct" ! "size" =: sizeof typeName
  zipWithM_ setNormalField [0 ..] (NE.toList $ fmap (fmap hproject) fields)
  where
    setNormalField :: Int -> (RfName, CType Proxy) -> FunWriter ()
    setNormalField k (rfName, fieldType) = do
      let maybeFieldStaticAsserts' = case maybeFieldStaticAsserts of
            Nothing -> pure ()
            Just r -> r
          (fieldFunCall, maybeFieldStaticAsserts) =
            toDataLayoutFunctionCall fakeFieldValue fieldType
          fakeFieldValue = [fmt|reinterpret_cast<{typeName}*>(0)->{fieldName}|]
          shownFieldType = R.renderCType fieldType
          fieldName = sanitizedRfName rfName
      retValue ! "Struct" ! "fields" ! LiteralInt (fromIntegral k) ! "name"
        =: LiteralString fieldName
      retValue
        ! "Struct"
        ! "fields"
        ! LiteralInt (fromIntegral k)
        ! "offset"
        =: (Identifier "offsetof" #! [Ident (Identifier typeName), Ident (Identifier fieldName)])
      retValue ! "Struct" ! "fields" ! LiteralInt (fromIntegral k) ! "size" =: sizeof fakeFieldValue
      retValue
        ! "Struct"
        ! "fields"
        ! LiteralInt (fromIntegral k)
        ! "ctype"
        =: [fmt|{shownFieldType}|]
      comment
        [fmt|TODO(greg): O[n] refactor: just add this layout to a set, \
and look it up from the ctype|]
      retValue ! "Struct" ! "fields" ! LiteralInt (fromIntegral k) ! "data_layout" =: fieldFunCall
      maybeFieldStaticAsserts'
toConDataLayoutBody typeName (CBitfieldCon (CBitfield _ fields value)) retValue =
  let underlyingType = getPrimCName $ bfprimToPrim value
      setBitField :: Int -> RfName -> FunWriter ()
      setBitField k fieldName =
        retValue ! "BitfieldStruct" ! "fields" ! LiteralInt (fromIntegral k) ! "name"
          =: LiteralString
            (sanitizedRfName fieldName)
   in do
        retValue ! "BitfieldStruct" ! "ctype" =: [fmt|{typeName}|]
        retValue ! "BitfieldStruct" ! "size" =: sizeof typeName
        retValue ! "BitfieldStruct" ! "underlying_type" =: [fmt|{underlyingType}|]
        zipWithM_ setBitField [0 ..] (NE.toList fields)

toUnionDataLayoutFunctions :: CUnion Proxy -> [CFunction]
toUnionDataLayoutFunctions cunion = conFunctions <> [unionFunction]
  where
    unionFunction =
      UnsafeCFunction
        { cfName = toDataLayoutFunctionName (CTypeUnion cunion),
          cfInlineOverloadName = Nothing,
          cfReturnType = Just (ParamExternalType jsonValue),
          cfParams = [],
          cfStaticLinkage = False,
          cfComment = Nothing,
          cfTape = runFunWriter $ do
            comment "make sure it's trivially copyable"
            staticAssertTriviallyCopyable (CTypeUnion cunion)
            retValue <- newDefaultNamed "ret" (CTypeBackdoor "Json::Value")
            retValue ! "Union" ! "ctype" =: LiteralString typeName
            retValue ! "Union" ! "size" =: sizeof typeName
            comment "Union tags"
            retValue ! "Union" ! "tag_type" =: [fmt|{R.renderCUnionTagType cunion}|]
            retValue ! "Union" ! "tag_member" =: [fmt|{R.renderCUnionTagMember cunion}|]
            setTags retValue
            comment "Union members"
            setUnionMembers retValue
            ret retValue
        }
    conFunctions :: [CFunction]
    conFunctions = fmap toConFunction (V.toList (cuCons cunion))
    toConFunction :: CCon Proxy -> CFunction
    toConFunction memberCon =
      (toStructDataLayoutFunctionWithNames conTypeName conFunName memberCon)
        { cfStaticLinkage = True
        }
      where
        conTypeName = R.renderCUnionConType (CUnionCon cunion memberCon)
        conFunName = Identifier $ "DataLayout" <> conTypeName
    typeName :: T.Text
    typeName = R.renderCUnionType cunion
    setTags retValue = zipWithM_ toSetTag [0 ..] (V.toList (cuCons cunion))
      where
        toSetTag :: Int -> CCon Proxy -> FunWriter ()
        toSetTag k tagCon = do
          let enumConName = R.renderCUnionTagLiteral cunion tagCon
          retValue ! "Union" ! "tags" ! LiteralInt (fromIntegral k) =: LiteralString enumConName
    setUnionMembers retValue = zipWithM_ toSetCon [0 ..] conFunctions
      where
        toSetCon :: Int -> CFunction -> FunWriter ()
        toSetCon k conFun =
          retValue ! "Union" ! "cons" ! LiteralInt (fromIntegral k) =: (conFunName #! [])
          where
            conFunName = cfName conFun

toEnumDataLayoutFunction :: CEnum Proxy -> CFunction
toEnumDataLayoutFunction cenum =
  UnsafeCFunction
    { cfName = functionName,
      cfInlineOverloadName = Nothing,
      cfReturnType = Just (ParamExternalType jsonValue),
      cfStaticLinkage = False,
      cfComment = Nothing,
      cfParams = [],
      cfTape = runFunWriter $ do
        comment "make sure it's trivially copyable"
        staticAssertTriviallyCopyable (CTypeEnum cenum)
        retValue <- newDefaultNamed "ret" (CTypeBackdoor "Json::Value")
        retValue ! "Enum" ! "ctype" =: LiteralString typeName
        retValue ! "Enum" ! "system_endianness" =: (Identifier "GetEndiannessString" #! [])
        retValue
          ! "Enum"
          ! "underlying_type"
          =: ( Identifier
                 [fmt|CxxAbiTypeNameWithSubs<typename std::underlying_type<{typeName}>::type>|]
                 #! []
             )
        retValue ! "Enum" ! "size" =: sizeof typeName
        for_ (allEnumCons cenum) $ \cenum' -> do
          let DcName conName = enumDcName cenum'
              k = runIdentity $ ceData cenum'
          retValue ! "Enum" ! "fields" ! LiteralInt (fromIntegral k) ! "name"
            =: LiteralString conName
          retValue ! "Enum" ! "fields" ! LiteralInt (fromIntegral k) ! "value"
            =: staticCast
              (CTypePrim' (PrimInt32 Proxy))
              (Ident (Identifier (R.renderEnumConLiteral cenum')))
        ret retValue
    }
  where
    functionName = toDataLayoutFunctionName (CTypeEnum cenum)
    typeName :: T.Text
    typeName = R.renderCEnumType cenum

toPrimDataLayoutFunction :: Prim Proxy -> CFunction
toPrimDataLayoutFunction cprim =
  UnsafeCFunction
    { cfName = functionName,
      cfInlineOverloadName = Nothing,
      cfReturnType = Just (ParamExternalType jsonValue),
      -- If you make it static, it'll complain that Char and Int8 are unused.
      -- If you try to detect which ones are needed, it's different for in C and C++ types.
      -- Can be done but not worth it.
      cfStaticLinkage = False,
      cfComment = Nothing,
      cfParams = [],
      cfTape = runFunWriter $ do
        retValue <- newDefaultNamed "ret" (CTypeBackdoor "Json::Value")
        retValue ! "Prim" ! "ctype" =: LiteralString typeName
        retValue ! "Prim" ! "system_endianness" =: (Ident (Identifier "GetEndiannessString") #! [])
        retValue ! "Prim" ! "size" =: sizeof typeName
        ret retValue
    }
  where
    functionName = toDataLayoutFunctionName (CTypePrim cprim)
    typeName :: T.Text
    typeName = getPrimCName cprim

jsonValue :: ExternalType
jsonValue = ExternalType "Json::Value"

arrayDataLayoutFunction :: CFunction
arrayDataLayoutFunction =
  UnsafeCFunction
    { cfName = functionName,
      cfInlineOverloadName = Nothing,
      cfComment = Just (Comment "Data layout helper for array."),
      cfStaticLinkage = True,
      cfReturnType = Just (ParamExternalType jsonValue),
      cfParams = inputs,
      cfTape = cexprFunctionFromParams inputs $ \num_elements total_size element_data_layout -> do
        root <- newDefaultNamed "root" (CTypeBackdoor "Json::Value")
        root ! "Array" ! "length" =: num_elements
        root ! "Array" ! "total_size" =: total_size
        root ! "Array" ! "element" =: (Identifier "std::move" #! [element_data_layout])
        ret root
    }
  where
    functionName = Identifier "ArrayDataLayout"
    inputs =
      [ Param
          { pType = ParamCxxType (CxxTypeCType (CTypePrim (PrimWord64 Proxy))),
            pId = Identifier "num_elements",
            pUnused = False,
            pMutable = False
          },
        Param
          { pType = ParamCxxType (CxxTypeCType (CTypePrim (PrimWord64 Proxy))),
            pId = Identifier "total_size",
            pUnused = False,
            pMutable = False
          },
        Param
          { pType = ParamExternalType jsonValue,
            pId = Identifier "element_data_layout",
            pUnused = False,
            pMutable = False
          }
      ]

endiannessStringFunction :: CFunction
endiannessStringFunction =
  UnsafeCFunction
    { cfName = Identifier "GetEndiannessString",
      cfInlineOverloadName = Nothing,
      cfComment = Just (Comment [fmt|Return "be" or "le".|]),
      cfStaticLinkage = True,
      cfReturnType = Just (ParamExternalType (ExternalType "std::string")),
      cfParams = [],
      cfTape = runFunWriter $ do
        if_ (Identifier "htobe32" #! [LiteralInt 47] :== LiteralInt 47) $ ret "be"
        ret "le"
    }

-- | A wrapper for C @sizeof@ expression on a Text identifier
-- TODO: This should not work on 'T.Text'. Instead it should work on 'CType'
sizeof :: T.Text -> CExpr
sizeof t = Identifier "sizeof" #! [Ident (Identifier t)]
