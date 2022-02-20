{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Functions for rendering C/C++ types and data.
module Kitty.CTypes.Render
  ( renderCxxType,
    renderCType,
    renderCNat,

    -- * Prim
    renderCxxPrimType,

    -- * Structs
    renderCxxStructType,
    renderCStructType,

    -- * Tuple
    infiniteTupleFieldNames,

    -- * Unions
    renderCxxUnionType,
    renderCUnionType,
    renderCUnionTagType,
    renderCUnionTagMember,
    renderCUnionConType,
    renderCUnionMemberName,
    renderCxxUnionConType,
    renderCUnionTagLiteral,

    -- * Enums
    renderCEnumType,
    renderEnumConLiteral,
    renderEnumNumFieldsLiteralWithTypeName,
    renderEnumForceSignedLiteralWithTypeName,

    -- * Data
    renderSetCxxData,
    renderSetCData,
  )
where

import qualified Data.ByteString as BS
import Data.Functor.Identity (Identity (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Kitty.CTypes.KTypeRep (ctypeNameFromKTypeRep)
import Kitty.CTypes.Types
  ( CCon,
    CConF (..),
    CEnum (..),
    CNat (..),
    CStruct,
    CStructF (..),
    CType,
    CTypeF (..),
    CUnion,
    CUnionConF (..),
    CUnionF (..),
    CxxCon (..),
    CxxOrCCon (..),
    CxxPrim (..),
    CxxStruct (..),
    CxxType (..),
    CxxUnion (..),
    CxxUnionCon (..),
    DcName (..),
    Prim (..),
    RfName,
    cconName,
    cxxOrCConName,
    enumDcName,
    fromBitfieldPrim',
    makeRfName,
    sanitizedRfName,
  )
import Kitty.Prim (getPrimCName)
import Kitty.Recursion (hproject)
import PyF (fmt)
import qualified Text.Casing as Casing

renderCEnumType :: CEnum f -> T.Text
renderCEnumType = ctypeNameFromKTypeRep . ceTypeRep

renderEnumForceSignedLiteralWithTypeName :: T.Text -> T.Text
renderEnumForceSignedLiteralWithTypeName typeName = [fmt|kForceSigned{typeName}|]

renderEnumNumFieldsLiteralWithTypeName :: T.Text -> T.Text
renderEnumNumFieldsLiteralWithTypeName typeName = [fmt|kNumFields{typeName}|]

renderEnumConLiteral :: CEnum Identity -> T.Text
renderEnumConLiteral cenum =
  renderEnumConLiteralWithTypeName (renderCEnumType cenum) (enumDcName cenum)

renderEnumConLiteralWithTypeName :: T.Text -> DcName -> T.Text
renderEnumConLiteralWithTypeName typeName con = [fmt|k{typeName}{conName}|]
  where
    conName = unDcName con

-- If we were going to do C++11 enum classes, it would be:
--
--   renderEnumConLiteral typeName con = [fmt|{typeName}::k{conName}|]
--     where
--       typeName = renderCEnumType cenum
--       conName = unDcName con

renderCStructType :: CStructF l f -> T.Text
renderCStructType = ctypeNameFromKTypeRep . csTypeRep

renderCUnionType :: CUnionF l f -> T.Text
renderCUnionType = ctypeNameFromKTypeRep . cuTypeRep

renderCxxStructType :: CxxStruct f -> T.Text
renderCxxStructType = ctypeNameFromKTypeRep . cxxsTypeRep

renderCxxUnionType :: CxxUnion f -> T.Text
renderCxxUnionType = ctypeNameFromKTypeRep . cxxuTypeRep

renderCxxPrimType :: CxxPrim f -> T.Text
renderCxxPrimType (PrimString _) = "std::string"
renderCxxPrimType (PrimByteString _) = "std::vector<uint8_t>"

renderCxxType :: CxxType f -> T.Text
renderCxxType (CxxTypeCType x) = renderCType x
renderCxxType (CxxTypePrim prim) = renderCxxPrimType prim
renderCxxType (CxxTypeVector _ x _) = [fmt|std::vector<{renderCxxType x}>|]
renderCxxType (CxxTypeArray n x _) = [fmt|std::array<{renderCxxType x}, {renderCNat n}>|]
renderCxxType (CxxTypeTuple [x, y]) = [fmt|std::pair<{renderCxxType x}, {renderCxxType y}>|]
renderCxxType (CxxTypeTuple xs) = [fmt|std::tuple<{T.intercalate ", " (fmap renderCxxType xs)}>|]
renderCxxType (CxxTypeMap x y _) = [fmt|std::map<{renderCxxType x}, {renderCxxType y}>|]
renderCxxType (CxxTypeStruct x) = renderCxxStructType x
renderCxxType (CxxTypeUnion x) = renderCxxUnionType x

renderCNat :: CNat -> T.Text
renderCNat (CNatInt k) = T.pack (show k)
renderCNat (CNatType _ rep) = camelToScreamingSnake ("Num" <> ctypeNameFromKTypeRep rep)
  where
    camelToScreamingSnake = T.pack . Casing.toScreamingSnake . Casing.fromHumps . T.unpack

-- If we ever decide to get rid of the system dimension #defines:
-- renderCNat (CNatType n _)  = T.pack (show n)

renderCType :: CType f -> T.Text
renderCType (CTypePrim prim) = getPrimCName prim
renderCType (CTypeArray n x _) = [fmt|std::array<{renderCType $ hproject x}, {renderCNat n}>|]
renderCType (CTypeEnum x) = renderCEnumType x
renderCType (CTypeStruct x) = renderCStructType x
renderCType (CTypeUnion x) = renderCUnionType x

renderPrimData :: Prim Identity -> T.Text
-- For floating point, the correct way to render a constant is showHFloat in haskell
-- or printf("%a") in C. Unfortunately, while this is perfectly legal C99,
-- it only becomes legal C++ in C++17!
-- So for now just print the value.
-- renderPrimData (PrimFloat  (Identity x)) = [fmt|{Numeric.showHFloat x ""}|]
-- renderPrimData (PrimDouble  (Identity x)) = [fmt|{Numeric.showHFloat x ""}|]51
renderPrimData (PrimFloat (Identity x)) = [fmt|{x}f|]
renderPrimData (PrimDouble (Identity x)) = [fmt|{x}|]
renderPrimData (PrimInt8 (Identity x)) = [fmt|INT8_C({x})|]
renderPrimData (PrimInt16 (Identity x)) = [fmt|INT16_C({x})|]
renderPrimData (PrimInt32 (Identity x)) = [fmt|INT32_C({x})|]
renderPrimData (PrimInt64 (Identity x)) = [fmt|INT64_C({x})|]
renderPrimData (PrimWord8 (Identity x)) = [fmt|UINT8_C({x})|]
renderPrimData (PrimWord16 (Identity x)) = [fmt|UINT16_C({x})|]
renderPrimData (PrimWord32 (Identity x)) = [fmt|UINT32_C({x})|]
renderPrimData (PrimWord64 (Identity x)) = [fmt|UINT64_C({x})|]
renderPrimData (PrimBool (Identity True)) = "true"
renderPrimData (PrimBool (Identity False)) = "false"

renderCxxPrimData :: CxxPrim Identity -> T.Text
renderCxxPrimData (PrimString (Identity xs))
  | T.null xs = [fmt|std::string("")|]
  | otherwise = [fmt|std::string({literalText}, {length bytes})|]
  where
    literalText :: String
    literalText = concatMap (\v -> [fmt|"\\x{v:02x}"|]) bytes
    bytes = BS.unpack (TE.encodeUtf8 xs)
renderCxxPrimData (PrimByteString (Identity x)) =
  "{" <> T.intercalate ", " (T.pack . show <$> bytes) <> "}"
  where
    bytes :: [Word8]
    bytes = BS.unpack x

renderSetCxxData :: T.Text -> CxxType Identity -> T.Text
renderSetCxxData prefix (CxxTypeCType x) = renderSetCData prefix x
renderSetCxxData prefix (CxxTypePrim x) = [fmt|{prefix} = {renderCxxPrimData x};|]
renderSetCxxData prefix (CxxTypeVector _ _ elements) =
  T.intercalate "\n" $
    [fmt|{prefix}.resize({n});|] :
      [ renderSetCxxData [fmt|{prefix}[{k}]|] element
        | (k, element) <- zip [(0 :: Int) ..] elements
      ]
  where
    n = length elements
renderSetCxxData prefix (CxxTypeArray _ _ elements) =
  T.intercalate
    "\n"
    [renderSetCxxData [fmt|{prefix}[{k}]|] element | (k, element) <- zip [(0 :: Int) ..] elements]
renderSetCxxData prefix (CxxTypeTuple [x, y]) =
  T.intercalate
    "\n"
    [renderSetCxxData [fmt|{prefix}.first|] x, renderSetCxxData [fmt|{prefix}.second|] y]
renderSetCxxData prefix (CxxTypeTuple elements) =
  T.intercalate
    "\n"
    [ renderSetCxxData [fmt|std::get<{k}>({prefix})|] element
      | (k, element) <- zip [(0 :: Int) ..] elements
    ]
renderSetCxxData prefix (CxxTypeMap _ _ elements) =
  T.intercalate
    "\n"
    [ renderSetCxxData [fmt|{prefix}[{key'}]|] value
      | (key, value) <- elements,
        let key' = case key of
              CxxTypeCType (CTypePrim x) -> renderPrimData x
              CxxTypePrim x -> renderCxxPrimData x
              _ -> error [fmt|got non-primitive type for map key {renderCxxType key}|]
    ]
renderSetCxxData prefix (CxxTypeStruct x) = renderSetCxxStructData prefix x
renderSetCxxData prefix (CxxTypeUnion x) = renderSetCxxUnionData prefix x

renderSetCData :: T.Text -> CType Identity -> T.Text
renderSetCData prefix (CTypePrim x) = [fmt|{prefix} = {renderPrimData x};|]
renderSetCData prefix (CTypeArray _ _ elements) =
  T.intercalate
    "\n"
    [ renderSetCData [fmt|{prefix}[{k}]|] (hproject element)
      | (k, element) <- zip [(0 :: Int) ..] elements
    ]
renderSetCData prefix (CTypeEnum cenum) = [fmt|{prefix} = {renderEnumConLiteral cenum};|]
renderSetCData prefix (CTypeStruct x) = renderSetCStructData prefix x
renderSetCData prefix (CTypeUnion x) = renderSetCUnionData prefix x

renderSetCStructData :: T.Text -> CStruct Identity -> T.Text
renderSetCStructData prefix (CStruct _ con) = renderSetCConData prefix con

renderSetCConData :: T.Text -> CCon Identity -> T.Text
renderSetCConData prefix (CNullaryCon _) = [fmt|// {prefix} is nullary con|]
renderSetCConData prefix (CNormalCon _ _ fields) =
  T.intercalate
    "\n"
    [ renderSetCData [fmt|{prefix}.{sanitizedRfName fieldName}|] (hproject field)
      | (fieldName, field) <- NE.toList fields
    ]
renderSetCConData prefix (CBitfieldCon bitfield) =
  T.intercalate
    "\n"
    [ [fmt|{prefix}.{sanitizedRfName fieldName} = {showCBool fieldValue};|]
      | (fieldName, fieldValue) <- NE.toList (fromBitfieldPrim' bitfield)
    ]
  where
    showCBool :: Bool -> T.Text
    showCBool True = "true"
    showCBool False = "false"

renderSetCxxStructData :: T.Text -> CxxStruct Identity -> T.Text
renderSetCxxStructData prefix (CxxStruct _ con) = renderSetCxxConData prefix con

renderSetCxxConData :: T.Text -> CxxCon Identity -> T.Text
renderSetCxxConData prefix (CxxNormalCon _ fields) =
  T.intercalate
    "\n"
    [ renderSetCxxData [fmt|{prefix}.{sanitizedRfName fieldName}|] field
      | (fieldName, field) <- NE.toList fields
    ]

renderSetCxxOrCConData :: T.Text -> CxxOrCCon Identity -> T.Text
renderSetCxxOrCConData prefix (CxxCon con) = renderSetCxxConData prefix con
renderSetCxxOrCConData prefix (CCon con) = renderSetCConData prefix con

-- C union
renderCUnionTagType :: CUnion f -> T.Text
renderCUnionTagType cunion = [fmt|{renderCUnionType cunion}__tag|]

renderCUnionTagMember :: CUnion f -> T.Text
renderCUnionTagMember cunion = [fmt|tag_{renderCUnionType cunion}|]

renderCUnionConType :: CUnionConF l f -> T.Text
renderCUnionConType (CUnionCon cunion ccon) = [fmt|{renderCUnionType cunion}__{conName}|]
  where
    DcName conName = cconName ccon

renderCUnionMemberName :: CCon f -> T.Text
renderCUnionMemberName ccon = [fmt|as_{conName}|] where DcName conName = cconName ccon

renderCUnionTagLiteral :: CUnion f -> CCon f -> T.Text
renderCUnionTagLiteral cunion =
  renderEnumConLiteralWithTypeName (renderCUnionType cunion) . cconName

-- C++ union
renderCxxUnionConType :: CxxUnionCon f -> T.Text
renderCxxUnionConType (CxxUnionCon cunion ccon) = [fmt|{renderCxxUnionType cunion}::{conName}|]
  where
    DcName conName = cxxOrCConName ccon

renderSetCUnionData :: T.Text -> CUnion Identity -> T.Text
renderSetCUnionData prefix cunion@(CUnion _ _ _ con) =
  [fmt|\
{prefix}.{renderCUnionTagMember cunion} = {renderCUnionTagLiteral cunion con};
{renderSetCConData conPrefix con}
|]
  where
    conPrefix = [fmt|{prefix}.{renderCUnionMemberName con}|]

renderSetCxxUnionData :: T.Text -> CxxUnion Identity -> T.Text
renderSetCxxUnionData prefix cxxUnion@(CxxUnion _ _ _ con) =
  [fmt|\
{{
  // set con
  {conTypeName} {conVarName}{{}};
{renderSetCxxOrCConData ("  " <> conVarName) con}

  // assign con
  {prefix}.payload = {conVarName};
}}
|]
  where
    conTypeName = renderCxxUnionConType (CxxUnionCon cxxUnion con)
    -- local variable to set
    -- TODO(greg/guillume): Replace with function writer monad and unique name generator.
    -- Keep this prefix for the unique name generator for debugging.
    conVarName =
      T.replace "[" "_"
        . T.replace "]" "_"
        . T.replace "." "_"
        . T.replace ":" "_"
        . T.replace "'" "_"
        $ [fmt|{prefix}__con__{conTypeName}|]

infiniteTupleFieldNames :: [RfName]
infiniteTupleFieldNames =
  [ makeRfName [fmt|tuple_element_{k}|]
    | k <- [(0 :: Int) ..]
  ]
