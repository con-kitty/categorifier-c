{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Render C typedefs.
module Categorifier.C.CTypes.Codegen.Render.Typedefs
  ( renderTypedef,
    writeCField,
  )
where

import Categorifier.C.CTypes.Codegen.Render.Utils
  ( indentWith,
    nolintNextLineReadabilityIdentifierNaming,
    nolintReadabilityIdentifierNaming,
    toCommentText,
  )
import Categorifier.C.CTypes.DSL.CxxAst (Comment (..), Typedef (..))
import qualified Categorifier.C.CTypes.Render as R
import Categorifier.C.CTypes.Types
  ( CBitfield (..),
    CCon,
    CConF (..),
    CEnum (..),
    CNat,
    CStruct,
    CStructF (..),
    CType,
    CTypeF (..),
    CUnion,
    CUnionConF (..),
    CUnionF (..),
    CxxCon (..),
    CxxOrCCon (..),
    CxxStruct (..),
    CxxType (..),
    CxxUnion (..),
    DcName (..),
    RfName,
    allEnumCons,
    bfprimToPrim,
    cxxOrCConName,
    sanitizedRfName,
  )
import Categorifier.C.Prim (getPrimCName)
import Categorifier.C.Recursion (hproject)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import PyF (fmt)

renderTypedef :: Typedef -> T.Text
renderTypedef (TypedefEnum x) = renderCEnumDef x
renderTypedef (TypedefCxxUnion x) = renderCxxUnionDef x
renderTypedef (TypedefCUnion x) = renderCUnionDef x
renderTypedef (TypedefCxxStruct x) = renderCxxStructDef x
renderTypedef (TypedefCStruct x) = renderCStructDef x

writeBitfieldsThenPadding :: CBitfield Proxy -> T.Text
writeBitfieldsThenPadding (CBitfield (DcName dcname) fields value) =
  T.intercalate "\n" $ fmap writeBitField (NE.toList fields) <> [padding]
  where
    n = NE.length fields
    writeBitField :: RfName -> T.Text
    writeBitField fieldName = [fmt|  {underlyingType} {sanitizedRfName fieldName} : 1;|]
    underlyingType = getPrimCName $ bfprimToPrim value
    padding
      | n == 8 = "  // no padding needed"
      | n == 16 = "  // no padding needed"
      | n == 32 = "  // no padding needed"
      | n == 64 = "  // no padding needed"
      | n < 8 = [fmt|  {underlyingType} _padding : {8 - n};|]
      | n < 16 = [fmt|  {underlyingType} _padding : {16 - n};|]
      | n < 32 = [fmt|  {underlyingType} _padding : {32 - n};|]
      | n < 64 = [fmt|  {underlyingType} _padding : {64 - n};|]
      | otherwise =
          error
            [fmt|\
Bitfield struct "{dcname}" has too many bits ({n}) to represent as a standard int type.
Please see `The C Programming Language` by Brian Kernighan and Dennis Ritchie.
|]

renderCStructDef :: CStruct Proxy -> T.Text
renderCStructDef cstruct =
  renderCStructDefWithTypeName
    (Just comment)
    (R.renderCStructType cstruct)
    (csCon cstruct)
  where
    comment = Comment [fmt|C struct {R.renderCStructType cstruct}|]

-- Render a struct typedef with a given name.
-- Useful for creating normal structs and also bonus structs derived from union constructors.
renderCStructDefWithTypeName :: Maybe Comment -> T.Text -> CCon Proxy -> T.Text
renderCStructDefWithTypeName mcomment typeName (CNullaryCon _) =
  [fmt|\
{toCommentText mcomment}\
typedef struct {{
  // If we compile a library (like a serialization library) in C and then call it
  // from C++, the structs must be binary compatible to avoid memory corruption.
  // Empty C structs are undefined (size 0 with GCC extensions) and empty C++ structs
  // are size 1. So we add this dummy field to sidestep this mismatch.
  uint8_t pls_ignore_this_padding_c_and_cxx_structs_need_to_be_the_same;
}} {typeName};  {nolintReadabilityIdentifierNaming}
|]
renderCStructDefWithTypeName mcomment typeName (CNormalCon _ _ fields) =
  [fmt|\
{toCommentText mcomment}\
typedef struct {{
{T.intercalate "\n" (writeCField <$> NE.toList (fmap hproject <$> fields))}
}} {typeName};  {nolintReadabilityIdentifierNaming}
|]
renderCStructDefWithTypeName mcomment typeName (CBitfieldCon bitfield) =
  [fmt|\
{toCommentText mcomment}\
typedef struct {{
{writeBitfieldsThenPadding bitfield}
}} {typeName};  {nolintReadabilityIdentifierNaming}
|]

getCxxArrayElements :: CxxType Proxy -> ([CNat], CxxType Proxy)
getCxxArrayElements (CxxTypeArray nat elemType _) = (nat : nats, bottomElemType)
  where
    (nats, bottomElemType) = getCxxArrayElements elemType
getCxxArrayElements (CxxTypeCType (CTypeArray nat elemType elems)) =
  -- call the C++ version to avoid code duplication
  getCxxArrayElements
    ( CxxTypeArray
        nat
        (CxxTypeCType $ hproject elemType)
        (CxxTypeCType <$> fmap hproject elems)
    )
getCxxArrayElements t = ([], t)

writeCField :: (RfName, CType Proxy) -> T.Text
writeCField (rfName, ctype) = writeCxxField (rfName, CxxTypeCType ctype)

writeCxxField :: (RfName, CxxType Proxy) -> T.Text
writeCxxField (fieldName, fieldType) = case getCxxArrayElements fieldType of
  ([], _) -> [fmt|  {R.renderCxxType fieldType} {sanitizedRfName fieldName};|]
  (dims, elemType) ->
    let shownDims = T.intercalate ", " $ fmap R.renderCNat dims
     in -- TODO(greg):
        -- Multidimensional arrays or arrays-of-arrays?
        -- shownDims = T.intercalate "][" $ fmap showCNat dims
        [fmt|  {R.renderCxxType elemType} {sanitizedRfName fieldName}[{shownDims}];|]

renderCxxStructDef :: CxxStruct Proxy -> T.Text
renderCxxStructDef cstruct@(CxxStruct _ con) =
  renderCxxStructDefWithTypeName
    (Just comment)
    (R.renderCxxStructType cstruct)
    con
  where
    comment = Comment "C++ struct {renderCxxStructType cstruct}"

renderCxxStructDefWithTypeName :: Maybe Comment -> T.Text -> CxxCon Proxy -> T.Text
renderCxxStructDefWithTypeName mcomment typeName (CxxNormalCon _ fields) =
  [fmt|\
{toCommentText mcomment}\
{nolintNextLineReadabilityIdentifierNaming}
struct {typeName} {{
{T.intercalate "\n" (writeCxxField <$> NE.toList fields)}
}};
|]

renderCEnumDef :: CEnum Proxy -> T.Text
renderCEnumDef cenum =
  renderCEnumDefWithTypeName
    (R.renderCEnumType cenum)
    (R.renderEnumConLiteral <$> allEnumCons cenum)

renderCEnumDefWithTypeName :: T.Text -> [T.Text] -> T.Text
renderCEnumDefWithTypeName typeName cons =
  [fmt|\
typedef enum {{
  {nolintNextLineReadabilityIdentifierNaming}
{fields}  {nolintNextLineReadabilityIdentifierNaming}
}} {typeName};  {nolintReadabilityIdentifierNaming}
static_assert(sizeof({typeName}) == sizeof(int32_t), "enum is not 32 bits");
|]
  where
    fields = T.intercalate ",\n" $ zipWith toField allFieldNames [(-1) ..]
      where
        -- first element is "FooForceSigned = -1"
        -- middle elements are "Foo{FieldNameK} = {k}"
        -- last element is "FooNumFields = {num fields}"
        allFieldNames = forceSignedName : cons <> [numFieldsName]
          where
            forceSignedName = R.renderEnumForceSignedLiteralWithTypeName typeName
            numFieldsName = R.renderEnumNumFieldsLiteralWithTypeName typeName
    toField :: T.Text -> Int -> T.Text
    toField conName k = [fmt|  {conName} = {k}|]

renderCUnionDef :: CUnion Proxy -> T.Text
renderCUnionDef cunion@(CUnion _ conVec _ _) =
  [fmt|\
// ================ Union {typeName} ================
// Tag type for {typeName}
{tagEnumDecl}\

// Data constructors for {typeName}
{structDecls}\

// The union itself
typedef struct {{
  // Tag
  {tagType} {tagMember};
  // Possible constructors
  union {{
{members}
  }};
}} {typeName};  {nolintReadabilityIdentifierNaming}
|]
  where
    cons = V.toList conVec
    tagEnumDecl = renderCEnumDefWithTypeName tagType (R.renderCUnionTagLiteral cunion <$> cons)
    members = indentWith "    " . T.intercalate "\n" . fmap renderUnionMember $ cons
    tagType = R.renderCUnionTagType cunion
    tagMember = R.renderCUnionTagMember cunion
    typeName = R.renderCUnionType cunion
    structDecls = T.intercalate "\n" (renderUnionCon <$> cons)
    renderUnionCon :: CCon Proxy -> T.Text
    renderUnionCon con =
      renderCStructDefWithTypeName
        comment
        (R.renderCUnionConType (CUnionCon cunion con))
        con
      where
        comment = Nothing
    renderUnionMember :: CCon Proxy -> T.Text
    renderUnionMember con =
      [fmt|{R.renderCUnionConType (CUnionCon cunion con)} {R.renderCUnionMemberName con};|]

renderCxxUnionDef :: CxxUnion Proxy -> T.Text
renderCxxUnionDef cxxunion@(CxxUnion _ conVec _ _) =
  [fmt|\
// ================ C++ Union (mapbox::variant) {typeName} ================
{nolintNextLineReadabilityIdentifierNaming}
struct {typeName} {{
{structDecls}\

  // Payload
  mapbox::util::variant<{scopedMemberTypes}> payload;
}};
|]
  where
    cons = V.toList conVec
    scopedMemberTypes = T.intercalate ", " $ fmap toScopedConName cons
      where
        toScopedConName = unDcName . cxxOrCConName
    typeName = R.renderCxxUnionType cxxunion
    structDecls =
      T.intercalate "\n" . fmap (indentWith "  ") $ zipWith renderCxxUnionCon [0 ..] cons
    renderCxxUnionCon :: Int -> CxxOrCCon Proxy -> T.Text
    renderCxxUnionCon k con = case con of
      CxxCon cxxCon -> renderCxxStructDefWithTypeName comment conTypeName cxxCon
      CCon cCon -> renderCStructDefWithTypeName comment conTypeName cCon
      where
        conTypeName = unDcName (cxxOrCConName con)
        comment = Just $ Comment [fmt|Constructor {k} of {typeName}: {conTypeName}.|]
