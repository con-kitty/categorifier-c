{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Misc helper functions for enums.
module Categorifier.C.CTypes.Codegen.EnumUtils
  ( toEnumUtilsModule,
  )
where

import Categorifier.C.CTypes.DSL.CxxAst
  ( CExpr (..),
    CFunction (..),
    CxxModule (..),
    CxxTarget (..),
    Identifier (..),
    Include (..),
    Param (..),
    ParamType (..),
    TypeLevelFunctions (..),
    Typedef (..),
    dereference,
    (#!),
  )
import Categorifier.C.CTypes.DSL.FunctionWriter
  ( caseEnum,
    caseUnionTag,
    cexprFunctionFromParams,
    if_,
    ret,
    (=:),
  )
import Categorifier.C.CTypes.Instances ()
import qualified Categorifier.C.CTypes.Render as R
import Categorifier.C.CTypes.ToCxxType (CoercibleT, ToCxxType (..))
import Categorifier.C.CTypes.Types
import Categorifier.C.KTypes.C (toCxxTypeViaC)
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import PyF (fmt)

data EnumOrUnion
  = Enum' (CEnum Proxy)
  | Union (CUnion Proxy)

enumOrUnionTagTypeName :: EnumOrUnion -> T.Text
enumOrUnionTagTypeName (Enum' cenum) = R.renderCEnumType cenum
enumOrUnionTagTypeName (Union cunion) = R.renderCUnionTagType cunion

enumOrUnionNumFields :: EnumOrUnion -> Int
enumOrUnionNumFields (Enum' cenum) = length (ceCons cenum)
enumOrUnionNumFields (Union cunion) = length (cuCons cunion)

toEnumUtilsModule :: [CEnum Proxy] -> [CUnion Proxy] -> CxxModule
toEnumUtilsModule cenums cunions =
  CxxModule
    { moduleIncludes = includes,
      moduleDefines = [],
      moduleTypedefs = [TypedefEnum (toEnumFromStringResult Proxy)],
      moduleUsingDecls = [],
      moduleFunctions =
        fmap enumToStringFun enumsAndUnions <> fmap enumFromStringFun enumsAndUnions,
      moduleTypeLevelFunctions = [enumLengths]
    }
  where
    enumsAndUnions = fmap Enum' cenums <> fmap Union cunions
    includes = [IncludeSystemFile "string", IncludeModule CTypes]
    enumLengths =
      TypeLevelFunctions
        { tlfReturnType = "int32_t",
          tlfClassName = "EnumLength",
          tlfFunctionName = "Length",
          tlfInstances = fmap toInstance enumsAndUnions
        }
      where
        toInstance enumOrUnion = (enumOrUnionTagTypeName enumOrUnion, body)
          where
            n = enumOrUnionNumFields enumOrUnion
            body = [fmt|return {n};|]

renderEnumOrUnionTagTypeName :: EnumOrUnion -> T.Text
renderEnumOrUnionTagTypeName (Enum' cenum) = R.renderCEnumType cenum
renderEnumOrUnionTagTypeName (Union cunion) = R.renderCUnionTagType cunion

enumOrUnionParamType :: EnumOrUnion -> ParamType
enumOrUnionParamType (Enum' cenum) = ParamCxxType (CxxTypeCType (CTypeEnum cenum))
enumOrUnionParamType (Union cunion) = ParamCUnionTag cunion

enumToStringFunName :: EnumOrUnion -> Identifier
enumToStringFunName enumOrUnion = Identifier [fmt|EnumToString_{typeName}|]
  where
    typeName = renderEnumOrUnionTagTypeName enumOrUnion

enumFromStringFunName :: EnumOrUnion -> Identifier
enumFromStringFunName enumOrUnion = Identifier [fmt|EnumFromString_{typeName}|]
  where
    typeName = renderEnumOrUnionTagTypeName enumOrUnion

enumToStringFun :: EnumOrUnion -> CFunction
enumToStringFun enumOrUnion =
  UnsafeCFunction
    { cfName = enumToStringFunName enumOrUnion,
      cfInlineOverloadName = Just $ Identifier [fmt|EnumToString|],
      cfReturnType = Just (ParamCxxType (CxxTypePrim (PrimString Proxy))),
      cfStaticLinkage = False,
      -- Ugly comment for debugging. TODO(greg): prettify.
      cfComment = Nothing,
      cfParams = params,
      cfTape = body
    }
  where
    params =
      [ Param
          { pType = enumOrUnionParamType enumOrUnion,
            pId = Identifier "value",
            pUnused = False,
            pMutable = False
          }
      ]
    body = cexprFunctionFromParams params $ \value -> case enumOrUnion of
      Enum' cenum -> caseEnum cenum value (ret . LiteralString) (ret "<<INVALID ENUM VALUE>>")
      Union cunion -> caseUnionTag cunion value (ret . LiteralString) (ret "<<INVALID ENUM VALUE>>")

-- Result of EnumFromString function.
data EnumFromStringResult = Success | Failure deriving (Generic)

instance SupportsKBits f => ToCxxType f EnumFromStringResult

toEnumFromStringResult ::
  forall g.
  (Applicative g, Traversable g, CoercibleT g) =>
  g EnumFromStringResult ->
  CEnum g
toEnumFromStringResult x = case toCxxTypeViaC x of
  CxxTypeCType (CTypeEnum r) -> r
  r -> error [fmt|EnumFromStringResult is not CTypeEnum, it is {R.renderCxxType r}|]

enumFromStringFun :: EnumOrUnion -> CFunction
enumFromStringFun enumOrUnion =
  UnsafeCFunction
    { cfName = enumFromStringFunName enumOrUnion,
      cfInlineOverloadName = Just $ Identifier [fmt|EnumFromString|],
      cfReturnType = Just (ParamCxxType (CxxTypeCType (CTypeEnum (toEnumFromStringResult Proxy)))),
      cfStaticLinkage = False,
      cfComment = Nothing,
      cfParams = params,
      cfTape = body
    }
  where
    params =
      [ Param
          { pType = ParamCxxType (toCxxTypeViaC (Proxy @T.Text)),
            pId = Identifier "enum_as_string",
            pUnused = False,
            pMutable = False
          },
        Param
          { pType = enumOrUnionParamType enumOrUnion,
            pId = Identifier "enum_as_value",
            pUnused = False,
            pMutable = True
          }
      ]
    cons :: [CExpr]
    cons = fmap (Ident . Identifier) $ case enumOrUnion of
      Enum' cenum -> fmap R.renderEnumConLiteral (allEnumCons cenum)
      Union cunion -> fmap (R.renderCUnionTagLiteral cunion) (V.toList (cuCons cunion))
    body = cexprFunctionFromParams params $ \enum_as_string enum_as_value -> do
      for_ cons $ \con -> if_ (enum_as_string :== (enumToStringFunName' #! [con])) $ do
        dereference enum_as_value =: con
        ret $
          Ident . Identifier $ R.renderEnumConLiteral (toEnumFromStringResult (Identity Success))
      ret $ Ident . Identifier $ R.renderEnumConLiteral (toEnumFromStringResult (Identity Failure))
    enumToStringFunName' = enumToStringFunName enumOrUnion
