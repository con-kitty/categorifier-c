{-# LANGUAGE OverloadedStrings #-}

-- | Modules that have all the C and C++ typedefs.
module Categorifier.C.CTypes.Codegen.TypesHeaders
  ( toCTypesModule,
    toCxxTypesModule,
  )
where

import Categorifier.C.CTypes.Codegen.Helpers (CStructOrUnion (..), CxxStructOrUnion (..))
import Categorifier.C.CTypes.Codegen.Render.Render (passByReference)
import Categorifier.C.CTypes.DSL.CxxAst
  ( CExpr (..),
    CFunction (..),
    CxxModule (..),
    CxxTarget (..),
    Identifier (..),
    Include (..),
    Param (..),
    ParamType (..),
    SystemLib (..),
    Typedef (..),
    dereference,
  )
import Categorifier.C.CTypes.DSL.FunctionWriter (comment, newDefaultNamed, ret, runFunWriter, (=:))
import qualified Categorifier.C.CTypes.Render as R
import Categorifier.C.CTypes.Types
import Categorifier.C.Recursion (hproject)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V

toCTypesModule :: [CEnum Proxy] -> [CStructOrUnion] -> CxxModule
toCTypesModule enums structsOrUnions =
  CxxModule
    { moduleIncludes =
        [ IncludeModule Dimensions,
          IncludeSystemLib StdInt,
          IncludeSystemLib StdBool,
          IncludeSystemLib Assert -- static_assert
        ],
      moduleDefines = [],
      moduleTypedefs =
        let toSUDef (CS x) = TypedefCStruct x
            toSUDef (CU x) = TypedefCUnion x
         in fmap TypedefEnum enums <> fmap toSUDef structsOrUnions,
      moduleUsingDecls = [],
      moduleFunctions =
        let toSUSmartishConstructors (CU u) = toUnionSmartishConstructors u
            toSUSmartishConstructors (CS _) = []
         in concatMap toSUSmartishConstructors structsOrUnions,
      moduleTypeLevelFunctions = []
    }

toCxxTypesModule :: [CxxStructOrUnion] -> CxxModule
toCxxTypesModule structsOrUnions =
  CxxModule
    { moduleIncludes =
        [ IncludeSystemFile "cstring",
          IncludeSystemFile "list",
          IncludeSystemFile "map",
          IncludeSystemFile "tuple",
          IncludeSystemFile "utility",
          IncludeSystemFile "vector",
          IncludeSystemFile "variant.hpp",
          IncludeModule CTypes
        ],
      moduleDefines = [],
      moduleTypedefs =
        let toSUDef (CxxS x) = TypedefCxxStruct x
            toSUDef (CxxU x) = TypedefCxxUnion x
         in fmap toSUDef structsOrUnions,
      moduleUsingDecls = [],
      moduleFunctions = [],
      moduleTypeLevelFunctions = []
    }

-- TODO(greg): These return by value so they are inefficient in C, right?
-- Implement a version that returns by reference?
toUnionSmartishConstructors :: CUnion Proxy -> [CFunction]
toUnionSmartishConstructors cunion@(CUnion _ ccons _ _) = fmap f (V.toList ccons)
  where
    f ccon =
      UnsafeCFunction
        { cfName = Identifier $ "Construct" <> R.renderCUnionConType (CUnionCon cunion ccon),
          cfInlineOverloadName = Nothing,
          cfReturnType = Just (ParamCxxType (CxxTypeCType (CTypeUnion cunion))),
          cfStaticLinkage = False, -- TODO(greg): consider inlining
          cfComment = Nothing,
          cfParams = params,
          cfTape = runFunWriter $ do
            retValue <- newDefaultNamed "ret" (CTypeUnion cunion)
            retValue :. makeRfName (R.renderCUnionTagMember cunion)
              =: Ident
                (Identifier $ R.renderCUnionTagLiteral cunion ccon)
            setCon retValue
            ret retValue
        }
      where
        asCon = R.renderCUnionMemberName ccon
        (params, setCon) = case ccon of
          -- Don't need to pass a parameter or set anything for a nullary con.
          CNullaryCon {} -> ([], const $ comment "Nullary con has nothing to set")
          -- If it has exactly one field, skip the constructor and just pass the field.
          CNormalCon _ _ ((rfName, fieldType) NE.:| []) ->
            let paramType = ParamCxxType (CxxTypeCType $ hproject fieldType)
             in ( [ Param
                      { pType = paramType,
                        pId = Identifier "field_value",
                        pUnused = False,
                        pMutable = False
                      }
                  ],
                  \retValue ->
                    retValue :. makeRfName asCon :. rfName
                      =: maybeDereference
                        paramType
                        (Ident (Identifier "field_value"))
                )
          -- Otherwise just pass the struct.
          _ ->
            let paramType = ParamCUnionCon (CUnionCon cunion ccon)
             in ( [ Param
                      { pType = paramType,
                        pId = Identifier "con",
                        pUnused = False,
                        pMutable = False
                      }
                  ],
                  \retValue ->
                    retValue :. makeRfName asCon
                      =: maybeDereference paramType (Ident (Identifier "con"))
                )

maybeDereference :: ParamType -> (CExpr -> CExpr)
maybeDereference paramType
  | passByReference paramType = dereference
  | otherwise = id
