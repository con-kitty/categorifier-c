{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Categorifier.C.CTypes.Codegen.Helpers
  ( OrderedTypes (..),
    CStructOrUnion (..),
    CxxStructOrUnion (..),
    getOrderedTypes,
  )
where

import Categorifier.C.CTypes.KTypeRep (KTypeRep)
import Categorifier.C.CTypes.Types
import Categorifier.C.Lens.Rules (categorifierLensRules)
import Categorifier.C.Recursion (hproject)
import Control.Lens (Lens', makeLensesWith, (%~), (^.))
import Control.Monad.Trans.State.Strict (State, execState, get, modify, put)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import qualified Data.Set as S

data CStructOrUnion = CS (CStruct Proxy) | CU (CUnion Proxy) deriving (Eq, Ord)

data CxxStructOrUnion = CxxS (CxxStruct Proxy) | CxxU (CxxUnion Proxy) deriving (Eq, Ord)

data GetAcc = GetAcc
  { gaCENats :: S.Set CNat,
    gaCEnums :: S.Set (CEnum Proxy),
    -- | usually KIO types, but need to be generated at the top level
    -- TODO(greg): remove this once the function exporter runs in the array function writer monad
    gaCArrays :: S.Set (CType Proxy),
    gaCStructMap :: M.Map KTypeRep (CStruct Proxy),
    gaCUnionMap :: M.Map KTypeRep (CUnion Proxy),
    gaCStructsOrUnions :: [CStructOrUnion],
    gaCxxStructMap :: M.Map KTypeRep (CxxStruct Proxy),
    gaCxxUnionMap :: M.Map KTypeRep (CxxUnion Proxy),
    gaCxxStructsOrUnions :: [CxxStructOrUnion]
  }

makeLensesWith categorifierLensRules ''GetAcc

data OrderedTypes
  = OrderedTypes
      [CNat]
      [CEnum Proxy]
      [CStructOrUnion]
      [CxxStructOrUnion]
      [CType Proxy]

getOrderedTypes :: Foldable f => f (CxxType Proxy) -> OrderedTypes
getOrderedTypes types =
  OrderedTypes
    (S.toList (gaCENats finalState))
    (S.toList (gaCEnums finalState))
    (reverse (gaCStructsOrUnions finalState))
    (reverse (gaCxxStructsOrUnions finalState))
    (S.toList (gaCArrays finalState))
  where
    finalState =
      execState
        (traverse_ getOrderedCxxType types)
        GetAcc
          { gaCENats = S.empty,
            gaCEnums = S.empty,
            gaCArrays = S.empty,
            gaCStructMap = M.empty,
            gaCUnionMap = M.empty,
            gaCStructsOrUnions = [],
            gaCxxStructMap = M.empty,
            gaCxxUnionMap = M.empty,
            gaCxxStructsOrUnions = []
          }

getOrderedCxxType :: CxxType Proxy -> State GetAcc ()
getOrderedCxxType (CxxTypeCType x) = getOrderedCType x
getOrderedCxxType (CxxTypeStruct x) = getOrderedCxxStruct x
getOrderedCxxType (CxxTypeUnion x) = getOrderedCxxUnion x
getOrderedCxxType (CxxTypeArray nat elemType _) = do
  modify (gaCENats_ %~ S.insert nat)
  getOrderedCxxType elemType
getOrderedCxxType CxxTypePrim {} = pure ()
getOrderedCxxType (CxxTypeTuple xs) = traverse_ getOrderedCxxType xs
getOrderedCxxType (CxxTypeVector _ elemType _) = getOrderedCxxType elemType
getOrderedCxxType (CxxTypeMap keyType valueType _) =
  getOrderedCxxType keyType >> getOrderedCxxType valueType

getOrderedCType :: CType Proxy -> State GetAcc ()
getOrderedCType (CTypeEnum x) = modify (gaCEnums_ %~ S.insert x)
getOrderedCType (CTypeStruct x) = getOrderedCStruct x
getOrderedCType (CTypeUnion x) = getOrderedCUnion x
getOrderedCType (CTypeArray nat elemType elems) = do
  modify (gaCENats_ %~ S.insert nat)
  getOrderedCType $ hproject elemType
  modify (gaCArrays_ %~ S.insert (CTypeArray nat elemType elems))
getOrderedCType (CTypePrim _) = pure ()

getOrderedStructOrUnion ::
  a ->
  (a -> KTypeRep) ->
  Lens' GetAcc (M.Map KTypeRep a) ->
  (a -> GetAcc -> GetAcc) ->
  (a -> [CxxType Proxy]) ->
  State GetAcc ()
getOrderedStructOrUnion newThing toRep mapLens prependToSuList toFieldTypes = do
  let rep = toRep newThing
      fieldTypes = toFieldTypes newThing
  acc0 <- get
  case M.lookup rep (acc0 ^. mapLens) of
    Just _ -> pure ()
    Nothing -> do
      put $ (mapLens %~ M.insert rep newThing) acc0
      -- types this definition is dependent on
      traverse_ getOrderedCxxType fieldTypes
      modify (prependToSuList newThing)

getOrderedCxxStruct :: CxxStruct Proxy -> State GetAcc ()
getOrderedCxxStruct x =
  getOrderedStructOrUnion
    x
    cxxsTypeRep
    gaCxxStructMap_
    prependToSuList
    (getCxxConFields . cxxsCon)
  where
    prependToSuList newThing = gaCxxStructsOrUnions_ %~ (CxxS newThing :)

getOrderedCxxUnion :: CxxUnion Proxy -> State GetAcc ()
getOrderedCxxUnion x =
  getOrderedStructOrUnion
    x
    cxxuTypeRep
    gaCxxUnionMap_
    prependToSuList
    (concatMap getCxxOrCConFields . cxxuCons)
  where
    prependToSuList newThing = gaCxxStructsOrUnions_ %~ (CxxU newThing :)

getOrderedCStruct :: CStruct Proxy -> State GetAcc ()
getOrderedCStruct x =
  getOrderedStructOrUnion
    x
    csTypeRep
    gaCStructMap_
    prependToSuList
    (getCConFields . csCon)
  where
    prependToSuList newThing = gaCStructsOrUnions_ %~ (CS newThing :)

getOrderedCUnion :: CUnion Proxy -> State GetAcc ()
getOrderedCUnion x =
  getOrderedStructOrUnion
    x
    cuTypeRep
    gaCUnionMap_
    prependToSuList
    (concatMap getCConFields . cuCons)
  where
    prependToSuList newThing = gaCStructsOrUnions_ %~ (CU newThing :)

getCxxConFields :: CxxCon f -> [CxxType f]
getCxxConFields (CxxNormalCon _ fields) = fmap snd (NE.toList fields)

getCxxOrCConFields :: CxxOrCCon f -> [CxxType f]
getCxxOrCConFields (CxxCon r) = getCxxConFields r
getCxxOrCConFields (CCon r) = getCConFields r

getCConFields :: CCon f -> [CxxType f]
getCConFields (CNullaryCon _) = []
getCConFields (CNormalCon _ _ fields) = fmap (CxxTypeCType . hproject . snd) (NE.toList fields)
getCConFields (CBitfieldCon _) = []
