{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Categorifier.C.CTypes.WhyNotCType
  ( whyNotCType,
  )
where

import Categorifier.C.CTypes.Render (renderCxxType)
import Categorifier.C.CTypes.Types
  ( CxxCon (..),
    CxxOrCCon (..),
    CxxStruct (..),
    CxxType (..),
    CxxUnion (..),
    DcName (..),
    sanitizedRfName,
  )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text, pack, unpack)
import Data.Tree (Tree (..), drawTree)
import qualified Data.Vector as V
import PyF (fmt)

-- | Draw a tree structure to tell you why this is not a CType.
whyNotCType :: CxxType f -> Text
whyNotCType cxxType = case pathToCxxTypes "" cxxType of
  Nothing -> pack "<found no reasons this should be a CxxType instead of a CType>"
  Just pathTree -> pack . drawTree . fmap unpack $ pathTree

pathToCxxTypes :: Text -> CxxType f -> Maybe (Tree Text)
pathToCxxTypes _ CxxTypeCType {} = Nothing
pathToCxxTypes topName t@(CxxTypeStruct (CxxStruct _ (CxxNormalCon _ fields))) =
  Just $
    Node [fmt|{topName} ({renderCxxType t})|] $
      mapMaybe f (NonEmpty.toList fields)
  where
    f (fieldName, cxxType) = pathToCxxTypes (sanitizedRfName fieldName) cxxType
pathToCxxTypes topName t@(CxxTypeUnion (CxxUnion _ cons _ _)) =
  Just $
    Node [fmt|{topName} ({renderCxxType t})|] $
      mapMaybe f $
        zip [(0 :: Int) ..] (V.toList cons)
  where
    f (_, CCon {}) = Nothing
    f (k, CxxCon (CxxNormalCon (DcName conName) fields)) =
      Just $
        Node [fmt|union constructor {k} ({conName})|] $
          mapMaybe g (NonEmpty.toList fields)
      where
        g (fieldName, cxxType) = pathToCxxTypes (sanitizedRfName fieldName) cxxType
pathToCxxTypes topName (CxxTypeArray _ elementType _) = pathToCxxTypes topName elementType
pathToCxxTypes topName t@CxxTypePrim {} = Just $ Node [fmt|{topName} ({renderCxxType t})|] []
pathToCxxTypes topName t@(CxxTypeTuple elements) =
  Just $
    Node [fmt|{topName} ({renderCxxType t})|] $
      mapMaybe f $
        zip [(0 :: Int) ..] elements
  where
    f (k, tupleElement) = pathToCxxTypes [fmt|tuple element {k}|] tupleElement
pathToCxxTypes topName (CxxTypeVector _ elementType _) = pathToCxxTypes topName elementType
pathToCxxTypes topName t@(CxxTypeMap keyType valueType _) =
  Just $
    Node [fmt|{topName} ({renderCxxType t})|] $
      catMaybes
        [ pathToCxxTypes "key type" keyType,
          pathToCxxTypes "value type" valueType
        ]
