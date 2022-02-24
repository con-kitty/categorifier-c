{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- Data.Text.Prettyprint.Doc is deprecated in prettyprinter-1.7
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | C-code emission from CExpr graphs -- this module contains the low-level functionality for turning
-- AST fragments into pretty-printer fragments.
module Categorifier.C.CExpr.C.Pretty
  ( cFunArgs,
    newline,
    infixBinOp,
    prefixBinOp,
    prefixUnOp,
    indexArray,
    ternary,
    assignStatement,
    constAssignStatement,
    commentName,
  )
where

import Data.Text.Prettyprint.Doc (Doc, (<+>))
import qualified Data.Text.Prettyprint.Doc as Doc

{- First we can define all the pure helper functions -}

cFunArgs :: [Doc ann] -> Doc ann
cFunArgs = Doc.group . Doc.encloseSep "(" ")" ", "

newline :: Doc ann
newline = ";" <> Doc.line

infixBinOp :: Doc ann -> Doc ann -> Doc ann -> Doc ann
infixBinOp o l r = l <+> o <+> r

prefixBinOp :: Doc ann -> Doc ann -> Doc ann -> Doc ann
prefixBinOp o l r = o <> cFunArgs [l, r]

prefixUnOp :: Doc ann -> Doc ann -> Doc ann
prefixUnOp o a = o <> Doc.parens a

indexArray :: Doc ann -> Doc ann -> Doc ann
indexArray arr idx = arr <> Doc.brackets idx

ternary :: Doc ann -> Doc ann -> Doc ann -> Doc ann
ternary p t f = p <+> "?" <+> t <+> ":" <+> f

assignStatement :: Doc ann -> Doc ann -> Doc ann
assignStatement name expr = name <+> "=" <+> expr <> newline

constAssignStatement :: Doc ann -> Doc ann -> Doc ann -> Doc ann
constAssignStatement ty name expr = "const" <+> ty <+> assignStatement name expr

commentName :: Doc ann -> Doc ann -> Doc ann
commentName name fun = "/* " <> name <> "() */" <+> fun
