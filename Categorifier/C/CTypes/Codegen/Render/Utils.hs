{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Categorifier.C.CTypes.Codegen.Render.Utils
  ( indentWith,
    staticAssertTriviallyCopyable,
    staticAssertTriviallyCopyable',
    toCommentText,
    nolintNextLineReadabilityIdentifierNaming,
    nolintReadabilityIdentifierNaming,
    staticAssert,
  )
where

import Categorifier.C.CTypes.DSL.CxxAst (CExpr (..), Comment (..), Identifier (..), (#!))
import Categorifier.C.CTypes.DSL.FunctionWriter (FunWriter, force_)
import qualified Categorifier.C.CTypes.Render as R
import Categorifier.C.CTypes.Types (CType)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import PyF (fmt)

indentWith :: T.Text -> T.Text -> T.Text
indentWith leadingSpaces = T.intercalate "\n" . fmap indent . T.splitOn "\n"
  where
    indent :: T.Text -> T.Text
    indent x
      | T.null x = x
      | -- don't add spaces to an empty line
        otherwise =
          leadingSpaces <> x

staticAssertTriviallyCopyable :: CType Proxy -> FunWriter ()
staticAssertTriviallyCopyable = staticAssertTriviallyCopyable' . R.renderCType

staticAssertTriviallyCopyable' :: T.Text -> FunWriter ()
staticAssertTriviallyCopyable' typeName =
  staticAssert
    (Ident (Identifier [fmt|std::is_trivially_copyable<{typeName}>::value|]))
    [fmt|{typeName} is not trivially copyable|]

toCommentText :: Maybe Comment -> T.Text
toCommentText Nothing = ""
toCommentText (Just (Comment comment)) = "// " <> T.replace "\n" "\n// " comment <> "\n"

nolintNextLineReadabilityIdentifierNaming :: T.Text
nolintNextLineReadabilityIdentifierNaming = "// NOLINTNEXTLINE(readability-identifier-naming)"

nolintReadabilityIdentifierNaming :: T.Text
nolintReadabilityIdentifierNaming = "// NOLINT(readability-identifier-naming)"

staticAssert :: CExpr -> CExpr -> FunWriter ()
staticAssert a b = force_ $ Identifier "static_assert" #! [a, b]
