module Categorifier.C.CTypes.Codegen.Sanitize
  ( sanitizeText,
  )
where

import qualified Data.Set as S
import qualified Data.Text as T

sanitizeText :: T.Text -> T.Text
sanitizeText = T.pack . sanitize . T.unpack

sanitize :: String -> String
sanitize =
  let badchars = S.fromList ['\'', ' ', '(', ')', '[', ']', '<', '>', ',', ':', '.', '*']
      maybeReplaceChar x
        | S.member x badchars = '_'
        | otherwise = x
   in fmap maybeReplaceChar
