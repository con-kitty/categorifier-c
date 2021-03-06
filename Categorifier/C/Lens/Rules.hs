module Categorifier.C.Lens.Rules
  ( categorifierLensRules,
  )
where

import Control.Lens ((.~))
import Control.Lens.Internal.FieldTH (DefName (TopName))
import Control.Lens.TH (LensRules, classyRules, lensClass, lensField)
import Data.Char (toLower)
import Language.Haskell.TH.Syntax (mkName, nameBase)

categorifierLensRules :: LensRules
categorifierLensRules =
  lensClass .~ rewriteClass $
    lensField .~ rewriteField $
      classyRules
  where
    rewriteClass n = case nameBase n of
      x : xs ->
        Just
          ( mkName ("Has" <> (x : xs)),
            mkName ((toLower x : xs) <> "_")
          )
      [] -> Nothing
    rewriteField _ _ n = [TopName (mkName (nameBase n <> "_"))]
