{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}

module Categorifier.C.Hierarchy.Specialized
  ( cSpecializedHierarchy,
  )
where

import Categorifier.Core.Types (CategoryStack, Lookup)
import Categorifier.Hierarchy
  ( Hierarchy (..),
    emptyHierarchy,
    identifier,
    mkFunctionApps,
  )

-- | This hierarchy replaces some categorical operations with their specialized
-- versions for `Categorifier.C.CExpr.Cat.Cat`. For example, `(.) $fCategoryCat`
-- (which occurs quite often in the result) is replaced by `(.$)`, thereby significantly
-- reduces common subexpressions.
cSpecializedHierarchy :: Lookup (Hierarchy CategoryStack)
cSpecializedHierarchy = do
  kapplyV <-
    pure <$> do
      fn <- identifier' "applyCat"
      pure (\onDict _cat a b -> mkFunctionApps onDict fn [a, b] [])
  kapply2V <-
    pure <$> do
      fn <- identifier' "apply2Cat"
      pure (\onDict _cat x a b f g -> mkFunctionApps onDict fn [x, a, b] [f, g])
  kcomposeV <-
    pure <$> do
      fn <- identifier' ".$"
      pure (\onDict _cat a b c -> mkFunctionApps onDict fn [b, c, a] [])
  kcompose2V <-
    pure <$> do
      fn <- identifier' "compose2Cat"
      pure (\onDict _cat x b c a f g -> mkFunctionApps onDict fn [x, b, c, a] [f, g])
  kcurryV <-
    pure <$> do
      fn <- identifier' "curryCat"
      pure (\onDict _cat a b c -> mkFunctionApps onDict fn [a, b, c] [])
  kexlV <-
    pure <$> do
      fn <- identifier' "exlCat"
      pure (\onDict _cat a b -> mkFunctionApps onDict fn [a, b] [])
  kexrV <-
    pure <$> do
      fn <- identifier' "exrCat"
      pure (\onDict _cat a b -> mkFunctionApps onDict fn [a, b] [])
  kforkV <-
    pure <$> do
      fn <- identifier' "&&&$"
      pure (\onDict _cat a b c -> mkFunctionApps onDict fn [a, b, c] [])
  kuncurryV <-
    pure <$> do
      fn <- identifier' "uncurryCat"
      pure (\onDict _cat a b c -> mkFunctionApps onDict fn [a, b, c] [])
  pure
    emptyHierarchy
      { applyV = kapplyV,
        apply2V = kapply2V,
        composeV = kcomposeV,
        compose2V = kcompose2V,
        curryV = kcurryV,
        exlV = kexlV,
        exrV = kexrV,
        forkV = kforkV,
        uncurryV = kuncurryV
      }
  where
    identifier' = identifier "Categorifier.C.CExpr.Cat"
