{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kitty.KTypes.FMod
  ( FMod (..),
  )
where

import Control.Applicative (liftA2)
import Kitty.CExpr.Cat (Cat (..), cat)
import Kitty.CExpr.Cat.TargetOb (TargetOb)
import Kitty.CExpr.Types.Core (CExpr, CExprF (..))
import Kitty.CExpr.Types.Operations (FPBinOp (..))
import Kitty.KTypes.C (C (..))
import qualified Kitty.KTypes.Libm as Libm
import Kitty.Plugin.Kitty (FModCat (..))
import Kitty.Recursion (hembed)

-- | doesn't require Real, used for overloading symbolics
--
--  __NB__: The constraint here is a hack for "Kitty.Cat" to get around a case where single-entry
--          dictionaries can be represented in Core as a `TyRepCo.Coercion` (`TyRepCo.AxiomInstCo`)
--          to their entry, which isn't handled by the plugin yet.
class Num a => FMod a where
  fmod :: a -> a -> a

instance FMod Double where fmod = Libm.libmFMod

instance FMod Float where fmod = Libm.libmFModf

instance (Floating a, FMod a) => FModCat (->) a where
  fmodK = uncurry fmod

instance (Floating (TargetOb a), FMod (TargetOb a)) => FModCat Cat a where
  fmodK = cat $ uncurry fmod

instance FMod (C Double) where
  fmod = liftA2 Libm.libmFMod

instance FMod (C Float) where
  fmod = liftA2 Libm.libmFModf

instance FMod (CExpr Double) where
  fmod x y = hembed $ FPBinOpF FPFmod x y

instance FMod (CExpr Float) where
  fmod x y = hembed $ FPBinOpF FPFmod x y
