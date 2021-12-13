module Kitty.KTypes.FMod
  ( FMod (..),
  )
where

import qualified Kitty.KTypes.Libm as Libm

-- | doesn't require Real, used for overloading symbolics
--
--  __NB__: The constraint here is a hack for "Kitty.Cat" to get around a case where single-entry
--          dictionaries can be represented in Core as a `TyRepCo.Coercion` (`TyRepCo.AxiomInstCo`)
--          to their entry, which isn't handled by the plugin yet.
class Num a => FMod a where
  fmod :: a -> a -> a

instance FMod Double where fmod = Libm.libmFMod

instance FMod Float where fmod = Libm.libmFModf
