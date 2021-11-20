-- | Lens utilities.
module Kitty.Lens
  ( runLens,
    overIsoAction,
  )
where

import Control.Lens (AnIso, withIso)
import Control.Monad.Trans.State.Lazy (State, execState)

runLens :: a -> State a b -> a
runLens = flip execState

--get :: Xyz Double -> Double
--get xyz = xyz ^. x'
--
--set :: Xyz Double -> Double -> Xyz Double
--set xyz0 xnew = (x' .~ xnew) xyz0
--
--modify :: Xyz Double -> (Double -> Double) -> Xyz Double
--modify xyz0 f = (x' %~ f) xyz0
--
--go :: State a ()
--go = do
--  x' .= 2
--  y' .= 3

-- | This is like 'Control.Lens.over', specialized to 'Control.Lens.Iso's, but it allows the
-- user-supplied function to have side effects.  The main use is to pass it an 'Control.Lens.Iso'
-- for a @newtype@ wrapper and a traversal.
--
-- To use this with a newtype directly, rather than having to create your own 'Control.Lens.Iso' by
-- hand, check out 'Control.Lens._Wrapping' and 'Control.Lens._Unwrapping', e.g.  If you have used
-- 'Control.Lens.TH.makeWrapped' on your newtype, you can do things like the following:
--
--    overIsoAction (_Unwrapping HK1FlipL) yourFunction itsArgument
overIsoAction :: Functor f => AnIso s t a b -> (a -> f b) -> s -> f t
overIsoAction i f = withIso i go
  where
    go applyNewtype removeNewtype = fmap removeNewtype . f . applyNewtype
