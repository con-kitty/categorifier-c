{-# LANGUAGE MonoLocalBinds #-}

-- | Equality (including 'NaN == NaN') for 'PolyVec' types.
module Kitty.KGenGenerate.Test.Equality
  ( polyEqNaN,
  )
where

import qualified Barbies
import Data.Functor.Compose (Compose (..))
import qualified Data.Vector as V
import Kitty.KGenGenerate.Test.GenerateTyped (arraysEq)
import Kitty.KTypes.C (C (..))
import Kitty.PolyVec (PolyVec, pvectorize)
import Kitty.Prim (Arrays)

polyEqNaN :: forall a. PolyVec C a => a -> a -> Bool
polyEqNaN a b =
  case (,) <$> pvectorize a <*> pvectorize b of
    Left _ -> False
    Right (x, y) -> arraysEq (conv x) (conv y)
      where
        conv :: Arrays (Compose V.Vector C) -> Arrays V.Vector
        conv = Barbies.bmap (fmap unsafeC . getCompose)
