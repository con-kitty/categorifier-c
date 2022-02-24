{-# LANGUAGE MonoLocalBinds #-}

-- | Equality (including 'NaN == NaN') for 'PolyVec' types.
module Categorifier.C.KGenGenerate.Test.Equality
  ( polyEqNaN,
  )
where

import qualified Barbies
import Categorifier.C.KGenGenerate.Test.GenerateTyped (arraysEq)
import Categorifier.C.KTypes.C (C (..))
import Categorifier.C.PolyVec (PolyVec, pvectorize)
import Categorifier.C.Prim (Arrays)
import Data.Functor.Compose (Compose (..))
import qualified Data.Vector as V

polyEqNaN :: forall a. PolyVec C a => a -> a -> Bool
polyEqNaN a b =
  case (,) <$> pvectorize a <*> pvectorize b of
    Left _ -> False
    Right (x, y) -> arraysEq (conv x) (conv y)
      where
        conv :: Arrays (Compose V.Vector C) -> Arrays V.Vector
        conv = Barbies.bmap (fmap unsafeC . getCompose)
