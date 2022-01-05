{-# LANGUAGE MonoLocalBinds #-}

module Kitty.KTypes.SwitchCase
  ( KIf (..),
    switch,
    switch',
    unsafeIndex,
    kIfThenElseChain,
  )
where

import qualified Barbies
import Data.Functor.Compose (Compose (..))
import qualified Data.Vector as V
import Data.Word (Word8)
import qualified Kitty.Common.IO.Exception as Exception
import Kitty.KTypes.Conditional (KSelect (..))
import Kitty.KTypes.Equality (KEq (..))
import Kitty.KTypes.KEnum (KEnum (..))
import Kitty.KTypes.KLiteral (KLiteral (..), true)
import Kitty.PolyVec (PolyVec, pdevectorize, pvectorize)
import Kitty.Prim (Arrays, IsPrimitive)

class KSelect f => KIf f where
  -- | implement `kIfThenElse` by converting a bool to a KEnum.
  -- This is dangerous in general, but fine here.
  -- This is the sole purpose for 'unsafeBoolToZeroOrOne'
  kIfThenElse :: forall b. PolyVec f b => f Bool -> b -> b -> b
  kIfThenElse boolIndex tru fls = unsafeIndex [fls, tru] (unsafeBoolToZeroOrOne boolIndex)

switch ::
  forall f a b.
  (KSelect f, Bounded a, Enum a, PolyVec f b) =>
  KEnum f a ->
  (a -> b) ->
  b
switch (KEnum index) toBranch = unsafeIndex allOutputs index
  where
    allEnums :: [a]
    allEnums = enumFrom minBound
    allOutputs :: [b]
    allOutputs = fmap toBranch allEnums

-- | Version of 'switch' that chains 'kIfThenElse's and uses a default argument instead of doing an
-- array index on all options. This is more efficient when you want to pattern match on a couple
-- possibilities but accept the default most of the time.
switch' :: forall a b f. (KEq f a, KIf f, PolyVec f b) => a -> [(a, b)] -> b -> b
switch' inputSwitchIsLookingFor optionList default' = foldr matchOr default' optionList
  where
    matchOr :: (a, b) -> b -> b
    matchOr (thisInput, thisOutput) earliestMatchThusFar =
      kIfThenElse
        (thisInput .== inputSwitchIsLookingFor)
        -- Found an earlier match, so take it.
        thisOutput
        earliestMatchThusFar

-- | Internal function for doing @select@-like operations.
unsafeIndex :: forall f a. (KSelect f, PolyVec f a) => [a] -> f Word8 -> a
unsafeIndex allOutputs0 index = either Exception.impureThrow id $ pdevectorize outputs
  where
    processOutputs :: [Arrays (Compose V.Vector f)] -> Arrays (Compose V.Vector f)
    processOutputs = Barbies.bmapC @IsPrimitive selectPrimList . Barbies.bdistribute
      where
        selectPrimList ::
          forall c.
          IsPrimitive c =>
          Compose [] (Compose V.Vector f) c ->
          Compose V.Vector f c
        selectPrimList (Compose allPrimOutputs) =
          Compose $ selectList (getCompose <$> allPrimOutputs) index
    outputs :: Arrays (Compose V.Vector f)
    outputs = either Exception.impureThrow processOutputs $ traverse pvectorize allOutputs0

-- | Grabs first element of list which has a 'True' first element.
-- If all are 'False' returns a default.
kIfThenElseChain ::
  forall a f.
  (KEq f (f Bool), KLiteral f Bool, KIf f, PolyVec f a) =>
  [(f Bool, a)] ->
  a ->
  a
kIfThenElseChain = switch' true
