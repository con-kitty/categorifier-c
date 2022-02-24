-- | Branching and control flow for code-generatable expressions
module Categorifier.C.KTypes.Conditional
  ( KTernary (..),
    KSelect (..),
  )
where

import Categorifier.C.KTypes.BooleanLogic (KAnd)
import Categorifier.C.Prim (IsPrimitive)
import Data.Vector (Vector)
import Data.Word (Word8)

class KAnd f => KTernary f a | a -> f where
  kTernary :: f Bool -> a -> a -> a

-- | Legacy class used to implement 'Categorifier.C.KTypes.SwitchCase.switch' and
-- 'Categorifier.C.KTypes.SwitchCase.kIfThenElse'.
--
-- TODO(greg/peddie): clean this up.
-- TODO(ziyang): can we move KSelect to SwitchCase.hs and merge with KIf?
class KSelect f where
  selectList :: IsPrimitive a => [Vector (f a)] -> f Word8 -> Vector (f a)

  -- | used to implement 'Categorifier.C.KTypes.kIfThenElse', it better just be 0 or 1
  unsafeBoolToZeroOrOne :: f Bool -> f Word8
