{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module F (fCategorified) where

import qualified Categorifier.C.CExpr.Cat as C
import Categorifier.C.CExpr.Cat.TargetOb (TargetOb)
import Categorifier.C.CTypes.CGeneric (CGeneric)
import qualified Categorifier.C.CTypes.CGeneric as CG
import Categorifier.C.CTypes.GArrays (GArrays)
import Categorifier.C.KTypes.C (C)
import Categorifier.C.KTypes.KEnum (KEnum, toKEnum)
import Categorifier.C.KTypes.Sum.Maybe (KMaybe, kJust, kNothing)
import qualified Categorifier.Categorify as Categorify
import Categorifier.Client (deriveHasRep)
import Data.Int (Int32)
import Data.Word (Word64)
import GHC.Generics (Generic)

data Pet = Cat | Dog | Capybara
  deriving (Bounded, Enum, Eq, Ord, Generic)

deriveHasRep ''Pet

-- The following instances are only needed when using `Pet` directly. It isn't needed when
-- wrapping `Pet` in a `KEnum`, as the code below does.

-- instance CGeneric Pet

-- instance GArrays C Pet

-- The `TargetOb` instance for an enum type should be defined via
-- `Categorifier.Client.Rep`, rather than `CG.Rep`.
-- type instance TargetOb Pet = TargetOb (Categorifier.Client.Rep Pet)

data Input = Input
  { iInt32 :: C Int32,
    -- | To use an enum type, in general, we need to wrap it with `KEnum`.
    --
    -- It is possible to use the bare enum type (i.e., `Pet`) directly, as long
    -- as the `IfCat` instance is not needed (in other words, `Pet` does not appear
    -- in any @if@ branch). In this example, the `IfCat` instance is needed due
    -- to `getCompanion`.
    --
    -- To use `Pet` directly, we also need to define its `CGeneric`, `GArrays`
    -- and `TargetOb` instances. See the comment above.
    iEnum :: KEnum C Pet
  }
  deriving (Generic)

deriveHasRep ''Input

instance CGeneric Input

instance GArrays C Input

type instance TargetOb Input = TargetOb (CG.Rep Input ())

data Output = Output
  { oWord64 :: C Word64,
    oEnum :: (KEnum C Pet, KEnum C Pet),
    -- | To use a `Maybe` type, wrap it with `KMaybe`.
    oMaybe :: KMaybe C (C Int32)
  }
  deriving (Generic)

deriveHasRep ''Output

instance CGeneric Output

instance GArrays C Output

type instance TargetOb Output = TargetOb (CG.Rep Output ())

f :: Input -> Output
f inp =
  Output
    { -- Currently, `fromKEnum` is not supported by the plugin. To compare a `KEnum` with
      -- an enum type, avoid using `fromKEnum` to convert the former to an enum, rather,
      -- use `toKEnum` to convert the latter to a `KEnum`.
      oWord64 = if iEnum inp == toKEnum Cat then fromIntegral (iInt32 inp) + 5 else 42,
      oEnum = getCompanion (iEnum inp),
      oMaybe = if iInt32 inp > 0 then kJust (iInt32 inp * 2) else kNothing
    }

getCompanion :: KEnum C Pet -> (KEnum C Pet, KEnum C Pet)
getCompanion p
  | p == toKEnum Cat = (p, toKEnum Dog)
  | p == toKEnum Dog = (p, toKEnum Capybara)
  | otherwise = (p, toKEnum Cat)

fCategorified :: Input `C.Cat` Output
fCategorified = Categorify.expression f
