{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This example demonstrates the usage of a recursive type, @Tree a@, and a recursive
-- function, @h@.
--
-- There are some restrictions when using sum types (recursive or not, except enum types
-- and @Maybe@ types represented by @KEnum@ and @KMaybe):
--
-- * Sum types cannot be part of @Input@ and @Output@ when generating C code from a Haskell
--   function of type @Input -> Output@. They can be used internally by the function.
--
-- * In addition, sum types cannot appear in a branch, due to the lack of @IfCat@ instances.
--   In other words, this is not allowed:
--   @
--      tree = if odd input then Empty else Branch 1 Empty Empty
--   @
--
-- It is possible to remove these restrictions for non-recursive sum types, but it is not
-- clear whether this is possible for recursive or mutually recursive types.
module F (fCategorified) where

import qualified Categorifier.C.CExpr.Cat as C
import Categorifier.C.CExpr.Cat.TargetOb (TargetOb)
import Categorifier.C.CTypes.CGeneric (CGeneric)
import qualified Categorifier.C.CTypes.CGeneric as CG
import Categorifier.C.CTypes.GArrays (GArrays)
import Categorifier.C.KTypes.C (C)
import qualified Categorifier.Categorify as Categorify
import qualified Categorifier.Client as Client
import Data.Int (Int32, Int64)
import GHC.Generics (Generic)

data Input = Input
  { iInt32 :: C Int32,
    iInt64 :: C Int64
  }
  deriving (Generic)

Client.deriveHasRep ''Input

instance CGeneric Input

instance GArrays C Input

type instance TargetOb Input = TargetOb (CG.Rep Input ())

data Output = Output
  { oInt64 :: C Int64,
    oBool :: Bool
  }
  deriving (Generic)

Client.deriveHasRep ''Output

instance CGeneric Output

instance GArrays C Output

type instance TargetOb Output = TargetOb (CG.Rep Output ())

data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Generic)

Client.deriveHasRep ''Tree

type instance TargetOb (Tree a) = Tree (TargetOb a)

f :: Input -> Output
f = h . g

g :: Input -> Tree (C Int32)
g inp = Branch (iInt32 inp) (Branch (fromIntegral (iInt64 inp)) Empty Empty) Empty

h :: Tree (C Int32) -> Output
h = \case
  Empty -> Output {oInt64 = 0, oBool = False}
  Branch x l r ->
    Output
      { oInt64 = fromIntegral x + oInt64 (h l) + oInt64 (h r),
        oBool = True
      }

fCategorified :: Input `C.Cat` Output
fCategorified = Categorify.expression f
