{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This is similar to the @recursive-types@ example, except that the @Tree@ type
-- is monomorphic. This makes it slightly more complicated, because in the other
-- example, we could define the @TargetOb@ instance for @Tree@ as
--
--  @
--     type instance TargetOb (Tree a) = Tree (TargetOb a)
--  @
--
-- while here we have to create another data type, @TreeAux@, and use it to define
-- @TargetOb Tree@.
--
-- Note that the following won't work:
--
--  @
--     -- Won't work because Tree does not have CGeneric instance
--     type instance TargetOb Tree = TargetOb (CG.Rep Tree ())
--
--     -- Won't work because this doesn't terminate (Tree is recursive)
--     type instance TargetOb Tree = TargetOb (Categorifier.Client.Rep Tree)
--  @
--
-- The generated C code is the same as the @recursive-types@ eaxmple.
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

data Tree = Empty | Branch (C Int32) Tree Tree
  deriving (Generic)

Client.deriveHasRep ''Tree

data TreeAux = EmptyAux | BranchAux (TargetOb Int32) TreeAux TreeAux
  deriving (Generic)

Client.deriveHasRep ''TreeAux

type instance TargetOb Tree = TreeAux

f :: Input -> Output
f = h . g

g :: Input -> Tree
g inp = Branch (iInt32 inp) (Branch (fromIntegral (iInt64 inp)) Empty Empty) Empty

h :: Tree -> Output
h = \case
  Empty -> Output {oInt64 = 0, oBool = False}
  Branch x l r ->
    Output
      { oInt64 = fromIntegral x + oInt64 (h l) + oInt64 (h r),
        oBool = True
      }

fCategorified :: Input `C.Cat` Output
fCategorified = Categorify.expression f
