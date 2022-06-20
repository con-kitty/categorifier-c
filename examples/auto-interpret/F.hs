{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module F (wrap_f, g) where

import qualified Categorifier.C.CExpr.Cat as C
import Categorifier.C.CExpr.Cat.TargetOb (TargetOb)
import Categorifier.C.CTypes.CGeneric (CGeneric)
import qualified Categorifier.C.CTypes.CGeneric as CG
import Categorifier.C.CTypes.GArrays (GArrays)
import Categorifier.C.KTypes.C (C)
import Categorifier.C.KTypes.FromIntegral (kFromIntegral)
import Categorifier.C.KTypes.KType1 (KType1)
import Categorifier.C.KTypes.SwitchCase (kIfThenElse)
import Categorifier.C.KTypes.TotalOrder ((.>))
import qualified Categorifier.Categorify as Categorify
import Categorifier.Client (deriveHasRep)
import Data.Int (Int32)
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

data Input = Input
  { iInt32 :: C Int32,
    iDouble :: C Double
  }
  deriving (Generic)

deriveHasRep ''Input

instance CGeneric Input

instance GArrays C Input

type instance TargetOb Input = TargetOb (CG.Rep Input ())

data Output = Output
  { oWord64 :: C Word64,
    oFloat :: C Float,
    oBool :: Bool
  }
  deriving (Generic)

deriveHasRep ''Output

instance CGeneric Output

instance GArrays C Output

type instance TargetOb Output = TargetOb (CG.Rep Output ())

f :: Input -> Output
f inp =
  Output
    { oWord64 = g (iInt32 inp),
      oFloat = realToFrac $ min (iDouble inp) 3.14,
      oBool = iDouble inp > 0
    }

-- Categorifier can't currently categorify `unsafePerformIO` or `IO` actions, but since
-- `g` is polymorphic, it can be "automatically interpreted", rather than inlined,
-- which means Categorifier never needs to process the unfolding of `g`.
--
-- In other words, Categorifier needs to compile `g @C` into `g @CExpr`. Since `g` is
-- polymorphic, no actual compilation is needed.
--
-- To enable automatic interpretation, this module needs to be compiled with
-- `-fplugin-opt Categorifier:autointerpreter:Categorifier.C.UnconCat.tryAutoInterpret`,
-- since this function defines how to automatically interpret eligible functions for the
-- C category.
--
-- "In the body of g" is printed during C code generation, i.e., the `putStrLn` runs
-- in Haskell and not in C.
--
-- Note that in this instance, `g` needs to be either exported from this module, or marked
-- `NOINLINE`. Otherwise, since `g` is used only once, GHC would inline it before the
-- Categorifier plugin runs, a step known as `PreInlineUnconditionally`.
g :: KType1 f => f Int32 -> f Word64
g x = unsafePerformIO $ do
  putStrLn "In the body of g"
  pure $ kIfThenElse (x .> 0) (kFromIntegral x + 5) 42

Categorify.functionOnly 'f [t|C.Cat|] []
