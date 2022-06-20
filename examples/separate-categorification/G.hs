{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module G (g) where

import qualified Categorifier.C.CExpr.Cat as C
import Categorifier.C.KTypes.C (C)
import qualified Categorifier.Categorify as Categorify
import Data.Int (Int32)
import Data.Word (Word64)

g :: C Int32 -> C Word64
g x = if odd x then fromIntegral x + 5 else 42

Categorify.function 'g [t|C.Cat|] []
