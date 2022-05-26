{-# LANGUAGE TemplateHaskell #-}

module TH where

-- import Control.Monad (replicateM)

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (Ptr)
import Language.Haskell.TH.Syntax
  ( Callconv (..),
    Dec (ForeignD),
    Foreign (ImportF),
    ForeignSrcLang (LangC),
    Q,
    Safety (Safe),
    Type (..),
    addForeignSource,
    mkName,
  )

unitTy :: Type
unitTy = TupleT 0

ptrTy :: Type -> Type
ptrTy typ = ConT (mkName "Ptr") `AppT` typ

-- ptrUnitTy :: Type
-- ptrUnitTy = ptrTy `AppT` unitTy

arr :: Type -> Type -> Type
arr t1 t2 = ArrowT `AppT` t1 `AppT` t2

multiArgs :: [Type] -> Type -> Type
multiArgs inputArgs output =
  foldr arr output inputArgs

genInputArgSet :: Q [Type]
genInputArgSet =
  sequenceA
    [ -- input
      [t|Ptr Bool|],
      [t|Ptr Int8|],
      [t|Ptr Int16|],
      [t|Ptr Int32|],
      [t|Ptr Int64|],
      [t|Ptr Word8|],
      [t|Ptr Word16|],
      [t|Ptr Word32|],
      [t|Ptr Word64|],
      [t|Ptr Float|],
      [t|Ptr Double|],
      -- output
      [t|Ptr Bool|],
      [t|Ptr Int8|],
      [t|Ptr Int16|],
      [t|Ptr Int32|],
      [t|Ptr Int64|],
      [t|Ptr Word8|],
      [t|Ptr Word16|],
      [t|Ptr Word32|],
      [t|Ptr Word64|],
      [t|Ptr Float|],
      [t|Ptr Double|]
    ]

--     replicateM
--    22

-- replicate 22 (ptrTy unitTy)

myFunction :: Q [Dec]
myFunction = do
  addForeignSource
    LangC
    "#include <stdio.h>\n\
    \void test( void ) {\n\
    \  printf (\"clang version: %d\\n\", __clang_major__);\n\
    \}\n"

  c_simple_example_sig <-
    multiArgs <$> genInputArgSet <*> [t|IO ()|]
  pure $
    [ ForeignD $
        ImportF CCall Safe "test" (mkName "c_test") (AppT (ConT (mkName "IO")) unitTy),
      ForeignD $
        ImportF
          CCall
          Safe
          "simple_example"
          (mkName "c_simple_example")
          c_simple_example_sig
    ]
