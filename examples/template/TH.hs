{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TH where

import qualified Categorifier.C.CExpr.Cat as C
import Categorifier.C.CExpr.Cat.TargetOb (TargetOb)
import qualified Categorifier.C.CExpr.File as CExpr (FunctionText (..))
import qualified Categorifier.C.CExpr.IO as CExpr (layoutOptions)
import Categorifier.C.CExpr.Types.Core (CExpr)
import Categorifier.C.Codegen.FFI.Spec (SBVFunCall)
import Categorifier.C.Generate (generateCFunction)
import Categorifier.C.KTypes.C (C)
import Categorifier.C.KTypes.CExpr.Generate (generateCExprFunction)
import Categorifier.C.PolyVec (PolyVec, pdevectorize, pvectorize, pvlengths)
import Categorifier.C.Prim (ArrayCount, Arrays)
import qualified Categorifier.Common.IO.Exception as Exception
import Control.Monad ((<=<))
import Data.Functor.Compose (Compose (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc.Render.Text as Prettyprint
import Data.Vector (Vector)
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
    runIO,
  )

arr :: Type -> Type -> Type
arr t1 t2 = ArrowT `AppT` t1 `AppT` t2

multiArgs :: [Type] -> Type -> Type
multiArgs inputArgs output =
  foldr arr output inputArgs

arraysFun ::
  forall i o.
  (PolyVec CExpr (TargetOb i), PolyVec CExpr (TargetOb o)) =>
  (i `C.Cat` o) ->
  Arrays (Compose Vector CExpr) ->
  IO (Arrays (Compose Vector CExpr))
arraysFun f =
  Exception.throwIOLeft . pvectorize . C.lowerCat f <=< Exception.throwIOLeft . pdevectorize

inputDims :: forall a. PolyVec C a => Proxy a -> Arrays ArrayCount
inputDims = pvlengths (Proxy @C)

embedFunction ::
  forall i o.
  (PolyVec CExpr (TargetOb i), PolyVec CExpr (TargetOb o), PolyVec C i) =>
  Text ->
  (i `C.Cat` o) ->
  Q [Dec]
embedFunction name f = do
  let cname = "c_" <> name
  codeC <-
    runIO $ do
      x <- generateCExprFunction name (inputDims $ Proxy @i) (arraysFun f)
      case x of
        Left e -> error "error"
        Right (CExpr.FunctionText _ srcText) ->
          pure $ Prettyprint.renderStrict $ CExpr.layoutOptions srcText
  -- generateCFunction name f
  addForeignSource LangC (T.unpack codeC)
  c_sig <- [t|SBVFunCall|]
  pure $
    [ ForeignD $
        ImportF
          CCall
          Safe
          (T.unpack name)
          (mkName (T.unpack cname))
          c_sig
    ]
