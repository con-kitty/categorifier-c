-- | Some helper functions for calling C functions with the arrays calling convention.
module Categorifier.C.Codegen.FFI.ArraysCC
  ( fromArraysCC,
    unsafeFromArraysCC,
  )
where

import qualified Barbies
import Categorifier.C.Codegen.FFI.Call (callSBVCFunction')
import Categorifier.C.Codegen.FFI.Spec (SBVFunCall, mkSpec)
import Categorifier.C.KTypes.C (C (..))
import Categorifier.C.KTypes.KLiteral (KLiteral (..))
import Categorifier.C.PolyVec (FromArraysError, PolyVec, ToArraysError, pdevectorize, pvectorize)
import Categorifier.C.Prim (Arrays, IsPrimitive)
import qualified Categorifier.Common.IO.Exception as Exception
import Data.Functor.Compose (Compose (..))
import Data.Proxy (Proxy (..))
import Data.Vector (Vector)
import System.IO.Unsafe (unsafePerformIO)

-- | Convert an FFI binding to a C function with the arrays calling convention into
-- a Haskell function with domain types.
fromArraysCC ::
  forall a b.
  (PolyVec C a, PolyVec C b) =>
  Proxy (a -> b) ->
  SBVFunCall ->
  a ->
  IO b
fromArraysCC Proxy c_fun a =
  Exception.throwIOLeft . fromArrays
    =<< callSBVCFunction' c_fun (mkSpec (Proxy @a) (Proxy @b))
    =<< Exception.throwIOLeft (toArrays a)

-- | Like `fromArraysCC` but returns a pure type.
unsafeFromArraysCC ::
  forall a b.
  (PolyVec C a, PolyVec C b) =>
  Proxy (a -> b) ->
  SBVFunCall ->
  a ->
  b
unsafeFromArraysCC = ((unsafePerformIO .) .) . fromArraysCC

toArrays :: PolyVec C a => a -> Either ToArraysError (Arrays Vector)
toArrays = fmap (Barbies.bmap (fmap unsafeC . getCompose)) . pvectorize

fromArrays :: PolyVec C a => Arrays Vector -> Either FromArraysError a
fromArrays = pdevectorize @_ @C . Barbies.bmapC @IsPrimitive (Compose . fmap kliteral)
