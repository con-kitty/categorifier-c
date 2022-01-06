-- | Some helper functions for calling C functions with the arrays calling convention.
module Kitty.Codegen.FFI.ArraysCC
  ( fromArraysCC,
    unsafeFromArraysCC,
  )
where

import qualified Barbies
import Data.Functor.Compose (Compose (..))
import Data.Proxy (Proxy (..))
import Data.Vector (Vector)
import Kitty.Codegen.FFI.Call (callSBVCFunction')
import Kitty.Codegen.FFI.Spec (SBVFunCall, mkSpec)
import qualified Kitty.Common.IO.Exception as Exception
import Kitty.KTypes.C (C (..))
import Kitty.KTypes.KLiteral (KLiteral (..))
import Kitty.PolyVec (FromArraysError, PolyVec, ToArraysError, pdevectorize, pvectorize)
import Kitty.Prim (Arrays, IsPrimitive)
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
