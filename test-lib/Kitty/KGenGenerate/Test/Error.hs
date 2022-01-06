{-# LANGUAGE QuasiQuotes #-}

-- | Error tools for random expression test generation.
module Kitty.KGenGenerate.Test.Error
  ( KSelectIndexError (..),
    throwKSelectIndexError,
  )
where

import Control.Exception (throw)
import GHC.Stack (CallStack)
import qualified Kitty.Common.IO.Exception as Exception
import PyF (fmt)

-- | The `Kitty.KTypes.Conditional.KSelect` class's interface leaves a lot of ways for things to go
-- wrong.  To match SBV's behavior, it uses lists in many places where `Data.List.NonEmpty.NonEmpty`
-- or a fixed-size `Data.Distributive.Distributive` functor might be more appropriate.
data KSelectIndexError
  = KSelectMissingCase
  | KSelectMismatchedProductCases Int Int
  | KSelectInvalidReturn Int
  | KSelectInvalidArgument Int

displayKSelectIndexError :: KSelectIndexError -> String
displayKSelectIndexError err =
  [fmt|Internal error in KSelect instance for random testing:\n\t|]
    <> case err of
      KSelectMissingCase -> [fmt|No case alternatives received!|]
      KSelectMismatchedProductCases lenA lenB ->
        [fmt|Two instances given different lengths {show lenA} and {show lenB}!|]
      KSelectInvalidArgument len ->
        [fmt|Expected scalar input arguments; received {show len} values instead!|]
      KSelectInvalidReturn len ->
        [fmt|inner `selectList` returned nonscalar of length {show len}!|]

-- | An internal "impossible" situation occurred in a `Kitty.KTypes.Conditional.KSelect` instance;
-- throw an exception.
throwKSelectIndexError :: CallStack -> KSelectIndexError -> a
throwKSelectIndexError stack =
  throw
    . flip Exception.CallStacked stack
    . Exception.AsException displayKSelectIndexError
