-- | Primitive types for all Kittyhawk C code generation systems.
--
-- === Performance note
--
-- A bunch of type class instances were recently converted to derived instances (from handwritten
-- ones) using the new @-XDerivingVia@ feature in GHC 8.6.  This has the unfortunate side effect of
-- removing @INLINEABLE@ pragmas that previously qualified many of the methods.  If needed, these
-- can be re-exposed for other modules to inline by passing the @-fexpose-all-unfoldings@ and
-- @-fspecialize-aggressively@ flags to GHC.
module Kitty.Prim
  ( module Base,
    module Helpers,
    module Patterns,
    module ArrayName,
    module ArrayCount,
    module ArrayVec,
    module ArrayLens,
    module ConstArrays,
  )
where

import Kitty.Prim.ArrayCount as ArrayCount
import Kitty.Prim.ArrayLens as ArrayLens
import Kitty.Prim.ArrayName as ArrayName
import Kitty.Prim.ArrayVec as ArrayVec
import Kitty.Prim.Base as Base
import Kitty.Prim.ConstArrays as ConstArrays
import Kitty.Prim.Helpers as Helpers
import Kitty.Prim.Patterns as Patterns
