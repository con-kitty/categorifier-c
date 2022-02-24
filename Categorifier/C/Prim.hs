-- | Primitive types for all C code generation systems.
--
-- === Performance note
--
-- A bunch of type class instances were recently converted to derived instances (from handwritten
-- ones) using the new @-XDerivingVia@ feature in GHC 8.6.  This has the unfortunate side effect of
-- removing @INLINEABLE@ pragmas that previously qualified many of the methods.  If needed, these
-- can be re-exposed for other modules to inline by passing the @-fexpose-all-unfoldings@ and
-- @-fspecialize-aggressively@ flags to GHC.
module Categorifier.C.Prim
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

import Categorifier.C.Prim.ArrayCount as ArrayCount
import Categorifier.C.Prim.ArrayLens as ArrayLens
import Categorifier.C.Prim.ArrayName as ArrayName
import Categorifier.C.Prim.ArrayVec as ArrayVec
import Categorifier.C.Prim.Base as Base
import Categorifier.C.Prim.ConstArrays as ConstArrays
import Categorifier.C.Prim.Helpers as Helpers
import Categorifier.C.Prim.Patterns as Patterns
