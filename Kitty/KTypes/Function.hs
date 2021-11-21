{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides an abstract interface to function calls in the K DSL.
module Kitty.KTypes.Function
  ( -- * Functions on 'Arrays' instead of user-defined Haskell types
    ArraysVec,
    KFunCall (..),

    -- * Variadic function calls using 'PolyVec'
    kFunctionCall,

    -- * External C function calls
    Callee (..),
    kForeignFunctionCall,
    KForeignFunctionCall (..),

    -- ** Helper machinery
    accumulateInputsAndCallArraysFunction,
    IsFunCall,
    KVariadicDevecAndApply (..),
    KVariadicVectorizeFunctionInputs (..),
    VariadicFunctionCallError,
    MArraysVec,
    MArraysVecFiller,
    VectorizeT,
  )
where

import qualified Barbies
import Control.Monad (unless)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, ExceptT)
import qualified Control.Monad.Trans.Except as Except
import Data.Functor.Compose (Compose (..))
import Data.Functor.Product (Product (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (TypeRep, typeRep)
import Data.Vector (Vector)
import qualified Data.Vector as V (length, unsafeFreeze)
import qualified Data.Vector.Mutable as M (unsafeNew)
import Kitty.Common.IO.Exception (Exception (..), impureThrow)
import Kitty.PolyVec
  ( FromArraysError,
    PolyVec,
    ToArraysError,
    pdevectorize,
    pdevectorizeIncremental,
    pvectorize,
    pvectorizeIncremental,
    pvlengths,
  )
import Kitty.Prim (ArrayCount (..), ArrayMVec (..), Arrays, IsPrimitive)
import qualified Kitty.Show as Show
import PyF (fmt)

arraysLength :: ArraysVec f -> Arrays ArrayCount
arraysLength = Barbies.bmap (ArrayCount . V.length . getCompose)

-- | An 'Arrays' of immutable vectors
type ArraysVec f = Arrays (Compose Vector f)

-- | An 'Arrays' of mutable vectors
type MArraysVec s f = Arrays (ArrayMVec Vector s f)

class KFunCall f where
  kCallFunctionWithSpec ::
    -- | Function name in C
    String ->
    -- | Input spec
    Arrays ArrayCount ->
    -- | Output spec
    Arrays ArrayCount ->
    -- | User function on 'Arrays'
    (ArraysVec f -> ArraysVec f) ->
    -- | Inputs to compileable function
    ArraysVec f ->
    -- | Outputs from compileable function
    ArraysVec f

data ArrayCountMismatch = ArrayCountMismatch
  { acmExpectedArrayCounts :: Arrays ArrayCount,
    acmSawArrayCounts :: Arrays ArrayCount
  }
  deriving (Eq, Show, Ord)

diffArrayCounts :: Arrays ArrayCount -> Arrays ArrayCount -> [(TypeRep, Int)]
diffArrayCounts expected saw =
  filter ((/= 0) . snd) . Barbies.bfoldMapC @IsPrimitive go $ Barbies.bzip expected saw
  where
    go ::
      forall a.
      IsPrimitive a =>
      Product ArrayCount ArrayCount a ->
      [(TypeRep, Int)]
    go (Pair (ArrayCount expect) (ArrayCount see)) =
      pure (typeRep (Proxy @a), expect - see)

prettyArrayCountMismatch :: ArrayCountMismatch -> String
prettyArrayCountMismatch (ArrayCountMismatch expected saw) =
  [fmt|
  Array mismatch
  Diff (expected - saw):
{unlines . fmap show $ diffArrayCounts expected saw}
|]

data VariadicFunctionCallError
  = LeftoverArrayData (Arrays ArrayCount)
  | CountMismatch ArrayCountMismatch
  | PDevectorizeError FromArraysError
  | PVectorizeError ToArraysError

-- | __TODO__: This should go away, see https://kitty-hawk.atlassian.net/browse/SW-2658
instance Show VariadicFunctionCallError where
  showsPrec p = \case
    LeftoverArrayData a -> Show.appPrec p "LeftoverArrayData" [Show.arg a]
    CountMismatch a -> Show.appPrec p "CountMismatch" [showString $ prettyArrayCountMismatch a]
    PDevectorizeError e -> Show.appPrec p "PDevectorizeError" [Show.arg e]
    PVectorizeError e -> Show.appPrec p "PVectorizeError" [Show.arg e]

-- | __TODO__: This should go away, see https://kitty-hawk.atlassian.net/browse/SW-2658
instance Exception VariadicFunctionCallError

-- | The monad we use for vectorizing all our function arguments in a single mutable write pass.
type VectorizeT s a = ExceptT VariadicFunctionCallError (ST s) a

-- | An action that fills in the given 'MArraysVec' with some argument data.
type MArraysVecFiller f = forall s. MArraysVec s f -> VectorizeT s ()

-- | Allocate a set of 'MArraysVec' corresponding to the @'Arrays' 'ArrayCount'@ describing their
-- sizes.
emptyArrays :: forall s f. Arrays ArrayCount -> VectorizeT s (MArraysVec s f)
emptyArrays inputCounts = Barbies.btraverse allocateArray inputCounts
  where
    -- Allocation
    allocateArray :: forall a. ArrayCount a -> VectorizeT s (ArrayMVec Vector s f a)
    allocateArray = lift . fmap ArrayMVec . M.unsafeNew . unArrayCount

-- | Freeze a set of 'MArraysVec' into immutable 'Arrays'.  The 'MArraysVec' __must not be used__
-- after calling this function.
unsafeFreezeArrays :: forall s f. MArraysVec s f -> VectorizeT s (ArraysVec f)
unsafeFreezeArrays = Barbies.btraverse unsafeFreezeArray
  where
    -- Freezing mutable arrays for use in pure code
    unsafeFreezeArray :: forall a. ArrayMVec Vector s f a -> VectorizeT s (Compose Vector f a)
    unsafeFreezeArray = lift . fmap Compose . V.unsafeFreeze . unArrayMVec

-- Is @a@ a function call type (''FunCall') or something else (''FullyApplied')?
data FunCall
  = FunCall
  | FullyApplied

-- | Internal machinery of 'kFunctionCall' (see source for explanations).
type family IsFunCall a where
  IsFunCall (x -> y) = 'FunCall
  IsFunCall v = 'FullyApplied

-- | Given a proxy to the @f@ container type (e.g. 'Kitty.CExpr.Types.Core.CExpr' or
-- 'Kitty.KTypes.C.C') @fProxy@, a function name @functionName@ and a Haskell function
-- @myfunction@,
--
-- > 'kFunctionCall' @fProxy functionName myFunction@
--
-- produces a code-generatable function call node with the same type as @myFunction@.  Any calls to
-- the function returned from 'kFunctionCall' will be code-generated as calls to a separately
-- generated C function corresponding to @myFunction@; otherwise, they are the same as calling
-- @myFunction@.
--
-- __As long as all the input and argument types can be 'PolyVec'torized__, you should be able to
-- pass this any function type you like.
kFunctionCall ::
  forall f isFunCall a x.
  ( KFunCall f,
    KVariadicDevecAndApply isFunCall f (a -> x),
    KVariadicVectorizeFunctionInputs isFunCall f (a -> x),
    isFunCall ~ IsFunCall (a -> x)
  ) =>
  -- | @fProxy@, proxy to the @f@ container type
  Proxy f ->
  -- | @functionName@, the name that will appear in generated C code
  String ->
  -- | @myFunction@, the Haskell function you want to designate as a distinct
  -- function-call node
  (a -> x) ->
  -- | Result corresponding to a C function call when code is generated.
  (a -> x)
kFunctionCall Proxy name f =
  vectorizeFunctionInputs
    (Proxy @isFunCall)
    (accumulateInputsAndCallArraysFunction functionNode)
    mempty
    (const $ pure ())
  where
    -- This function operates in two passes.  The first pass computes @arrayFunction@ by using the a
    -- chain of calls to 'devectorizeAndApply' on the user function.  The second pass computes the
    -- result function by passing what we have of the @functionNode@ into a chain of calls to
    -- 'revectorize.  We end up with the same function type as we were originally given, but with a
    -- layer in between where the inputs and outputs are all 'Arrays' of data.  It's this layer that
    -- we need to do code-generation: 'kCallFunctionWithSpec' only needs to operate on these
    -- intermediate 'Arrays'.
    functionNode :: Arrays ArrayCount -> Arrays ArrayCount -> ArraysVec f -> ArraysVec f
    functionNode inSpec outSpec =
      kCallFunctionWithSpec name inSpec outSpec arrayFunction
    arrayFunction :: ArraysVec f -> ArraysVec f
    arrayFunction inputArrays = either impureThrow id $ Except.runExcept outputArrays
      where
        outputArrays :: Except VariadicFunctionCallError (ArraysVec f)
        outputArrays = devectorizeAndApply (Proxy @isFunCall) inputArrays f

data Callee
  = -- | header file from which the callee is imported
    Imported
      Text
  | HandWritten
      [Text]
      -- ^ header files
      (Maybe Text)
      -- ^ function declaration
      Text
      -- ^ function definition

-- | Like `kFunctionCall`, but instead of generating and calling a C function, it calls
-- an existing C function.
--
-- This is the function version of `kffcall`, to make it easier for categorization.
kForeignFunctionCall ::
  forall f isFunCall a b.
  ( KForeignFunctionCall f,
    KVariadicVectorizeFunctionInputs isFunCall f (KFFCall f a -> KFFCall f b),
    isFunCall ~ IsFunCall (KFFCall f a -> KFFCall f b)
  ) =>
  Proxy f ->
  -- | The name of the callee
  Text ->
  -- | The callee
  Callee ->
  -- | A Haskell function that is identical to the C function being called. This function is
  -- only used for evaluating the caller in Haskell. It is not involved in generating the
  -- caller to C. If evaluating the caller in Haskell is needed, then it does not need to
  -- be provided.
  Maybe (a -> b) ->
  (KFFCall f a -> KFFCall f b)
kForeignFunctionCall = kffcall

class KForeignFunctionCall (f :: Type -> Type) where
  type KFFCall f (a :: Type) :: Type

  kffcall ::
    forall isFunCall a b.
    ( KVariadicVectorizeFunctionInputs isFunCall f (KFFCall f a -> KFFCall f b),
      isFunCall ~ IsFunCall (KFFCall f a -> KFFCall f b)
    ) =>
    Proxy f ->
    Text ->
    Callee ->
    Maybe (a -> b) ->
    (KFFCall f a -> KFFCall f b)

-- | This helper function allocates arrays, populates them using the given filling action, and calls
-- the arrays function.
accumulateInputsAndCallArraysFunction ::
  forall f.
  -- | Arrays function
  (Arrays ArrayCount -> Arrays ArrayCount -> ArraysVec f -> ArraysVec f) ->
  -- | Array-filling action
  MArraysVecFiller f ->
  -- | Input counts
  Arrays ArrayCount ->
  -- | Output counts
  Arrays ArrayCount ->
  ArraysVec f
accumulateInputsAndCallArraysFunction arraysFunction marraysAction inputCounts outputCounts =
  arraysFunction inputCounts outputCounts functionInputs
  where
    vectorizedFunctionInputAction :: forall s. VectorizeT s (ArraysVec f)
    vectorizedFunctionInputAction = do
      marrays <- emptyArrays inputCounts
      marraysAction marrays
      unsafeFreezeArrays marrays
    -- The result of the recursive pass: the inputs to the 'kCallFunctionWithSpec' call.
    functionInputs :: ArraysVec f
    functionInputs = either impureThrow id $ runST $ Except.runExceptT vectorizedFunctionInputAction

-- This class describes variadic devectorization.  Given a user function, we want to pull its
-- arguments from a set of input arrays one at a time and devectorize into a value of the original
-- Haskell type, then apply the function to this value.  By doing recursion at the type level on the
-- function arrow and 'IsFunCall', we can incrementally divide up a single set of input arrays into
-- the appropriate structure for the user function.  In the base case, where we have accumulated all
-- the arguments, we can then vectorize the result of calling the user function.  The chain of
-- method calls will thus give us a value of type @(@'ArraysVec' @->@ 'ArraysVec'@)@.  This value is
-- then used __inside__ the 'kCallFunctionWithSpec' call to produce a DSL term corresponding to the
-- user's provided function.

-- | Internal machinery of 'kFunctionCall' (see source for explanations).
class KVariadicDevecAndApply (isFunCall :: FunCall) f r where
  devectorizeAndApply ::
    -- | Flag for preventing instance overlap
    Proxy isFunCall ->
    -- | Input arrays so far
    ArraysVec f ->
    -- | User function applied to all arguments so far (i.e. partially applied except in the base
    -- case)
    r ->
    -- | Arrays returned after calling the user function with all arguments
    Except VariadicFunctionCallError (ArraysVec f)

-- Base case of the recursion: we have some data arrays which we expect to be empty if everything
-- has gone well, and we have the result of the original user function, which we can return if the
-- arrays are indeed empty.
instance PolyVec f r => KVariadicDevecAndApply 'FullyApplied f r where
  devectorizeAndApply Proxy emptyInArrays vectorizedOutputs
    | leftoverLengths /= mempty = Except.throwE $ LeftoverArrayData leftoverLengths
    | otherwise =
      Except.withExceptT PVectorizeError . Except.except $ pvectorize vectorizedOutputs
    where
      leftoverLengths = arraysLength emptyInArrays

-- Recursive case: this is called with arrays of remaining input data and the original user function
-- with all preceding arguments applied.  We have to pull out the input data corresponding to the
-- present argument @a@, devectorize it and apply the function to it, then do the recursive call
-- with the remaining input data and the more-fully-applied user function.
instance
  (PolyVec f a, KVariadicDevecAndApply isFunCall f r, IsFunCall r ~ isFunCall) =>
  KVariadicDevecAndApply 'FunCall f (a -> r)
  where
  devectorizeAndApply Proxy inputArrays f =
    case pdevectorizeIncremental inputArrays of
      Left err ->
        Except.throwE $ PDevectorizeError err
      Right (nextFunctionInput :: a, remainingInputArrays) ->
        devectorizeAndApply (Proxy @isFunCall) remainingInputArrays (f nextFunctionInput)

-- This class describes variadic vectorization.  We have a 'kCallFunctionWithSpec', and we want to
-- wrap it up so that it can be called with the original type signature the user gave us.  By doing
-- recursion at the type level on the function arrow and 'IsFunCall', we can go through the user's
-- input arguments one by one and turn them into the appropriate 'Arrays' components.  These are
-- accumulated in a mutable buffer for efficiency.  (Actually, we are accumulating a function that
-- will do this when given an initial set of arrays.)  In the base case, where we have accumulated
-- the array-writing action as well as the input sizes, we call the top-level callback to do the
-- function call with what we've accumulated.  The return value of this callback gets devectorized
-- into the appropriate user return type.

-- | Internal machinery of 'kFunctionCall' (see source for explanations).
class KVariadicVectorizeFunctionInputs (isFunCall :: FunCall) f r where
  vectorizeFunctionInputs ::
    Proxy isFunCall ->
    -- | Top-level function in terms of 'Arrays' and their sizes, from whatever 'KFunCall' instance
    -- we're using.  This is only used in the base case and takes care of all non-'PolyVec'-related
    -- operations for us.
    (MArraysVecFiller f -> Arrays ArrayCount -> Arrays ArrayCount -> ArraysVec f) ->
    -- | Sizes of input arrays (accumulated from each function argument)
    Arrays ArrayCount ->
    -- | Action that, given a set of 'MArraysVec', will append the data for all the preceding
    -- arguments to the 'MArraysVec'.  This action is accumulated as we traverse the input arguments
    -- to the function, so that at the base case, it will 'Kitty.PolyVec.pvectorize' all the inputs
    -- into a single set of 'MArraysVec'.
    MArraysVecFiller f ->
    r

-- Base case of the recursion: we have an array-writing action that will vectorize all the
-- arguments, as well as size information for all the inputs.  Call the callback with what we've
-- assembled from all the function arguments; this returns the output of the function call for us to
-- devectorize into the return type @r@.
instance
  PolyVec f r =>
  KVariadicVectorizeFunctionInputs 'FullyApplied f r
  where
  vectorizeFunctionInputs ::
    Proxy 'FullyApplied ->
    (MArraysVecFiller f -> Arrays ArrayCount -> Arrays ArrayCount -> ArraysVec f) ->
    Arrays ArrayCount ->
    MArraysVecFiller f ->
    r
  vectorizeFunctionInputs Proxy arraysFunction inputCounts marraysAction
    | returnedOutputCounts /= expectedOutputCounts =
      impureThrow countMismatchError
    | otherwise =
      either impureThrow id $ pdevectorize functionOutputs
    where
      functionOutputs = arraysFunction marraysAction inputCounts expectedOutputCounts
      expectedOutputCounts = pvlengths (Proxy @f) (Proxy @r)
      returnedOutputCounts = arraysLength functionOutputs
      countMismatchError :: VariadicFunctionCallError
      countMismatchError =
        CountMismatch $
          ArrayCountMismatch expectedOutputCounts returnedOutputCounts

-- for readability.  I can't figure out how to get this to apply only to this instance of
-- 'vectorizeFunctionInputs'.
{-# ANN module "HLint: ignore Avoid lambda" #-}

-- Recursive case: form a new set of array counts and an action that will write the contents of the
-- given argument of type @a@ into the provided buffer.  Pass these to the next call.
instance
  ( PolyVec f a,
    KVariadicVectorizeFunctionInputs isFunCall f r,
    IsFunCall r ~ isFunCall
  ) =>
  KVariadicVectorizeFunctionInputs 'FunCall f (a -> r)
  where
  vectorizeFunctionInputs Proxy arraysFunction countacc marraysAction = \arg ->
    vectorizeFunctionInputs (Proxy @isFunCall) arraysFunction newCounts $
      \marrays -> marraysAction marrays *> vectorizeThis arg marrays
    where
      -- Expected array counts corresponding to this input type @a@, from its 'PolyVec' instance.
      thisCounts = pvlengths (Proxy @f) (Proxy @a)
      -- The array counts so far plus the array counts corresponding to this input type @a@.  We
      -- will pass this in the recursive call.
      newCounts = countacc <> thisCounts
      vectorizeThis :: forall s. a -> MArraysVec s f -> VectorizeT s ()
      vectorizeThis a marrays = do
        -- The array counts we actually got when we vectorized our input value of type @a@.
        counts <-
          Except.withExceptT PVectorizeError . Except.ExceptT $
            pvectorizeIncremental a marrays countacc
        unless (counts == newCounts) . Except.throwE . CountMismatch $
          ArrayCountMismatch newCounts counts
