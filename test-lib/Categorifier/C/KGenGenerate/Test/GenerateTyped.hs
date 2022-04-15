{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Expression generation for KGen in tagless-final style.
module Categorifier.C.KGenGenerate.Test.GenerateTyped
  ( genMIMOFunction,
    genCounts,
    genInputValues,
    fromCInputs,

    -- * Type constraints
    Gennable,

    -- * Array stuff
    Arrays (..),
    Compose (..),
    ArrayCount (..),
    arraysEq,
  )
where

import qualified Barbies
import qualified Categorifier.C.Barbies as Barbies
import Categorifier.C.KGen.KGen (IEEEFloatConvertible, SymVal)
import Categorifier.C.KGenGenerate.Test.Error (KSelectIndexError (..), throwKSelectIndexError)
import Categorifier.C.KGenGenerate.Test.InputReader (InputReader (..))
import Categorifier.C.KTypes.ArcTan2 (ArcTan2 (..))
import Categorifier.C.KTypes.BooleanLogic (KAnd (..))
import Categorifier.C.KTypes.C (C (..))
import Categorifier.C.KTypes.Conditional (KSelect (..), KTernary (..))
import Categorifier.C.KTypes.Equality (KEq (..))
import Categorifier.C.KTypes.FMod (FMod (..))
import Categorifier.C.KTypes.FromIntegral (KFromIntegral (..))
import Categorifier.C.KTypes.IEEE (KConvertFloat (..), KIsInfinite (..), KIsNaN (..))
import Categorifier.C.KTypes.KBits (KBits (..))
import Categorifier.C.KTypes.KDivisible (KDivisible (..))
import Categorifier.C.KTypes.KLiteral (KLiteral (..))
import Categorifier.C.KTypes.Round (KRound (..))
import Categorifier.C.KTypes.TotalOrder (KOrd (..))
import Categorifier.C.Prim
  ( ArrayCount (..),
    ArrayLens (..),
    ArrayName (..),
    Arrays (..),
    FullBlownArrayLens (..),
    HasArrays (..),
    IsPrimitive,
    arrayNames,
    fullBlownLensArray,
    lensArray,
  )
import Categorifier.C.Prim.Hedgehog (genBool, genFloating, genIntegral)
import Control.Lens (imap, view)
import Control.Monad (replicateM, when)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Cont as Cont
import Control.Monad.Trans.Reader (Reader)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Control.Monad.Trans.Writer.CPS as Writer
import Data.Bits (FiniteBits (finiteBitSize))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Product (Product (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Monoid (All (..), Sum (..))
import Data.Proxy (Proxy (..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Stack (callStack)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as H
import qualified Hedgehog.Range as H

{- Operation generators are all fully polymorphic in type classes. -}

genNumUnOp :: (Num a, H.MonadGen m) => m (a -> a)
genNumUnOp =
  H.element
    [ negate,
      signum,
      abs
    ]

genFloatingUnOp :: (Floating a, H.MonadGen m) => m (a -> a)
genFloatingUnOp =
  H.element
    [ exp,
      log,
      sqrt,
      sin,
      cos,
      tan,
      asin,
      acos,
      atan,
      sinh,
      cosh,
      tanh
      -- -- no, dawg
      -- asinh,
      -- acosh,
      -- atanh
    ]

genBoolUnOp :: (KAnd f, H.MonadGen m) => m (f Bool -> f Bool)
genBoolUnOp =
  H.element
    [ kNot
    ]

genNumBinOp ::
  (KOrd f (f a), Num (f a), H.MonadGen m) => m (f a -> f a -> f a)
genNumBinOp =
  H.element
    [ (*),
      (+),
      (-),
      kMax,
      kMin
    ]

-- | Helper for generating a valid index
genBitIndexFor :: forall a m. (FiniteBits a, Integral a, H.MonadGen m) => Proxy a -> m Int
genBitIndexFor Proxy = H.integral range
  where
    maxIndex = finiteBitSize (0 :: a) - 1
    range = H.linear 0 $ fromIntegral maxIndex :: H.Range Int

genTestBit :: forall f a m. (FiniteBits a, Integral a, KBits f a, H.MonadGen m) => m (f a -> f Bool)
genTestBit = do
  bitIndex <- genBitIndexFor (Proxy @a)
  pure (`testBit` bitIndex)

genSetBit ::
  forall f a m. (FiniteBits a, Integral a, KBits f a, H.MonadGen m) => m (f a -> f Bool -> f a)
genSetBit = do
  bitIndex <- genBitIndexFor (Proxy @a)
  pure (`setBitTo` bitIndex)

genIntBinOp :: (KDivisible (f a), H.MonadGen m) => m (f a -> f a -> f a)
genIntBinOp =
  H.element
    [ kDiv,
      kMod
    ]

genFloatingBinOp :: (FMod a, ArcTan2 a, H.MonadGen m) => m (a -> a -> a)
genFloatingBinOp =
  H.element
    [ (/),
      logBase,
      (**),
      arctan2,
      fmod
    ]

genBoolBinOp :: (KEq f (f Bool), H.MonadGen m) => m (f Bool -> f Bool -> f Bool)
genBoolBinOp =
  H.element
    [ (.&&),
      (.||),
      (.==),
      (./=)
    ]

genNumCmpOp :: (KOrd f (f a), H.MonadGen m) => m (f a -> f a -> f Bool)
genNumCmpOp =
  H.element
    [ (.==),
      (./=),
      (.<),
      (.>),
      (.<=),
      (.>=)
    ]

{- Assorted useful constraint synonyms -}

type GenFloatingConstraint f a =
  ( Floating (f a),
    ArcTan2 (f a),
    FMod (f a),
    KEq f (f a),
    KOrd f (f a),
    KLiteral f a,
    IsPrimitive a,
    KTernary f (f a),
    KFromIntegral f a,
    KConvertFloat f,
    KIsInfinite f a,
    KIsNaN f a,
    SymVal a
  )

type GenNumConstraint f a =
  ( Num (f a),
    IsPrimitive a,
    KLiteral f a,
    KEq f (f a),
    KOrd f (f a),
    KDivisible (f a),
    KTernary f (f a),
    KFromIntegral f a,
    KConvertFloat f,
    KRound f a,
    SymVal a,
    IEEEFloatConvertible a
  )

-- | Is this thing an integer and a machine word?
type GenWordConstraint f a = (GenNumConstraint f a, FiniteBits a, KBits f a)

type GenBoolConstraint f = (KAnd f, KLiteral f Bool, KEq f (f Bool), KTernary f (f Bool))

type Gennable f =
  ( GenFloatingConstraint f Double,
    GenFloatingConstraint f Float,
    GenNumConstraint f Int8,
    GenNumConstraint f Int16,
    GenNumConstraint f Int32,
    GenNumConstraint f Int64,
    GenWordConstraint f Word8,
    GenWordConstraint f Word16,
    GenWordConstraint f Word32,
    GenWordConstraint f Word64,
    GenBoolConstraint f,
    KSelect f
  )

{- Hedgehog helpers -}

-- This is like Hedgehog.Gen.recursive, except there's no requirement that everything be `a`, so we
-- can sandwich some conversions in.
polyRecursive ::
  (Functor f, Semigroup (f (m a)), H.MonadGen m) => (f (m a) -> m b) -> f (m a) -> f (m a) -> m b
polyRecursive f leaves branches = H.sized constrainSize
  where
    constrainSize n =
      if n <= 1
        then f leaves
        else f $ leaves <> fmap H.small branches

{- Primitive generators -}

{- Expression generators -}

genNumExprOptions ::
  (H.MonadGen m, Gennable f, GenNumConstraint f a) =>
  m (f a) ->
  [m (f a)]
genNumExprOptions self =
  [ genNumUnOp >>= H.subterm self,
    genNumBinOp >>= H.subterm2 self self,
    genIntBinOp >>= H.subterm2 self self,
    genKTernary genBoolExpr self self,
    kFromIntegral <$> genInt8Expr,
    kFromIntegral <$> genInt16Expr,
    kFromIntegral <$> genInt32Expr,
    kFromIntegral <$> genInt64Expr,
    kFromIntegral <$> genWord8Expr,
    kFromIntegral <$> genWord16Expr,
    kFromIntegral <$> genWord32Expr,
    kFromIntegral <$> genWord64Expr,
    kRoundDouble <$> genFloatingExpr,
    kRoundFloat <$> genFloatingExpr,
    genSelectList genWord8Expr self
  ]

genNumExpr :: forall m f a. (GenNumConstraint f a, Gennable f, H.MonadGen m) => m (f a)
genNumExpr =
  polyRecursive
    H.choice
    [kliteral <$> genIntegral]
    $ genNumExprOptions genNumExpr

genWordExpr :: forall m f a. (GenWordConstraint f a, Gennable f, H.MonadGen m) => m (f a)
genWordExpr =
  polyRecursive
    H.choice
    [ kliteral <$> genIntegral,
      pure zeroBits
    ]
    $ genNumExprOptions genWordExpr
      <> [flip <$> genSetBit <*> genBoolExpr >>= H.subterm genWordExpr]

genInt8Expr :: (Gennable f, H.MonadGen m) => m (f Int8)
genInt8Expr = genNumExpr

genInt16Expr :: (Gennable f, H.MonadGen m) => m (f Int16)
genInt16Expr = genNumExpr

genInt32Expr :: (Gennable f, H.MonadGen m) => m (f Int32)
genInt32Expr = genNumExpr

genInt64Expr :: (Gennable f, H.MonadGen m) => m (f Int64)
genInt64Expr = genNumExpr

genWord8Expr :: (Gennable f, H.MonadGen m) => m (f Word8)
genWord8Expr = genWordExpr

genWord16Expr :: (Gennable f, H.MonadGen m) => m (f Word16)
genWord16Expr = genWordExpr

genWord32Expr :: (Gennable f, H.MonadGen m) => m (f Word32)
genWord32Expr = genWordExpr

genWord64Expr :: (Gennable f, H.MonadGen m) => m (f Word64)
genWord64Expr = genWordExpr

genFloatingExpr ::
  forall m f a. (Gennable f, RealFloat a, GenFloatingConstraint f a, H.MonadGen m) => m (f a)
genFloatingExpr =
  polyRecursive
    H.choice
    [kliteral <$> genFloating]
    [ genNumUnOp >>= H.subterm genFloatingExpr,
      genFloatingUnOp >>= H.subterm genFloatingExpr,
      genNumBinOp >>= H.subterm2 genFloatingExpr genFloatingExpr,
      genFloatingBinOp >>= H.subterm2 genFloatingExpr genFloatingExpr,
      genKTernary genBoolExpr genFloatingExpr genFloatingExpr,
      kFromIntegral <$> genInt8Expr,
      kFromIntegral <$> genInt16Expr,
      kFromIntegral <$> genInt32Expr,
      kFromIntegral <$> genInt64Expr,
      kFromIntegral <$> genWord8Expr,
      kFromIntegral <$> genWord16Expr,
      kFromIntegral <$> genWord32Expr,
      kFromIntegral <$> genWord64Expr,
      genSelectList genWord8Expr genFloatingExpr
    ]

genDoubleExpr :: (Gennable f, H.MonadGen m) => m (f Double)
genDoubleExpr =
  H.choice
    [ genFloatingExpr,
      kFloatToDouble <$> genFloatingExpr
    ]

genFloatExpr :: (Gennable f, H.MonadGen m) => m (f Float)
genFloatExpr =
  H.choice
    [ genFloatingExpr,
      kDoubleToFloat <$> genFloatingExpr
    ]

genBoolExpr :: (Gennable f, H.MonadGen m) => m (f Bool)
genBoolExpr =
  polyRecursive
    H.choice
    [kliteral <$> genBool]
    [ genBoolUnOp >>= H.subterm genBoolExpr,
      genBoolBinOp >>= H.subterm2 genBoolExpr genBoolExpr,
      genNumCmpOp <*> genInt8Expr <*> genNumExpr,
      genNumCmpOp <*> genInt16Expr <*> genNumExpr,
      genNumCmpOp <*> genInt32Expr <*> genNumExpr,
      genNumCmpOp <*> genInt64Expr <*> genNumExpr,
      genNumCmpOp <*> genWord8Expr <*> genNumExpr,
      genNumCmpOp <*> genWord16Expr <*> genNumExpr,
      genNumCmpOp <*> genWord32Expr <*> genNumExpr,
      genNumCmpOp <*> genWord64Expr <*> genNumExpr,
      genNumCmpOp <*> genFloatExpr <*> genFloatingExpr,
      genNumCmpOp <*> genDoubleExpr <*> genFloatingExpr,
      H.subterm3 genBoolExpr genBoolExpr genBoolExpr kTernary,
      genTestBit <*> genWord8Expr,
      genTestBit <*> genWord16Expr,
      genTestBit <*> genWord32Expr,
      genTestBit <*> genWord64Expr,
      kIsInfinite <$> genFloatExpr,
      kIsInfinite <$> genDoubleExpr,
      kIsNaN <$> genFloatExpr,
      kIsNaN <$> genDoubleExpr,
      genSelectList genWord8Expr genBoolExpr
    ]

{- If-then-else -}

genKTernary :: (H.MonadGen m, KTernary f (f a)) => m (f Bool) -> m (f a) -> m (f a) -> m (f a)
genKTernary bg lg rg = H.subterm2 lg rg . kTernary =<< bg

{- selectList -}

-- This stuff all uses lists because `selectList` itself does.

subtermListM :: H.MonadGen m => m a -> Int -> ([a] -> m a) -> m a
subtermListM gen count = Cont.runContT . replicateM count . Cont.ContT $ H.subtermM gen

subtermList :: H.MonadGen m => m a -> Int -> ([a] -> a) -> m a
subtermList gen count f = subtermListM gen count $ pure . f

-- | The strategy here is: rather than try to tackle arbitrary types in each case, let's at least
-- test the basic logic by using a scalar value in each branch, which is easy and not much different
-- to the way the rest of the expression generation works already.
genSelectList ::
  forall m f a.
  (H.MonadGen m, KSelect f, KLiteral f Word8, KDivisible (f Word8), IsPrimitive a) =>
  m (f Word8) ->
  m (f a) ->
  m (f a)
genSelectList idx elemGen = do
  count <- H.integral $ H.exponential 1 256 :: m Int
  -- If we mod by the count here, we will get a value in [0, count).
  idxmod <- (`kMod` (kliteral $ fromIntegral count)) <$> idx
  subtermList elemGen count (\elems -> selectHead $ selectList (fmap pure elems) idxmod)
  where
    selectHead xs =
      let len = length xs
       in if len == 1
            then Vector.head xs
            else throwKSelectIndexError callStack $ KSelectInvalidReturn len

{- Input generation -}

newtype SafelyGenerate m f a = SafelyGenerate
  { getSafelyGenerate :: ArrayCount a -> m (Compose Vector f a)
  }

safeGenerators :: forall m f. (H.MonadGen m, Gennable f) => Arrays (SafelyGenerate m f)
safeGenerators =
  Arrays
    { arrayBool = mkGen genBool,
      arrayInt8 = mkGen genIntegral,
      arrayInt16 = mkGen genIntegral,
      arrayInt32 = mkGen genIntegral,
      arrayInt64 = mkGen genIntegral,
      arrayWord8 = mkGen genIntegral,
      arrayWord16 = mkGen genIntegral,
      arrayWord32 = mkGen genIntegral,
      arrayWord64 = mkGen genIntegral,
      arrayFloat = mkGen genFloating,
      arrayDouble = mkGen genFloating
    }
  where
    mkGen :: forall a. KLiteral f a => m a -> SafelyGenerate m f a
    mkGen gen = SafelyGenerate $
      \(ArrayCount n) ->
        Compose . Vector.fromList
          <$> replicateM n (kliteral <$> gen)

genInputValues ::
  forall m f. (Gennable f, H.MonadGen m) => Arrays ArrayCount -> m (Arrays (Compose Vector f))
genInputValues = Barbies.bzipWithM getSafelyGenerate safeGenerators

fromCInputs :: Arrays (Compose Vector C) -> Arrays Vector
fromCInputs = Barbies.bmap (fmap unsafeC . getCompose)
{-# INLINE fromCInputs #-}

{- Open terms -}

-- TODO(MP): Get clever about using 'H.shrink' in some of these situations. Hedgehog is often
-- limited in number of shrinking steps due to memory usage, but it also doesn't seem to do a good
-- job shrinking complex terms, in spite of conscientious use of 'subterm*'

newtype FreeTerms a = FreeTerms {_getFreeTerms :: Word}
  deriving newtype (Show, Read, Eq, Ord, Num)

newtype Budget = Budget {_getBudget :: Word}
  deriving newtype (Show, Read, Eq, Ord, Num)

newtype Generate m f a = Generate {getGenerate :: m (f a)}

newtype OpenGenerate m f a = OpenGenerate {getOpenGenerate :: OpenGenState f -> m (f a)}

newtype KVec f a = KVec {getKVec :: [f a]}
  deriving newtype (Semigroup, Monoid)

data OpenGenTypeState f a = OpenGenTypeState
  { stillFree :: !(KVec f a),
    beenBound :: !(KVec f a)
  }

data OpenGenState f = OpenGenState
  { _budget :: !Budget,
    stateArrays :: !(Arrays (OpenGenTypeState f))
  }

generatorArray :: (H.MonadGen m, Gennable f) => Arrays (Generate m f)
generatorArray =
  Arrays
    { arrayBool = Generate genBoolExpr,
      arrayInt8 = Generate genNumExpr,
      arrayInt16 = Generate genNumExpr,
      arrayInt32 = Generate genNumExpr,
      arrayInt64 = Generate genNumExpr,
      arrayWord8 = Generate genNumExpr,
      arrayWord16 = Generate genNumExpr,
      arrayWord32 = Generate genNumExpr,
      arrayWord64 = Generate genNumExpr,
      arrayFloat = Generate genFloatingExpr,
      arrayDouble = Generate genFloatingExpr
    }

openGeneratorArray :: (H.MonadGen m, Gennable f) => Arrays (OpenGenerate m f)
openGeneratorArray =
  Arrays
    { arrayBool = OpenGenerate $ generateOpen arrayBool_,
      arrayInt8 = OpenGenerate $ generateOpen arrayInt8_,
      arrayInt16 = OpenGenerate $ generateOpen arrayInt16_,
      arrayInt32 = OpenGenerate $ generateOpen arrayInt32_,
      arrayInt64 = OpenGenerate $ generateOpen arrayInt64_,
      arrayWord8 = OpenGenerate $ generateOpen arrayWord8_,
      arrayWord16 = OpenGenerate $ generateOpen arrayWord16_,
      arrayWord32 = OpenGenerate $ generateOpen arrayWord32_,
      arrayWord64 = OpenGenerate $ generateOpen arrayWord64_,
      arrayFloat = OpenGenerate $ generateOpen arrayFloat_,
      arrayDouble = OpenGenerate $ generateOpen arrayDouble_
    }

newtype UnOpGen m f a = UnOpGen {getUnOpGen :: f a -> m (f a)}

unOpGenArray :: (Gennable f, H.MonadGen m) => Arrays (UnOpGen m f)
unOpGenArray =
  Arrays
    { arrayBool = mkUnOpGen genBoolUnOp,
      arrayInt8 = mkUnOpGen genNumUnOp,
      arrayInt16 = mkUnOpGen genNumUnOp,
      arrayInt32 = mkUnOpGen genNumUnOp,
      arrayInt64 = mkUnOpGen genNumUnOp,
      arrayWord8 = mkUnOpGen genNumUnOp,
      arrayWord16 = mkUnOpGen genNumUnOp,
      arrayWord32 = mkUnOpGen genNumUnOp,
      arrayWord64 = mkUnOpGen genNumUnOp,
      arrayFloat = mkUnOpGen $ H.choice [genFloatingUnOp, genNumUnOp],
      arrayDouble = mkUnOpGen $ H.choice [genFloatingUnOp, genNumUnOp]
    }
  where
    mkUnOpGen g0 = UnOpGen $ \x -> g0 >>= H.subterm (pure x)

newtype BinOpGen m f a = BinOpGen {getBinOpGen :: f a -> f a -> m (f a)}

binOpGenArray :: (Gennable f, H.MonadGen m) => Arrays (BinOpGen m f)
binOpGenArray =
  Arrays
    { arrayBool = mkBinOpGen genBoolBinOp,
      arrayInt8 = mkBinOpGen genNumBinOp,
      arrayInt16 = mkBinOpGen genNumBinOp,
      arrayInt32 = mkBinOpGen genNumBinOp,
      arrayInt64 = mkBinOpGen genNumBinOp,
      arrayWord8 = mkBinOpGen genNumBinOp,
      arrayWord16 = mkBinOpGen genNumBinOp,
      arrayWord32 = mkBinOpGen genNumBinOp,
      arrayWord64 = mkBinOpGen genNumBinOp,
      arrayFloat = mkBinOpGen $ H.choice [genFloatingBinOp, genNumBinOp],
      arrayDouble = mkBinOpGen $ H.choice [genFloatingBinOp, genNumBinOp]
    }
  where
    mkBinOpGen g0 = BinOpGen $ \x y -> g0 >>= H.subterm2 (pure x) (pure y)

newtype CmpGen m f a = CmpGen {_getCmpGen :: f a -> f a -> m (f Bool)}

cmpGenArray :: (Gennable f, H.MonadGen m) => Arrays (CmpGen m f)
cmpGenArray =
  Arrays
    { arrayBool = CmpGen $ \x y -> genBoolBinOp >>= H.subterm2 (pure x) (pure y),
      arrayInt8 = mkCmpGen,
      arrayInt16 = mkCmpGen,
      arrayInt32 = mkCmpGen,
      arrayInt64 = mkCmpGen,
      arrayWord8 = mkCmpGen,
      arrayWord16 = mkCmpGen,
      arrayWord32 = mkCmpGen,
      arrayWord64 = mkCmpGen,
      arrayFloat = mkCmpGen,
      arrayDouble = mkCmpGen
    }
  where
    mkCmpGen = CmpGen $ \x y -> genNumCmpOp <*> pure x <*> pure y

-- | Given some state and an 'Arrays' of records you want to apply a function to and choose from and
-- a selection lens that picks out an element to exclude, this function will apply the function to
-- the appropriate array record and return the result. Why does it exist?  Try selecting a random
-- sub-term fully polymorphically without a function like this.
chooseRecordWithFreeVars ::
  forall m f t b d.
  H.MonadGen m =>
  OpenGenState f ->
  Arrays t ->
  (forall g h. Functor g => (h d -> g (h d)) -> Arrays h -> g (Arrays h)) ->
  (forall a. IsPrimitive a => t a -> m b) ->
  m b
chooseRecordWithFreeVars ogState arrays selector f = do
  run <-
    flip State.execStateT Nothing $
      Barbies.bzipWith3MC_ @IsPrimitive go (stateArrays ogState) arrays arrayNames
  case run of
    Nothing -> error "No free variables in expression -- this should be impossible!"
    Just x -> x
  where
    ArrayName exclude = view selector arrayNames
    go ::
      IsPrimitive a =>
      OpenGenTypeState f a ->
      t a ->
      ArrayName a ->
      State.StateT (Maybe (m b)) m ()
    go thisState val (ArrayName name) = do
      let freeCount = length . getKVec $ stillFree thisState
          useThis = exclude /= name
      got <- State.get
      when (freeCount > 0 && useThis) $
        case got of
          Nothing -> State.put . Just $ f val
          Just _ -> lift H.bool >>= \b -> when b $ State.put (Just $ f val)

-- | This function has the following useful properties for budgets:
--
--  * The budget is never larger than the old budget.
--  * If old budget is greater than the number of free variables, the new budget is smaller than the
--    old budget.
--  * The budget is never smaller than the number of free variables.
--  * When applied repeatedly, the budget will converge exponentially to the number of free
--    variables. This counteracts the potential exponential explosion in tree size.
--
-- This means it can be applied in all situations and will do the right thing, whether or not we are
-- generating minimal or non-minimal expressions.
calcNewBudget :: Integral a => Budget -> a -> Budget
calcNewBudget (Budget oldBudget) numFreeVars' =
  Budget $ numFreeVars + (oldBudget - numFreeVars) `div` 2
  where
    numFreeVars = fromIntegral numFreeVars'

partitionFreeVarsByType ::
  forall f d.
  OpenGenState f ->
  (forall g h. Functor g => (h d -> g (h d)) -> Arrays h -> g (Arrays h)) ->
  (OpenGenState f, OpenGenState f)
partitionFreeVarsByType (OpenGenState oldBudget arrays) selector =
  ( OpenGenState (mkBudget thisState) thisState,
    OpenGenState (mkBudget otherState) otherState
  )
  where
    prs = Barbies.bzipWith go arrays arrayNames
    mkBudget = calcNewBudget oldBudget . countFreeVars
    chooseThis (Pair this _) = this
    chooseOther (Pair _ other) = other
    thisState = Barbies.bmap chooseThis prs
    otherState = Barbies.bmap chooseOther prs
    ArrayName exclude = view selector arrayNames
    go :: OpenGenTypeState f a -> ArrayName a -> Product (OpenGenTypeState f) (OpenGenTypeState f) a
    go ogs@(OpenGenTypeState fv bv) (ArrayName name) =
      let allBound = OpenGenTypeState mempty (fv <> bv)
       in if exclude == name
            then -- This is the selected "this" type, so bind all the free variables for non-"this"
            --      expressions and keep them for "this" type
              Pair ogs allBound
            else -- This is not the "this" type, so bind all the free variables for "this"-type
            --      expressions and keep them as-is for the rest.
              Pair allBound ogs

{-

This machinery is so we can do traversals and partition free variables randomly but with the
guarantee that we always do a meaningful split (one variable per side if there are at least two
variables).

-}

data PartitionRecord
  = Unpartitioned
  | DidL
  | DidR
  | Partitioned
  deriving (Show, Eq, Ord)

markL :: Monad m => StateT PartitionRecord m ()
markL = State.modify $ \case
  Unpartitioned -> DidL
  DidL -> DidL
  _ -> Partitioned

markR :: Monad m => StateT PartitionRecord m ()
markR = State.modify $ \case
  Unpartitioned -> DidR
  DidR -> DidR
  _ -> Partitioned

chooseLR :: H.MonadGen m => a -> a -> StateT PartitionRecord m a
chooseLR a b = do
  record <- State.get
  case record of
    DidR -> a <$ markL
    DidL -> b <$ markR
    _ -> do
      ch <- lift H.bool
      if ch
        then a <$ markL
        else b <$ markR

-- | This bad boy partitions a state into two new states, each of which has a subset of the free
-- variables of the original state.
--
-- The budget is guaranteed to decrease by one each time.
--
-- If there was more than one free variable in the original state, neither new state will be empty
-- of free variables.
partitionFreeVars ::
  forall m f. H.MonadGen m => OpenGenState f -> m (Product OpenGenState OpenGenState f)
partitionFreeVars inputState = do
  pairray <- State.evalStateT pairs Unpartitioned
  let l = subPartition leftPartition inputState pairray
      r = subPartition rightPartition inputState pairray
  pure $ Pair l r
  where
    -- We do a stateful traversal with a 'PartitionRecord' that lets us partition deterministically
    -- until we've put at least one free variable on each side, then use the Hedgehog generator to
    -- partition at random. This ensures that our budget can always decrease.
    pairs :: StateT PartitionRecord m (Arrays (Product (KVec f) (KVec f)))
    pairs = Barbies.btraverse part (stateArrays inputState)
    leftPartition :: Product (KVec f) (KVec f) a -> OpenGenTypeState f a -> OpenGenTypeState f a
    leftPartition (Pair l r) (OpenGenTypeState _ bound) =
      OpenGenTypeState l (r <> bound)
    rightPartition :: Product (KVec f) (KVec f) a -> OpenGenTypeState f a -> OpenGenTypeState f a
    rightPartition (Pair l r) (OpenGenTypeState _ bound) =
      OpenGenTypeState r (l <> bound)
    subPartition ::
      ( forall a.
        IsPrimitive a =>
        Product (KVec f) (KVec f) a ->
        OpenGenTypeState f a ->
        OpenGenTypeState f a
      ) ->
      OpenGenState f ->
      Arrays (Product (KVec f) (KVec f)) ->
      OpenGenState f
    subPartition lrf (OpenGenState budg arrays) prs =
      let result = Barbies.bzipWithC @IsPrimitive lrf prs arrays
       in OpenGenState (calcNewBudget budg $ countFreeVars result) result
    part :: OpenGenTypeState f a -> StateT PartitionRecord m (Product (KVec f) (KVec f) a)
    part ogs =
      fmap (\(Pair l r) -> Pair (KVec l) (KVec r))
        . chooseVars (getKVec $ stillFree ogs)
        $ Pair mempty mempty
    -- This function randomly partitions the list using a Hedgehog generator
    chooseVars [] acc = pure acc
    chooseVars (x : xs) (Pair l r) = do
      nextAcc <- chooseLR (Pair (x : l) r) (Pair l (x : r))
      chooseVars xs nextAcc

ogCountFreeVars :: OpenGenState f -> Int
ogCountFreeVars = countFreeVars . stateArrays

countFreeVars :: Arrays (OpenGenTypeState f) -> Int
countFreeVars = getSum . Writer.execWriter . Barbies.btraverse_ go
  where
    go = Writer.tell . Sum . length . getKVec . stillFree

ogCountFreeTypes :: OpenGenState f -> Int
ogCountFreeTypes = countFreeTypes . stateArrays

countFreeTypes :: Arrays (OpenGenTypeState f) -> Int
countFreeTypes = length . filter (> 0) . Writer.execWriter . Barbies.btraverse_ go
  where
    go = Writer.tell . pure . length . getKVec . stillFree

-- The generation algorithm for open terms is supposed to
--
--  * always terminate
--  * include every free variable given as an input in the expression
--  * try to respect 'size' to the extent possible
--  * typecheck at every stage, obviously, in spite of/due to all expressions being finally-encoded
--
-- We maintain a "budget", essentially our own way of tracking the size of term we want, along with
-- sets of free variables and already-bound input variables. The invariant we maintain is that the
-- budget is never smaller than the number of free variables. At every recursive step, we must
-- decrease the budget and the number of free variables. The budget affects the probability of using
-- a free variable; as the budget gets lower, we're more likely to use free variables when forming
-- expressions. The base case for these low-budget situations is the pre-determined set of minimal
-- expressions for each; if the budget is not minimal, we simply choose probabilistically between
-- using free variables and not, but we continue to decrease the budget.
--
-- In the case where we are asked for a term of a type with no remaining free variables and our
-- budget has just become minimal, we may not be able to decrease it in this step. The only
-- exception, therefore, to the convergence criterion above, is that we may leave the budget and
-- free variable sets untouched if we request a term of a type with free variables remaining.
--
-- Any time we generate multiple subterms, we partition the available free variables between the
-- subterms. Those left free in one subterm are added to the set of variables already bound in the
-- other subterm and vice versa, so that we guarantee both to always use every free variable we're
-- given and that the entirety of the expression tree we generate has access to the full set of
-- input values.
--
-- We still rely on Hedgehog to generate the terms we want by overriding its 'size' parameter.
generateOpen ::
  forall m f a.
  (H.MonadGen m, Gennable f, KTernary f (f a)) =>
  -- | the lens
  (forall g h. Functor g => (h a -> g (h a)) -> Arrays h -> g (Arrays h)) ->
  -- | the generator
  OpenGenState f ->
  m (f a)
generateOpen selector state@(OpenGenState oldBudget@(Budget budget) stateArr) =
  if remainingBudget > 0
    then genNonMinimal
    else generateMinimal
  where
    thisGenerator = getOpenGenerate (view selector openGeneratorArray)
    thisClosed = getGenerate (view selector generatorArray)
    getThisFreeVars :: OpenGenState f -> KVec f a
    getThisFreeVars = stillFree . view selector . stateArrays
    countThisFreeVars = length . getKVec . getThisFreeVars
    numFreeVars = ogCountFreeVars state
    numFreeTypes = ogCountFreeTypes state
    remainingBudget :: Word
    remainingBudget = budget - fromIntegral numFreeVars
    generateSame :: m (f a)
    generateSame =
      generateOpen selector $
        OpenGenState (calcNewBudget oldBudget numFreeVars) stateArr
    genNonMinimal :: m (f a)
    genNonMinimal =
      H.choice
        [ generateMinimal,
          genNonMinimalUnOp,
          genNonMinimalBinOp
        ]
    genNonMinimalUnOp = generateSame >>= getUnOpGen (view selector unOpGenArray)
    genNonMinimalBinOp = do
      let boundVars = getKVec . beenBound $ view selector stateArr
      -- This ensures we always generate one open term (which receives all the free-variable state)
      -- and pair it with one of our bound variables.
      aterm <- generateSame
      bterm <- case boundVars of
        -- No bound variables, just reduce the budget and move on
        [] -> generateSame
        -- Chose a bound variable for one branch
        xs -> H.choice $ fmap pure xs
      outterms <- H.element [(aterm, bterm), (bterm, aterm)]
      uncurry (getBinOpGen $ view selector binOpGenArray) outterms
    genMinimalBinOp :: m (f a)
    genMinimalBinOp = do
      Pair l r <- partitionFreeVars state
      lterm <- generateOpen selector l
      rterm <- generateOpen selector r
      getBinOpGen (view selector binOpGenArray) lterm rterm
    generateMinimal :: m (f a)
    generateMinimal
      | numFreeVars == 0 =
          -- If there are no free variables, we can choose uniformly between recursively generating
          -- this type (from the array) and pulling in a free variable that's already been bound
          -- elsewhere in the expression.
          H.choice $
            thisClosed :
            fmap pure (getKVec . beenBound $ view selector stateArr)
      | numFreeVars > 0
          && numFreeTypes == 1 =
          -- We only have free variables of one type we must still assign. This is where the budget
          -- comes in -- if our budget is getting short, we may _have_ to shoot for a minimally sized
          -- tree. Otherwise we have a choice.
          if countThisFreeVars state > 0
            then genMinimalThisOnly state
            else genMinimalOtherOnly state
      | otherwise =
          -- We have free variables of multiple types. If the budget is tight, we had better start
          -- branching in types.
          if countThisFreeVars state > 0
            then genMinimalThisAndOthers state
            else genMinimalOthers state
    -- If our budget is low, we have to select from the minimally-deep options we have
    -- available. These sets depend on whether we still need to use free variables of the type of
    -- _this_ term.

    -- Minimal-depth expression for remaining free variables only of this type
    --
    -- Strategy -- generate a direct op if possible, otherwise partition and use a binop on two
    -- subexpressions of this type.
    genMinimalThisOnly :: OpenGenState f -> m (f a)
    genMinimalThisOnly ogState = case getKVec $ getThisFreeVars ogState of
      -- Use the remaining free variable directly.
      [x] -> pure x
      -- Use the remaining free variables directly.
      [x, y] -> getBinOpGen (view selector binOpGenArray) x y
      _ -> genMinimalBinOp
    -- Minimal-depth expression for remaining free variables of mixed types, including this one.
    --
    -- Strategy -- generate an expression unifying this type and the other type.
    genMinimalThisAndOthers :: OpenGenState f -> m (f a)
    genMinimalThisAndOthers ogState = do
      let genAndCmp :: Arrays (Product (OpenGenerate m f) (CmpGen m f))
          genAndCmp = Barbies.bzip openGeneratorArray cmpGenArray
          (thisState, otherState) = partitionFreeVarsByType ogState selector
      chooseRecordWithFreeVars
        otherState
        genAndCmp
        selector
        ( \(Pair (OpenGenerate tg) (CmpGen cg)) -> do
            Pair lstate rstate <- partitionFreeVars otherState
            l <- tg lstate
            r <- tg rstate
            Pair tlstate trstate <- partitionFreeVars thisState
            genKTernary (cg l r) (thisGenerator tlstate) (thisGenerator trstate)
        )
    -- Minimal-depth expression for remaining free variables of only one other type.
    --
    -- Strategy -- go as directly as possible to generation of two subexpressions of that type
    genMinimalOtherOnly = genMinimalThisAndOthers
    -- Minimal-depth expression for remaining free variables of mixed types, not including this one.
    --
    -- Strategy -- we don't have a way to get these two different types directly, so instead go
    -- directly to a subexpression of one of those types (at which stage we will be in the previous
    -- case).
    genMinimalOthers = genMinimalThisAndOthers

-- | This takes an input spec and gives us an equivalent 'InputReader'. This can be used to generate
-- a random expression _without_ fixing the inputs. The only partiality comes from the need to index
-- into the real input vectors, whenever they arrive, and this is the same as making sure the input
-- matches the original input spec.
specToReaderArray ::
  forall f. Arrays ArrayCount -> Arrays (OpenGenTypeState (InputReader f))
specToReaderArray counts = Barbies.bzipWith go counts lensArray
  where
    go ::
      ArrayCount a -> ArrayLens (Compose Vector f) a -> OpenGenTypeState (InputReader f) a
    go (ArrayCount n) lns =
      OpenGenTypeState (KVec . fmap InputReader . ir lns $ replicate n ()) mempty
    ir :: ArrayLens (Compose Vector f) a -> [b] -> [Reader (Arrays (Compose Vector f)) (f a)]
    ir (ArrayLens lns) =
      fmap (\i -> Reader.reader $ (Vector.! i) . getCompose . view lns)
        . imap const

specToReaderState :: Arrays ArrayCount -> OpenGenState (InputReader f)
specToReaderState counts =
  OpenGenState (Budget . fromIntegral $ countFreeVars readerArr) readerArr
  where
    readerArr = specToReaderArray counts

genOutputs ::
  (H.MonadGen m, Gennable f, KTernary (InputReader f) (InputReader f a)) =>
  OpenGenState (InputReader f) ->
  FullBlownArrayLens a ->
  ArrayCount a ->
  m (KVec (InputReader f) a)
genOutputs readerState (FullBlownArrayLens thisLens) (ArrayCount thisCount) =
  KVec <$> replicateM thisCount (generateOpen thisLens readerState)

-- We need this layer of indirection so that we fix the 'a' to each of the primitive types
-- separately before we try to apply 'generateOpen'. This allows GHC to see that the necessary
-- instances are present; if we don't use it, it tries to find them for all 'a' and fails.
newtype MimoApply m f a = MimoApply
  { getMimoApply ::
      FullBlownArrayLens a ->
      ArrayCount a ->
      m (KVec (InputReader f) a)
  }

mimoApply ::
  forall m f.
  (Gennable f, H.MonadGen m) =>
  OpenGenState (InputReader f) ->
  Arrays (MimoApply m f)
mimoApply readerState =
  Arrays
    (MimoApply $ genOutputs readerState)
    (MimoApply $ genOutputs readerState)
    (MimoApply $ genOutputs readerState)
    (MimoApply $ genOutputs readerState)
    (MimoApply $ genOutputs readerState)
    (MimoApply $ genOutputs readerState)
    (MimoApply $ genOutputs readerState)
    (MimoApply $ genOutputs readerState)
    (MimoApply $ genOutputs readerState)
    (MimoApply $ genOutputs readerState)
    (MimoApply $ genOutputs readerState)

-- | Here it is!
genMIMOFunction ::
  forall m f.
  (H.MonadGen m, Gennable f) =>
  Arrays ArrayCount ->
  Arrays ArrayCount ->
  m (Arrays (Compose Vector f) -> Arrays (Compose Vector f))
genMIMOFunction inputCounts outputCounts =
  Reader.runReader . Barbies.btraverse goInput
    <$> Barbies.bzipWith3M getMimoApply (mimoApply readerState) fullBlownLensArray outputCounts
  where
    readerState :: OpenGenState (InputReader f)
    readerState = specToReaderState inputCounts
    goInput :: KVec (InputReader f) a -> Reader (Arrays (Compose Vector f)) (Compose Vector f a)
    goInput kv = fmap (Compose . Vector.fromList) . traverse getInputReader $ getKVec kv

genCounts :: H.MonadGen m => Int -> m (Arrays ArrayCount)
genCounts n =
  Barbies.btraverse (go n) (mempty :: Arrays ArrayCount)
  where
    go count _ = fmap ArrayCount . H.integral $ H.exponential 0 count

floatingResultEq :: RealFloat a => a -> a -> Bool
floatingResultEq a b
  | isNaN a && isNaN b = True
  | otherwise = a == b

intResultEq :: Eq a => a -> a -> Bool
intResultEq = (==)

newtype EqFunction a = EqFunction {_getEqFunction :: a -> a -> Bool}

eqArray :: Arrays EqFunction
eqArray =
  Arrays
    { arrayBool = EqFunction intResultEq,
      arrayInt8 = EqFunction intResultEq,
      arrayInt16 = EqFunction intResultEq,
      arrayInt32 = EqFunction intResultEq,
      arrayInt64 = EqFunction intResultEq,
      arrayWord8 = EqFunction intResultEq,
      arrayWord16 = EqFunction intResultEq,
      arrayWord32 = EqFunction intResultEq,
      arrayWord64 = EqFunction intResultEq,
      arrayFloat = EqFunction floatingResultEq,
      arrayDouble = EqFunction floatingResultEq
    }

arraysEq :: Arrays Vector -> Arrays Vector -> Bool
arraysEq aa = getAll . Writer.execWriter . Barbies.bzipWith3M_ go eqArray aa
  where
    go (EqFunction f) va vb
      | Vector.length va /= Vector.length vb = Writer.tell $ All False
      | otherwise = Writer.tell . foldMap All $ Vector.zipWith f va vb
