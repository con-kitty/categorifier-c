{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Number of primitives in a 'CType'.
module Kitty.CTypes.ArrayLengths
  ( arrayLengthsCType,
    arrayLengthsCCon,
    primArrayCount,
    maxUnionConLengths,
    Mismatch,
    checkForMismatches,
    showMismatches,
  )
where

import qualified Barbies
import Control.Lens (view)
import Control.Monad.Trans.Writer.CPS (Writer, execWriter, tell)
import Data.Bifunctor (bimap)
import Data.Either (isRight, partitionEithers)
import qualified Data.Foldable as F
import Data.Functor.Const (Const (..))
import Data.Functor.Product (Product (..))
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Semigroup (All (..))
import qualified Data.Text as T
import Data.Word (Word8, Word16, Word32, Word64)
import Kitty.CTypes.Traverse (CTraversalFuns_ (..), traverseCCon_, traverseCType_)
import Kitty.CTypes.Types (CCon, CType, CUnion, CUnionF (..), Prim (..))
import Kitty.Prim (ArrayCount (..), ArrayName (..), Arrays (..), PrimLens, arrayNames, prims_)
import PyF (fmt)

{-# INLINEABLE arrayLengthsCType #-}
arrayLengthsCType :: CType f -> Arrays ArrayCount
arrayLengthsCType = execWriter . traverseCType_ arrayLengthsFuns

{-# INLINEABLE arrayLengthsCCon #-}
arrayLengthsCCon :: CCon f -> Arrays ArrayCount
arrayLengthsCCon = execWriter . traverseCCon_ arrayLengthsFuns

arrayLengthsFuns :: forall f. CTraversalFuns_ (Writer (Arrays ArrayCount)) f
arrayLengthsFuns =
  CTraversalFuns_
    { ctfHandlePrim_ = primFun,
      ctfHandleEnum_ = enumFun,
      ctfHandleUnionCon_ = unionConFun
    }
  where
    enumFun _ = tell $ mempty {arrayWord8 = ArrayCount 1}
    primFun :: Prim f -> Writer (Arrays ArrayCount) ()
    primFun = tell . primArrayCount
    unionConFun :: CUnion f -> Writer (Arrays ArrayCount) ()
    unionConFun cunion@CUnion {cuTag = tag} = do
      _ <- primFun (PrimWord8 tag) -- count tag
      -- count maximum number of each type in the union
      tell (maxUnionConLengths cunion :: Arrays ArrayCount)

primArrayCount :: Prim f -> Arrays ArrayCount
primArrayCount (PrimBool _) = mempty {arrayBool = ArrayCount 1}
primArrayCount (PrimInt8 _) = mempty {arrayInt8 = ArrayCount 1}
primArrayCount (PrimInt16 _) = mempty {arrayInt16 = ArrayCount 1}
primArrayCount (PrimInt32 _) = mempty {arrayInt32 = ArrayCount 1}
primArrayCount (PrimInt64 _) = mempty {arrayInt64 = ArrayCount 1}
primArrayCount (PrimWord8 _) = mempty {arrayWord8 = ArrayCount 1}
primArrayCount (PrimWord16 _) = mempty {arrayWord16 = ArrayCount 1}
primArrayCount (PrimWord32 _) = mempty {arrayWord32 = ArrayCount 1}
primArrayCount (PrimWord64 _) = mempty {arrayWord64 = ArrayCount 1}
primArrayCount (PrimFloat _) = mempty {arrayFloat = ArrayCount 1}
primArrayCount (PrimDouble _) = mempty {arrayDouble = ArrayCount 1}

{-# INLINEABLE maxUnionConLengths #-}

-- | Return the max size needed to contain any of the union constructors, excluding the tag.
maxUnionConLengths :: CUnion f -> Arrays ArrayCount
maxUnionConLengths (CUnion _ cons _ _) = F.foldl' maxOver mempty conLengths
  where
    conLengths = fmap arrayLengthsCCon cons
    maxOver :: Arrays ArrayCount -> Arrays ArrayCount -> Arrays ArrayCount
    maxOver = Barbies.bzipWith maxArrayCount
    maxArrayCount (ArrayCount x) (ArrayCount y) = ArrayCount (max x y)

-- | For each array that matches, we have @`Right` len@, and for the ones that don't,
--   @`Left` (haskellLen, cLen)@.
type Mismatch = Const (Either (Int, Int) Int)

-- | Compares two `Arrays` and returns a diff-like structure iff the inputs don't match.
checkForMismatches :: Arrays ArrayCount -> Arrays ArrayCount -> Maybe (Arrays Mismatch)
checkForMismatches xs ys =
  let mismatches =
        Barbies.bzipWith
          (\(ArrayCount x) (ArrayCount y) -> Const $ if x == y then Right x else Left (x, y))
          xs
          ys
   in if getAll $ Barbies.bfoldMap (All . isRight . getConst) mismatches
        then Nothing
        else pure mismatches

-- * These should move elsewhere once we push the error handling up far enough.

-- | Proposes likely reasons for mismatches.
diagnoseMismatches :: Arrays Mismatch -> [T.Text]
diagnoseMismatches mismatches =
  "No matter what, you can always pass this over to the tools team to sort out." :
  catMaybes
    [ if check (<) (Proxy @Bool)
        && ( check (>) (Proxy @Word8)
               || check (>) (Proxy @Word16)
               || check (>) (Proxy @Word32)
               || check (>) (Proxy @Word64)
           )
        then
          pure
            [fmt|It's possible that Haskell is interpreting something as a bitfield that is
  interpreted as a struct on the C side. Try removing `AsCGBitfield` from new
 `Bool`-containing record field types.|]
        else Nothing,
      if check (>) (Proxy @Bool)
        && ( check (<) (Proxy @Word8)
               || check (<) (Proxy @Word16)
               || check (<) (Proxy @Word32)
               || check (<) (Proxy @Word64)
           )
        then
          pure
            [fmt|It's possible that C is interpreting something as a bitfield that is
  interpreted as a struct on the Haskell side. Try adding `AsCGBitfield` to new
 `Bool`-containing record field types.|]
        else Nothing
    ]
  where
    check :: forall prim. PrimLens prim => (Int -> Int -> Bool) -> Proxy prim -> Bool
    check fn Proxy = either (uncurry fn) (const False) . getConst $ view (prims_ @prim) mismatches

-- | If the array counts are the same, return Nothing.
-- If they're different, return a nicely formatted error message
-- summarizing the differences.
showCountMismatches :: Arrays Mismatch -> T.Text
showCountMismatches match =
  case partitionEithers . Barbies.bfoldMap (pure . f) $ Barbies.bzip arrayNames match of
    (mismatches, []) ->
      [fmt|\n
mismatches:
{T.intercalate "\n" mismatches}

no matches
|]
    (mismatches, matches) ->
      [fmt|\n
mismatches:
  {T.intercalate "\n  " mismatches}

matches:
  {T.intercalate "\n  " matches}
|]
  where
    f :: Product ArrayName (Const (Either (Int, Int) Int)) a -> Either T.Text T.Text
    f (Pair (ArrayName arrayName) (Const mismatch)) =
      bimap
        (\(x, y) -> [fmt|{arrayName}: {x} /= {y}|])
        (\x -> [fmt|{arrayName}: {x} == {x}|])
        mismatch

showMismatches :: Arrays Mismatch -> T.Text
showMismatches mismatches =
  foldr (\h t -> "\n- " <> h <> t) "" (diagnoseMismatches mismatches)
    <> showCountMismatches mismatches
