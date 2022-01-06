{-# LANGUAGE UndecidableInstances #-}

-- | Oleg's duplicating interpreter, specialized for our gnarly type structures.
module Kitty.KGenGenerate.Test.IProduct
  ( IProduct (..),
    splitIProduct,
  )
where

import qualified Barbies
import Data.Functor.Compose (Compose (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Stack (callStack)
import Kitty.KGenGenerate.Test.Error (KSelectIndexError (..), throwKSelectIndexError)
import Kitty.KTypes.ArcTan2 (ArcTan2 (..))
import Kitty.KTypes.BooleanLogic (KAnd (..))
import Kitty.KTypes.Conditional (KSelect (..), KTernary (..))
import Kitty.KTypes.Equality (KEq (..))
import Kitty.KTypes.FMod (FMod (..))
import Kitty.KTypes.FromIntegral (KFromIntegral (..))
import Kitty.KTypes.IEEE (KConvertFloat (..), KIsInfinite (..), KIsNaN (..))
import Kitty.KTypes.KBits (KBits (..))
import Kitty.KTypes.KDivisible (KDivisible (..))
import Kitty.KTypes.KLiteral (KLiteral (..))
import Kitty.KTypes.Round (KRound (..))
import Kitty.KTypes.TotalOrder (KOrd (..))
import Kitty.Prim (Arrays (..), IsPrimitive)

{- "A new, puzzling interpreter" of Kiselyov -}

data IProduct f g a = IPair
  { iProductLeft :: f a,
    iProductRight :: g a
  }

instance (KLiteral f a, KLiteral g a, IsPrimitive a) => KLiteral (IProduct f g) a where
  kliteral b = IPair (kliteral b) (kliteral b)

liftIProduct2 ::
  (f a -> f c -> f b) -> (g a -> g c -> g b) -> IProduct f g a -> IProduct f g c -> IProduct f g b
liftIProduct2 f g (IPair x y) (IPair a b) = IPair (f x a) (g y b)

liftIProduct :: (f a -> f b) -> (g a -> g b) -> IProduct f g a -> IProduct f g b
liftIProduct f g (IPair x y) = IPair (f x) (g y)

instance (Num (f a), Num (g a)) => Num (IProduct f g a) where
  (+) = liftIProduct2 (+) (+)
  (-) = liftIProduct2 (-) (-)
  (*) = liftIProduct2 (*) (*)
  negate = liftIProduct negate negate
  abs = liftIProduct abs abs
  signum = liftIProduct abs abs
  fromInteger i = IPair (fromIntegral i) (fromIntegral i)

instance (Fractional (f a), Fractional (g a)) => Fractional (IProduct f g a) where
  (/) = liftIProduct2 (/) (/)
  fromRational r = IPair (fromRational r) (fromRational r)

instance (Floating (f a), Floating (g a)) => Floating (IProduct f g a) where
  pi = IPair pi pi
  logBase = liftIProduct2 logBase logBase
  (**) = liftIProduct2 (**) (**)
  exp = liftIProduct exp exp
  log = liftIProduct log log
  sin = liftIProduct sin sin
  cos = liftIProduct cos cos
  tan = liftIProduct tan tan
  asin = liftIProduct asin asin
  acos = liftIProduct acos acos
  atan = liftIProduct atan atan
  sinh = liftIProduct sinh sinh
  cosh = liftIProduct cosh cosh
  tanh = liftIProduct tanh tanh
  asinh = liftIProduct asinh asinh
  acosh = liftIProduct acosh acosh
  atanh = liftIProduct atanh atanh

instance (ArcTan2 (f a), ArcTan2 (g a)) => ArcTan2 (IProduct f g a) where
  arctan2 = liftIProduct2 arctan2 arctan2

instance (FMod (f a), FMod (g a)) => FMod (IProduct f g a) where
  fmod = liftIProduct2 fmod fmod

instance (KDivisible (f a), KDivisible (g a)) => KDivisible (IProduct f g a) where
  kMod = liftIProduct2 kMod kMod
  kDiv = liftIProduct2 kDiv kDiv

instance (KAnd f, KAnd g) => KAnd (IProduct f g) where
  (.&&) = liftIProduct2 (.&&) (.&&)
  (.||) = liftIProduct2 (.||) (.||)
  kNot = liftIProduct kNot kNot

instance
  (KRound f a, KRound g a, KTernary (IProduct f g) (IProduct f g a)) =>
  KRound (IProduct f g) a
  where
  kRoundDouble = liftIProduct kRoundDouble kRoundDouble
  kRoundFloat = liftIProduct kRoundFloat kRoundFloat

instance (KBits f a, KBits g a) => KBits (IProduct f g) a where
  testBit pr idx = liftIProduct (`testBit` idx) (`testBit` idx) pr
  setBitTo pr idx val = liftIProduct2 (`setBitTo` idx) (`setBitTo` idx) pr val
  zeroBits = IPair zeroBits zeroBits

instance (KFromIntegral f b, KFromIntegral g b) => KFromIntegral (IProduct f g) b where
  kFromIntegral = liftIProduct kFromIntegral kFromIntegral

instance (KConvertFloat f, KConvertFloat g) => KConvertFloat (IProduct f g) where
  kFloatToDouble = liftIProduct kFloatToDouble kFloatToDouble
  kDoubleToFloat = liftIProduct kDoubleToFloat kDoubleToFloat

instance (KIsInfinite f a, KIsInfinite g a) => KIsInfinite (IProduct f g) a where
  kIsInfinite = liftIProduct kIsInfinite kIsInfinite

instance (KIsNaN f a, KIsNaN g a) => KIsNaN (IProduct f g) a where
  kIsNaN = liftIProduct kIsNaN kIsNaN

instance (KEq f (f a), KEq g (g a)) => KEq (IProduct f g) (IProduct f g a) where
  (.==) = liftIProduct2 (.==) (.==)
  (./=) = liftIProduct2 (./=) (./=)

instance (KOrd f (f a), KOrd g (g a)) => KOrd (IProduct f g) (IProduct f g a) where
  (.<) = liftIProduct2 (.<) (.<)
  (.>) = liftIProduct2 (.>) (.>)
  (.<=) = liftIProduct2 (.<=) (.<=)
  (.>=) = liftIProduct2 (.>=) (.>=)
  kMin = liftIProduct2 kMin kMin
  kMax = liftIProduct2 kMax kMax

instance (KTernary f (f a), KTernary g (g a)) => KTernary (IProduct f g) (IProduct f g a) where
  kTernary (IPair px py) (IPair truex truey) (IPair falsex falsey) =
    IPair (kTernary px truex falsex) (kTernary py truey falsey)

splitIProduct ::
  forall f g.
  (Arrays (Compose Vector (IProduct f g)) -> Arrays (Compose Vector (IProduct f g))) ->
  ( Arrays (Compose Vector f) -> Arrays (Compose Vector f),
    Arrays (Compose Vector g) -> Arrays (Compose Vector g)
  )
splitIProduct f = (fleft, fright)
  where
    fleft = Barbies.bmap outputLeft . f . Barbies.bmap inputLeft
    fright = Barbies.bmap outputRight . f . Barbies.bmap inputRight
    inputLeft :: Compose Vector f a -> Compose Vector (IProduct f g) a
    inputLeft (Compose v) = Compose $ fmap (\x -> IPair x $ dummy "left") v
    dummy side =
      error $ "Undefined " <> side <> "-side argument in splitIProduct!"
    inputRight :: Compose Vector g a -> Compose Vector (IProduct f g) a
    inputRight (Compose v) = Compose $ fmap (IPair (dummy "right")) v
    outputLeft :: Compose Vector (IProduct f g) a -> Compose Vector f a
    outputLeft (Compose v) = Compose $ fmap iProductLeft v
    outputRight :: Compose Vector (IProduct f g) a -> Compose Vector g a
    outputRight (Compose v) = Compose $ fmap iProductRight v

instance (KSelect f, KSelect g) => KSelect (IProduct f g) where
  selectList lst (IPair idxf idxg) = checkLengths $ V.zipWith IPair fs gs
    where
      fs = selectList (lefts tuplist) idxf
      gs = selectList (rights tuplist) idxg
      checkLengths =
        let lenF = length fs
            lenG = length gs
         in if lenF /= lenG
              then throwKSelectIndexError callStack $ KSelectMismatchedProductCases lenF lenG
              else id
      split = fmap (fmap (\(IPair a b) -> (a, b)))
      lefts = fmap (fmap fst)
      rights = fmap (fmap snd)
      tuplist = split lst

  unsafeBoolToZeroOrOne = liftIProduct unsafeBoolToZeroOrOne unsafeBoolToZeroOrOne
