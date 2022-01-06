{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Pretty-printing in tagless-final style.
module Kitty.KGenGenerate.Test.Pretty
  ( Pretty (..),
    prettyPrint,
    specToPrettyArray,
  )
where

import qualified Barbies
import Data.Foldable (fold, toList)
import Data.Functor.Classes (Show1 (..))
import Data.Functor.Compose (Compose (..))
import qualified Data.List as List
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text (pack)
import qualified Data.Text.Lazy as Text (toStrict)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Text
import Data.Typeable (Typeable, typeOf, typeRep)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
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
import Kitty.Prim
  ( ArrayCount (..),
    ArrayName (..),
    Arrays (..),
    IsPrimitive,
    PrimNum,
    arrayNames,
  )

-- | This wrapper gives instances that render themselves. If you write a program and use the
-- 'Pretty' interpretation, the semantics of the rendered program should be the same, but the
-- formatting may be slightly off. However, if you take _that_ program and use the 'Pretty'
-- interpretation, the result will be identical.
--
-- The point of this is to be able to render randomly-generated terms.
newtype Pretty a = Pretty {getPretty :: Text.Builder}

bld :: Text -> Builder
bld = Text.fromText

tshow :: Show a => a -> Text
tshow = Text.pack . show

bshow :: Show a => a -> Builder
bshow = bld . tshow

prettyPrint :: Pretty a -> Text
prettyPrint (Pretty x) = Text.toStrict $ Text.toLazyText x

parenthesise2 :: Text -> Builder -> Builder -> Builder
parenthesise2 ifx a b = "(" <> a <> " " <> bld ifx <> " " <> b <> ")"

parenthesise1 :: Text -> Builder -> Builder
parenthesise1 pfx a = "(" <> bld pfx <> " " <> a <> ")"

parenthesise2P :: Text -> Pretty a1 -> Pretty a2 -> Pretty a3
parenthesise2P ifx (Pretty a) (Pretty b) = Pretty $ parenthesise2 ifx a b

parenthesise1P :: Text -> Pretty a1 -> Pretty a2
parenthesise1P pfx (Pretty a) = Pretty $ parenthesise1 pfx a

parenthesise1PWithType :: Builder -> Text -> Pretty a1 -> Pretty a2
parenthesise1PWithType ty pfx (Pretty a) =
  Pretty $ "(" <> parenthesise1 pfx a <> " :: f " <> ty <> ")"

instance KEq Pretty (Pretty a) where
  (.==) = parenthesise2P ".=="
  (./=) = parenthesise2P "./="

instance KOrd Pretty (Pretty a) where
  (.<) = parenthesise2P ".<"
  (.>) = parenthesise2P ".>"
  (.<=) = parenthesise2P ".<="
  (.>=) = parenthesise2P ".>="
  kMin = parenthesise2P "`kMin`"
  kMax = parenthesise2P "`kMax`"

instance (IsPrimitive a, Show a, Typeable a) => KLiteral Pretty a where
  kliteral b =
    Pretty $
      "(kliteral (" <> bshow b <> " :: " <> shownTy
        <> ") :: f "
        <> shownTy
        <> ")"
    where
      shownTy = bshow $ typeOf b

instance KTernary Pretty (Pretty a) where
  kTernary (Pretty predicate) (Pretty true) (Pretty false) =
    Pretty $ "(kTernary " <> predicate <> " " <> true <> " " <> false <> ")"

instance KAnd Pretty where
  (.&&) = parenthesise2P ".&&"
  (.||) = parenthesise2P ".||"
  kNot = parenthesise1P "kNot"

instance Num (Pretty a) where
  (+) = parenthesise2P "+"
  (-) = parenthesise2P "-"
  (*) = parenthesise2P "*"
  negate = parenthesise1P "-"
  abs = parenthesise1P "abs"
  signum = parenthesise1P "signum"
  fromInteger i =
    if i < 0
      then Pretty ("(" <> bshow i <> ")")
      else Pretty (bshow i)

instance Fractional a => Fractional (Pretty a) where
  (/) = parenthesise2P "/"
  fromRational r = Pretty ("(fromRational (" <> bshow r <> "))")

instance Floating a => Floating (Pretty a) where
  pi = Pretty "pi"
  logBase = parenthesise2P "`logBase`"
  (**) = parenthesise2P "**"
  exp = parenthesise1P "exp"
  log = parenthesise1P "log"
  sin = parenthesise1P "sin"
  cos = parenthesise1P "cos"
  tan = parenthesise1P "tan"
  asin = parenthesise1P "asin"
  acos = parenthesise1P "acos"
  atan = parenthesise1P "atan"
  sinh = parenthesise1P "sinh"
  cosh = parenthesise1P "cosh"
  tanh = parenthesise1P "tanh"
  asinh = parenthesise1P "asinh"
  acosh = parenthesise1P "acosh"
  atanh = parenthesise1P "atanh"

instance ArcTan2 a => ArcTan2 (Pretty a) where
  arctan2 = parenthesise2P "`arctan2`"

instance FMod (Pretty a) where
  fmod = parenthesise2P "`fmod`"

instance KDivisible (Pretty a) where
  kMod = parenthesise2P "`kMod`"
  kDiv = parenthesise2P "`kDiv`"

instance
  (KTernary Pretty (Pretty a), Integral a, Bounded a, KLiteral Pretty a) =>
  KRound Pretty a
  where
  kRoundDouble x = parenthesise1PWithType (bshow (typeOf x)) "kRoundDouble" x
  kRoundFloat x = parenthesise1PWithType (bshow (typeOf x)) "kRoundFloat" x

instance KBits Pretty a where
  testBit bm idx = parenthesise1P ("(`testBit` (" <> tshow idx <> " :: Int))") bm

  setBitTo (Pretty bm) idx (Pretty val) =
    Pretty $ "(setBitTo " <> bm <> " (" <> bshow idx <> " :: Int) " <> val <> ")"

  zeroBits = Pretty "zeroBits"

instance PrimNum b => KFromIntegral Pretty b where
  kFromIntegral (Pretty x) =
    Pretty . parenthesise1 "kFromIntegral" $
      "(" <> x <> ") :: f " <> bshow (typeRep (Proxy @b))

instance KConvertFloat Pretty where
  kFloatToDouble = parenthesise1P "kFloatToDouble"
  kDoubleToFloat = parenthesise1P "kDoubleToFloat"

instance KIsInfinite Pretty a where
  kIsInfinite = parenthesise1P "kIsInfinite"

instance KIsNaN Pretty a where
  kIsNaN = parenthesise1P "kIsNaN"

specToPrettyArray :: Arrays ArrayCount -> Arrays (Compose Vector Pretty)
specToPrettyArray counts = Barbies.bzipWith go counts arrayNames
  where
    go (ArrayCount n) (ArrayName name) =
      Compose . Vector.fromList $ fmap (\i -> Pretty (bld name <> "_" <> bshow i)) [0 .. n - 1]

instance Show1 Pretty where
  liftShowsPrec _ _ prec = showsPrec prec . prettyPrint

-- | 'selectList' must produce a list of outputs.  As with "Kitty.GKenGenerate.Test.InputReader", we
-- don't really have a way to know here how many output values will be produced here, but we do know
-- that our plugin tests only work on scalar values, so we expect that returning a singleton list is
-- OK for now.
--
-- Changing 'selectList' to operate on a functor with known size might make this possible, as we
-- would know the expected output structure independent of which branch gets selected.  We already
-- know this is legit, as we cannot code-generate variable-length outputs.
instance KSelect Pretty where
  selectList lst (Pretty idx) =
    pure . Pretty $ "selectList " <> prettyList (fmap (prettyList . fmap getPretty) lst) <> idx
    where
      prettyList :: Foldable f => f Text.Builder -> Text.Builder
      prettyList elems = "[ " <> fold (List.intersperse ", " $ toList elems) <> " ]"

  unsafeBoolToZeroOrOne = parenthesise1P "unsafeBoolToZeroOrOne"
