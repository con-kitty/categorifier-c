{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
-- Data.Text.Prettyprint.Doc is deprecated in prettyprinter-1.7
{-# OPTIONS_GHC -Wno-deprecations #-}

module Categorifier.C.CExpr.C.Operations
  ( genBoolBinOp,
    genBoolUnOp,
    genCast,
    genCastFP,
    genCmpOp,
    genDoubleBinOp,
    genDoubleUnOp,
    genFPTestOp,
    genFloatBinOp,
    genFloatUnOp,
    genInput,
    genIntBinOp,
    genIntSetBit,
    genIntTestOp,
    genIntUnOp,
    genKBranch,
    genOutput,
    genPrimitive,
    genRoundFP,
    genTypeName,
  )
where

import Categorifier.C.CExpr.C.Pretty
  ( commentName,
    indexArray,
    infixBinOp,
    prefixBinOp,
    prefixUnOp,
    ternary,
  )
import Categorifier.C.CExpr.Types
  ( BoolBinOp (..),
    BoolUnOp (..),
    CmpOp (..),
    FPBinOp (..),
    FPTestOp (..),
    FPUnOp (..),
    IntBinOp (..),
    IntSetBit (..),
    IntTestOp (..),
    IntUnOp (..),
  )
import Categorifier.C.Prim
  ( IsPrimitive (..),
    PrimFractional,
    PrimGADT (..),
    PrimIntegral,
    PrimNum,
    PrimSignedIntegral,
    PrimUnsignedIntegral,
    PrimVal,
    primTypeOf,
    pattern BoolVal,
    pattern DoubleVal,
    pattern FloatVal,
    pattern Int16Type,
    pattern Int16Val,
    pattern Int32Type,
    pattern Int32Val,
    pattern Int64Type,
    pattern Int64Val,
    pattern Int8Type,
    pattern Int8Val,
    pattern Word16Val,
    pattern Word32Val,
    pattern Word64Val,
    pattern Word8Val,
  )
import Control.Arrow ((&&&))
import Data.Bool (bool)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text.Prettyprint.Doc (Doc, (<+>))
import qualified Data.Text.Prettyprint.Doc as Doc
import Numeric (showHFloat)
import qualified Numeric (showHex)

genIntBinOp ::
  forall a ann.
  PrimIntegral a =>
  IntBinOp ->
  Doc ann ->
  Doc ann ->
  Doc ann
genIntBinOp = \case
  Add -> infixBinOp "+"
  Mul -> infixBinOp "*"
  Sub -> infixBinOp "-"
  Quot -> genZeroMinProtectedIntOp "/" (genIntLiteralOf @a 0) minVal
  Rem -> \x -> genZeroMinProtectedIntOp "%" x (pure 0) x
  where
    genZeroMinProtectedIntOp op ifZero ifMin x y =
      ternary (Doc.parens . genCmpOp CmpEq y $ genIntLiteralOf @a 0) ifZero
        . Doc.parens
        . fromMaybe id (minValGuard <$> ifMin <*> minVal)
        $ infixBinOp op x y
      where
        minValGuard minDefault mv =
          ternary
            ( Doc.parens $
                genBoolBinOp
                  BAnd
                  (Doc.parens . genCmpOp CmpEq x $ genIntLiteralOf @a mv)
                  (Doc.parens . genCmpOp CmpEq y $ genIntLiteralOf @a (-1))
            )
            (genIntLiteralOf @a minDefault)
    minVal = case primTypeOf @a of
      Int8Type -> Just (toInteger (minBound :: Int8))
      Int16Type -> Just (toInteger (minBound :: Int16))
      Int32Type -> Just (toInteger (minBound :: Int32))
      Int64Type -> Just (toInteger (minBound :: Int64))
      _ -> Nothing

-- | Due to the constraints involved at the Haskell level (where `Num` is responsible for
-- everything), we may get handed situations where the user asks for a sign-related operation on an
-- unsigned integer.  These are not always sensible, but until the numeric hierarchy becomes precise
-- enough to rule them out:
--
--     * `negate` on an unsigned integer still generates a negation in the C code; unsigned integer
--       overflow is standard behavior in C, so this is just a way to move around the ring.
--
--     * `abs` on an unsigned integer is a no-op, because it's already positive.
--
--     * `signum` on an unsigned integer can just never return @-1@ (this may be a sensible thing
--       for the user to ask for, although @./= 0@ is pretty close).
--
-- We also emit comments in these cases.
genIntUnOp ::
  forall a ann.
  PrimIntegral a =>
  IntUnOp ->
  Doc ann ->
  Doc ann
genIntUnOp = case primGADT @a of
  GWord8 -> u @a
  GWord16 -> u @a
  GWord32 -> u @a
  GWord64 -> u @a
  GInt8 -> s @a
  GInt16 -> s @a
  GInt32 -> s @a
  GInt64 -> s @a
  where
    u ::
      forall u.
      PrimUnsignedIntegral u =>
      IntUnOp ->
      Doc ann ->
      Doc ann
    u = \case
      Negate -> commentName "negate on unsigned" . prefixUnOp "-"
      Signum ->
        let mkLiteral = genIntLiteralOf @u
         in \x ->
              commentName ("signum on unsigned value '" <> x <> "'") $
                ternary (Doc.parens $ genCmpOp CmpEq x (mkLiteral 0)) (mkLiteral 0) (mkLiteral 1)
      Abs -> commentName "abs on unsigned"

    s ::
      forall s.
      PrimSignedIntegral s =>
      IntUnOp ->
      Doc ann ->
      Doc ann
    s = \case
      Signum ->
        commentName "signum"
          . uncurry (infixBinOp "-")
          . (Doc.parens . infixBinOp "<" "0" &&& Doc.parens . infixBinOp ">" "0")
      Negate -> minBoundProtectedOp "-"
      Abs -> minBoundProtectedOp signedIntAbs
      where
        -- These values are only used for dealing with the second branch above, involving signed
        -- integers.
        minBoundProtectedOp :: Doc ann -> Doc ann -> Doc ann
        minBoundProtectedOp opName x =
          let minVal = genIntLiteralOf @s signedIntMin
           in ternary (Doc.parens $ genCmpOp CmpEq x minVal) minVal $ prefixUnOp opName x

        signedIntMin :: Integer
        signedIntAbs :: Doc ann
        (signedIntMin, signedIntAbs) = case primGADT @s of
          -- These first two cases (`Int8` and `Int16`) technically don't need to be protected
          -- against undefined behavior, since @abs()@ goes from @int@ to @int@ and can represent
          -- the negation of their minimum values.  We could hope that compiling with @-fwrapv@
          -- would give the right semantics if we didn't generate the special case for @minVal@
          -- (and we previously did this), but that extension is not standard and is known to
          -- interact in unexpected ways with optimizations.  So we just treat small integers the
          -- same way we treat larger ones.
          GInt8 -> (toInteger (minBound :: Int8), "abs")
          GInt16 -> (toInteger (minBound :: Int16), "abs")
          GInt32 -> (toInteger (minBound :: Int32), "abs")
          GInt64 -> (toInteger (minBound :: Int64), "labs")

genIntTestOp ::
  forall a ann.
  PrimIntegral a =>
  IntTestOp ->
  Doc ann ->
  Doc ann ->
  Doc ann
genIntTestOp TestBit index intVal = Doc.parens "bool" <+> Doc.parens testBit
  where
    one = genIntLiteralOf @a 1
    testBit = commentName "testBit" . infixBinOp "&" intVal $ Doc.parens (one <+> "<<" <+> index)

genIntSetBit ::
  forall a ann.
  PrimIntegral a =>
  IntSetBit ->
  Doc ann ->
  Doc ann ->
  Doc ann ->
  Doc ann
genIntSetBit SetBit intVal index bitVal = ternary bitVal (Doc.parens setBit) (Doc.parens clearBit)
  where
    one = genIntLiteralOf @a 1
    setBit = commentName "setBit" . infixBinOp "|" intVal $ Doc.parens (one <+> "<<" <+> index)
    clearBit =
      commentName "clearBit" . infixBinOp "&" intVal $
        Doc.parens (prefixUnOp "~" $ one <+> "<<" <+> index)

genRoundFP :: forall a ann. PrimFractional a => Doc ann -> Doc ann
genRoundFP = prefixUnOp $ case primGADT @a of
  GDouble -> "rint"
  GFloat -> "rintf"

-- | Cast from a fractional type.
genCastFP ::
  forall to ann.
  PrimNum to =>
  Doc ann ->
  Doc ann
genCastFP input = case primGADT @to of
  GInt8 -> genCastFPToInt @to
  GInt16 -> genCastFPToInt @to
  GInt32 -> genCastFPToInt @to
  GInt64 -> genCastFPToInt @to
  GWord8 -> genCastFPToInt @to
  GWord16 -> genCastFPToInt @to
  GWord32 -> genCastFPToInt @to
  GWord64 -> genCastFPToInt @to
  GFloat -> genCast @to input
  GDouble -> genCast @to input
  where
    genCastFPToInt :: forall i. PrimIntegral i => Doc ann
    genCastFPToInt = protectCastFP @i input plainRound
    plainRound = Doc.parens (genTypeName @to) <+> input

-- | Check for infinity and NaN when casting a fractional type to an integral type.
protectCastFP ::
  forall to ann.
  PrimIntegral to =>
  Doc ann ->
  Doc ann ->
  Doc ann
protectCastFP input =
  -- Check for Inf and NaN; this way if one is encountered, nothing other than the literal zero will
  -- be evaluated.  This is more compact than SBV's approach.
  genKBranch
    (Doc.parens $ genBoolBinOp BOr (genFPTestOp FPIsNaN input) (genFPTestOp FPIsInfinite input))
    zero
  where
    zero = case primGADT @to of
      GInt8 -> genIntLiteralOf @to 0
      GInt16 -> genIntLiteralOf @to 0
      GInt32 -> genIntLiteralOf @to 0
      GInt64 -> genIntLiteralOf @to 0
      GWord8 -> genIntLiteralOf @to 0
      GWord16 -> genIntLiteralOf @to 0
      GWord32 -> genIntLiteralOf @to 0
      GWord64 -> genIntLiteralOf @to 0

-- | Cast from an integral type.
genCast ::
  forall to ann.
  PrimNum to =>
  Doc ann ->
  Doc ann
genCast input = Doc.parens (genTypeName @to) <+> input

genDoubleBinOp :: FPBinOp -> Doc ann -> Doc ann -> Doc ann
genDoubleBinOp FPPow = prefixBinOp "pow"
genDoubleBinOp FPAtan2 = prefixBinOp "atan2"
genDoubleBinOp FPFmod = prefixBinOp "fmod"
genDoubleBinOp FPMax = prefixBinOp "fmax"
genDoubleBinOp FPMin = prefixBinOp "fmin"
genDoubleBinOp FPAdd = infixBinOp "+"
genDoubleBinOp FPMul = infixBinOp "*"
genDoubleBinOp FPSub = infixBinOp "-"
genDoubleBinOp FPDiv = infixBinOp "/"

genFloatBinOp :: FPBinOp -> Doc ann -> Doc ann -> Doc ann
genFloatBinOp FPPow = prefixBinOp "powf"
genFloatBinOp FPAtan2 = prefixBinOp "atan2f"
genFloatBinOp FPFmod = prefixBinOp "fmodf"
genFloatBinOp FPMax = prefixBinOp "fmaxf"
genFloatBinOp FPMin = prefixBinOp "fminf"
genFloatBinOp FPAdd = infixBinOp "+"
genFloatBinOp FPMul = infixBinOp "*"
genFloatBinOp FPSub = infixBinOp "-"
genFloatBinOp FPDiv = infixBinOp "/"

genDoubleUnOp :: FPUnOp -> Doc ann -> Doc ann
genDoubleUnOp =
  prefixUnOp . \case
    FPNegate -> "-"
    FPAbs -> "fabs"
    FPSqrt -> "sqrt"
    FPExp -> "exp"
    FPLog -> "log"
    FPSin -> "sin"
    FPCos -> "cos"
    FPTan -> "tan"
    FPASin -> "asin"
    FPACos -> "acos"
    FPATan -> "atan"
    FPSinh -> "sinh"
    FPCosh -> "cosh"
    FPTanh -> "tanh"

genFloatUnOp :: FPUnOp -> Doc ann -> Doc ann
genFloatUnOp =
  prefixUnOp . \case
    FPNegate -> "-"
    FPAbs -> "fabsf"
    FPSqrt -> "sqrtf"
    FPExp -> "expf"
    FPLog -> "logf"
    FPSin -> "sinf"
    FPCos -> "cosf"
    FPTan -> "tanf"
    FPASin -> "asinf"
    FPACos -> "acosf"
    FPATan -> "atanf"
    FPSinh -> "sinhf"
    FPCosh -> "coshf"
    FPTanh -> "tanhf"

genFPTestOp :: FPTestOp -> Doc ann -> Doc ann
genFPTestOp =
  prefixUnOp . \case
    FPIsNaN -> "isnan"
    FPIsInfinite -> "isinf"
    FPSignBit -> "signbit"

genCmpOp :: CmpOp -> Doc ann -> Doc ann -> Doc ann
genCmpOp =
  infixBinOp . \case
    CmpEq -> "=="
    CmpNeq -> "!="
    CmpGT -> ">"
    CmpLT -> "<"
    CmpGE -> ">="
    CmpLE -> "<="

genBoolBinOp :: BoolBinOp -> Doc ann -> Doc ann -> Doc ann
genBoolBinOp =
  infixBinOp . \case
    BAnd -> "&&"
    BOr -> "||"

genBoolUnOp :: BoolUnOp -> Doc ann -> Doc ann
genBoolUnOp BNot = prefixUnOp "!"

genKBranch :: Doc ann -> Doc ann -> Doc ann -> Doc ann
genKBranch = ternary

genBoolLiteral :: Bool -> Doc ann
genBoolLiteral = bool "false" "true"

genFloatLiteral :: Float -> Doc ann
genFloatLiteral = genFPLiteral "float" "F"

genDoubleLiteral :: Double -> Doc ann
genDoubleLiteral = genFPLiteral "double" ""

genFPLiteral :: (Doc.Pretty a, RealFloat a) => Doc ann -> Doc ann -> a -> Doc ann
genFPLiteral typeName suffix f
  | isNaN f =
      "((" <> typeName <> ") (NAN))"
  | isInfinite f,
    f < 0 =
      "((" <> typeName <> ") (-INFINITY))"
  | isInfinite f =
      "((" <> typeName <> ") (INFINITY))"
  | otherwise =
      Doc.pretty (showHFloat f "") <> suffix
        -- Add a comment in the source with the normal decimal representation.
        <+> "/* " <> Doc.pretty f <> suffix <> " */"

genIntLiteral :: Bool -> IntSize -> Integer -> Doc ann
genIntLiteral signed size i
  | signed, (size, i) `elem` mins = intLabel <> "_MIN"
  | otherwise = constantWrap $ sign <> "0x" <> zeroPad padLength <> Doc.pretty value
  where
    sign = if i < 0 then "-" else ""
    value = Numeric.showHex (abs i) ""
    zeroPad n = Doc.pretty $ replicate (n - length value) '0'
    padLength = (intBitSize size + 3) `div` 4
    intLabel = "INT" <> Doc.pretty (intBitSize size)
    constWrapper = (if signed then "" else "U") <> intLabel <> "_C"
    constantWrap d = constWrapper <> Doc.parens d
    mins =
      [ (Byte, toInteger (minBound :: Int8)),
        (TwoBytes, toInteger (minBound :: Int16)),
        (FourBytes, toInteger (minBound :: Int32)),
        (EightBytes, toInteger (minBound :: Int64))
      ]

genIntLiteralOf :: forall a ann. PrimIntegral a => Integer -> Doc ann
genIntLiteralOf = genIntLiteral isSigned size
  where
    (isSigned, size) = case primGADT @a of
      GInt8 -> (True, Byte)
      GInt16 -> (True, TwoBytes)
      GInt32 -> (True, FourBytes)
      GInt64 -> (True, EightBytes)
      GWord8 -> (False, Byte)
      GWord16 -> (False, TwoBytes)
      GWord32 -> (False, FourBytes)
      GWord64 -> (False, EightBytes)

-- TODO (ziyang): lift this into type level.
data IntSize
  = Byte
  | TwoBytes
  | FourBytes
  | EightBytes
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

intBitSize :: IntSize -> Int
intBitSize Byte = 8
intBitSize TwoBytes = 16
intBitSize FourBytes = 32
intBitSize EightBytes = 64

-- | The logic for generating all literal values is heavily borrowed from SBV.
genPrimitive :: PrimVal -> Doc ann
genPrimitive (BoolVal b) = genBoolLiteral b
genPrimitive (Int8Val i) = genIntLiteral True Byte $ toInteger i
genPrimitive (Int16Val i) = genIntLiteral True TwoBytes $ toInteger i
genPrimitive (Int32Val i) = genIntLiteral True FourBytes $ toInteger i
genPrimitive (Int64Val i) = genIntLiteral True EightBytes $ toInteger i
genPrimitive (Word8Val i) = genIntLiteral False Byte $ toInteger i
genPrimitive (Word16Val i) = genIntLiteral False TwoBytes $ toInteger i
genPrimitive (Word32Val i) = genIntLiteral False FourBytes $ toInteger i
genPrimitive (Word64Val i) = genIntLiteral False EightBytes $ toInteger i
genPrimitive (FloatVal f) = genFloatLiteral f
genPrimitive (DoubleVal d) = genDoubleLiteral d

-- TODO (ziyang): lift this into type level.
genTypeName :: forall a b. (IsPrimitive a, IsString b) => b
genTypeName = case primGADT @a of
  GBool -> "bool"
  GInt8 -> "int8_t"
  GInt16 -> "int16_t"
  GInt32 -> "int32_t"
  GInt64 -> "int64_t"
  GWord8 -> "uint8_t"
  GWord16 -> "uint16_t"
  GWord32 -> "uint32_t"
  GWord64 -> "uint64_t"
  GFloat -> "float"
  GDouble -> "double"

genInput :: forall a ann. IsPrimitive a => Int -> Doc ann
genInput idx = indexArray arrayName $ Doc.pretty idx
  where
    arrayName = "input_" <> genTypeName @a

genOutput :: forall a ann. IsPrimitive a => Int -> Doc ann
genOutput idx = indexArray arrayName $ Doc.pretty idx
  where
    arrayName = "output_" <> genTypeName @a
