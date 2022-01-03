{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | Top-level constraint for higher-kinded types
module Kitty.KTypes.KType1
  ( KType1,

    -- * Handy constraint synonyms
    AllPolyVec,
    AllKTernary,
    KDivisibleStuff,
    AllKOrd,
    KNum,
    KNumPrimitives,
    IntStuff,
    FloatStuff,
    KRoundPrimitives,
    AllKFromIntegral,
    AllKBits,
    KLiteralPrimitives,
    KCGenericPrimitives,
  )
where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Typeable (Typeable)
import Data.Word (Word8, Word16, Word32, Word64)
import Kitty.CTypes.CGeneric.Class (CGeneric)
import Kitty.KTypes.ArcTan2 (ArcTan2)
import Kitty.KTypes.Conditional (KTernary)
import Kitty.KTypes.FMod (FMod)
import Kitty.KTypes.FromIntegral (KFromIntegral)
import Kitty.KTypes.Function (KFunCall)
import Kitty.KTypes.IEEE (KConvertFloat, KIsInfinite, KIsNaN)
import Kitty.KTypes.KBits (KBits)
import Kitty.KTypes.KDivisible (KDivisible)
import Kitty.KTypes.KLiteral (KLiteralPrimitives)
import Kitty.KTypes.Round (KRound)
import Kitty.KTypes.SwitchCase (KIf)
import Kitty.KTypes.TotalOrder (KOrd)
import Kitty.PolyVec (AllPolyVec)
import Kitty.TypeUtils.Lists (AllInstancesOf, TypeMap)

type KNumPrimitives =
  '[ Int8,
     Int16,
     Int32,
     Int64,
     Word8,
     Word16,
     Word32,
     Word64,
     Float,
     Double
   ]

type KNum f = AllInstancesOf Num (TypeMap f KNumPrimitives)

-- | Does @f@ have a 'Num' instance for of all the integer primitive types?
type IntStuff f =
  ( Num (f Word8),
    Num (f Word16),
    Num (f Word32),
    Num (f Word64),
    Num (f Int8),
    Num (f Int16),
    Num (f Int32),
    Num (f Int64)
  )

-- | Does @f@ have various floating-point maths instances for 'Double' and 'Float'?
type FloatStuff f =
  ( Floating (f Double),
    ArcTan2 (f Double),
    FMod (f Double),
    Floating (f Float),
    ArcTan2 (f Float),
    FMod (f Float)
  )

type AllKTernary f =
  ( KTernary f (f Bool),
    KTernary f (f Word8),
    KTernary f (f Word16),
    KTernary f (f Word32),
    KTernary f (f Word64),
    KTernary f (f Int8),
    KTernary f (f Int16),
    KTernary f (f Int32),
    KTernary f (f Int64),
    KTernary f (f Float),
    KTernary f (f Double)
  )

-- | Does @f@ have a 'KDivisible' instance for of all the integer primitive types?
type KDivisibleStuff f =
  ( KDivisible (f Word8),
    KDivisible (f Word16),
    KDivisible (f Word32),
    KDivisible (f Word64),
    KDivisible (f Int8),
    KDivisible (f Int16),
    KDivisible (f Int32),
    KDivisible (f Int64)
  )

-- | Does @f@ have a 'KOrd' instance for of all the primitive types?
type AllKOrd f =
  ( KOrd f (f Bool),
    KOrd f (f Word8),
    KOrd f (f Word16),
    KOrd f (f Word32),
    KOrd f (f Word64),
    KOrd f (f Int8),
    KOrd f (f Int16),
    KOrd f (f Int32),
    KOrd f (f Int64),
    KOrd f (f Float),
    KOrd f (f Double)
  )

type KRoundPrimitives f =
  ( KRound f Word8,
    KRound f Word16,
    KRound f Word32,
    KRound f Word64,
    KRound f Int8,
    KRound f Int16,
    KRound f Int32,
    KRound f Int64
  )

type AllKFromIntegral f =
  ( KFromIntegral f Word8,
    KFromIntegral f Word16,
    KFromIntegral f Word32,
    KFromIntegral f Word64,
    KFromIntegral f Int8,
    KFromIntegral f Int16,
    KFromIntegral f Int32,
    KFromIntegral f Int64,
    KFromIntegral f Float,
    KFromIntegral f Double
  )

-- | Does @f@ have a 'KBits' instance for of all the "Data.Word" primitive types?
type AllKBits f =
  ( KBits f Word8,
    KBits f Word16,
    KBits f Word32,
    KBits f Word64
  )

type KCGenericPrimitives f =
  ( CGeneric (f Bool),
    CGeneric (f Word8),
    CGeneric (f Word16),
    CGeneric (f Word32),
    CGeneric (f Word64),
    CGeneric (f Int8),
    CGeneric (f Int16),
    CGeneric (f Int32),
    CGeneric (f Int64),
    CGeneric (f Float),
    CGeneric (f Double)
  )

-- | Morally, this should be a type of kind 'GHC.Exts.Constraint' to make control/modeling types
-- easier to write. It's currently implemented as a class to prevent having to switch on
-- @-XConstraintKinds@ everywhere.
class
  ( AllPolyVec f,
    AllKBits f,
    AllKOrd f,
    AllKFromIntegral f,
    KIsInfinite f Float,
    KIsInfinite f Double,
    KIsNaN f Float,
    KIsNaN f Double,
    KDivisibleStuff f,
    KConvertFloat f,
    KRoundPrimitives f,
    FloatStuff f,
    IntStuff f,
    KLiteralPrimitives f,
    KIf f,
    KFunCall f,
    Typeable f,
    KCGenericPrimitives f
  ) =>
  KType1 f
