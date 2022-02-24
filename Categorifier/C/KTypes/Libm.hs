{-# LANGUAGE ForeignFunctionInterface #-}

module Categorifier.C.KTypes.Libm
  ( -- * Direct FFI bindings
    c_fmod,
    c_atan2,
    c_fmin,
    c_fmax,
    c_log,
    c_exp,
    c_pow,
    c_sin,
    c_cos,
    c_tan,
    c_asin,
    c_acos,
    c_atan,
    c_sinh,
    c_cosh,
    c_tanh,
    c_fmodf,
    c_atan2f,
    c_fminf,
    c_fmaxf,
    c_logf,
    c_expf,
    c_powf,
    c_sinf,
    c_cosf,
    c_tanf,
    c_asinf,
    c_acosf,
    c_atanf,
    c_sinhf,
    c_coshf,
    c_tanhf,

    -- * Haskell type-converted bindings
    libmFMod,
    libmAtan2,
    libmFmin,
    libmFmax,
    libmLog,
    libmExp,
    libmPow,
    libmSin,
    libmCos,
    libmTan,
    libmAsin,
    libmAcos,
    libmAtan,
    libmSinh,
    libmCosh,
    libmTanh,
    libmFModf,
    libmAtan2f,
    libmFminf,
    libmFmaxf,
    libmLogf,
    libmExpf,
    libmPowf,
    libmSinf,
    libmCosf,
    libmTanf,
    libmAsinf,
    libmAcosf,
    libmAtanf,
    libmSinhf,
    libmCoshf,
    libmTanhf,
  )
where

import Foreign.C.Types (CDouble (..), CFloat (..))

foreign import ccall unsafe "math.h fmod" c_fmod :: CDouble -> CDouble -> CDouble

foreign import ccall unsafe "math.h atan2" c_atan2 :: CDouble -> CDouble -> CDouble

foreign import ccall unsafe "math.h fmin" c_fmin :: CDouble -> CDouble -> CDouble

foreign import ccall unsafe "math.h fmax" c_fmax :: CDouble -> CDouble -> CDouble

foreign import ccall unsafe "math.h log" c_log :: CDouble -> CDouble

foreign import ccall unsafe "math.h exp" c_exp :: CDouble -> CDouble

foreign import ccall unsafe "math.h pow" c_pow :: CDouble -> CDouble -> CDouble

foreign import ccall unsafe "math.h sin" c_sin :: CDouble -> CDouble

foreign import ccall unsafe "math.h cos" c_cos :: CDouble -> CDouble

foreign import ccall unsafe "math.h tan" c_tan :: CDouble -> CDouble

foreign import ccall unsafe "math.h asin" c_asin :: CDouble -> CDouble

foreign import ccall unsafe "math.h acos" c_acos :: CDouble -> CDouble

foreign import ccall unsafe "math.h atan" c_atan :: CDouble -> CDouble

foreign import ccall unsafe "math.h sinh" c_sinh :: CDouble -> CDouble

foreign import ccall unsafe "math.h cosh" c_cosh :: CDouble -> CDouble

foreign import ccall unsafe "math.h tanh" c_tanh :: CDouble -> CDouble

foreign import ccall unsafe "math.h fmodf" c_fmodf :: CFloat -> CFloat -> CFloat

foreign import ccall unsafe "math.h atan2f" c_atan2f :: CFloat -> CFloat -> CFloat

foreign import ccall unsafe "math.h fminf" c_fminf :: CFloat -> CFloat -> CFloat

foreign import ccall unsafe "math.h fmaxf" c_fmaxf :: CFloat -> CFloat -> CFloat

foreign import ccall unsafe "math.h logf" c_logf :: CFloat -> CFloat

foreign import ccall unsafe "math.h expf" c_expf :: CFloat -> CFloat

foreign import ccall unsafe "math.h powf" c_powf :: CFloat -> CFloat -> CFloat

foreign import ccall unsafe "math.h sinf" c_sinf :: CFloat -> CFloat

foreign import ccall unsafe "math.h cosf" c_cosf :: CFloat -> CFloat

foreign import ccall unsafe "math.h tanf" c_tanf :: CFloat -> CFloat

foreign import ccall unsafe "math.h asinf" c_asinf :: CFloat -> CFloat

foreign import ccall unsafe "math.h acosf" c_acosf :: CFloat -> CFloat

foreign import ccall unsafe "math.h atanf" c_atanf :: CFloat -> CFloat

foreign import ccall unsafe "math.h sinhf" c_sinhf :: CFloat -> CFloat

foreign import ccall unsafe "math.h coshf" c_coshf :: CFloat -> CFloat

foreign import ccall unsafe "math.h tanhf" c_tanhf :: CFloat -> CFloat

onCDouble :: (CDouble -> CDouble) -> Double -> Double
onCDouble f x = ret
  where
    CDouble ret = f (CDouble x)

onTwoCDoubles :: (CDouble -> CDouble -> CDouble) -> Double -> Double -> Double
onTwoCDoubles f x y = ret
  where
    CDouble ret = f (CDouble x) (CDouble y)

onCFloat :: (CFloat -> CFloat) -> Float -> Float
onCFloat f x = ret
  where
    CFloat ret = f (CFloat x)

onTwoCFloats :: (CFloat -> CFloat -> CFloat) -> Float -> Float -> Float
onTwoCFloats f x y = ret
  where
    CFloat ret = f (CFloat x) (CFloat y)

libmAtan2 :: Double -> Double -> Double
libmAtan2 = onTwoCDoubles c_atan2

libmFmin :: Double -> Double -> Double
libmFmin = onTwoCDoubles c_fmin

libmFmax :: Double -> Double -> Double
libmFmax = onTwoCDoubles c_fmax

libmPow :: Double -> Double -> Double
libmPow = onTwoCDoubles c_pow

libmFMod :: Double -> Double -> Double
libmFMod = onTwoCDoubles c_fmod

libmFModf :: Float -> Float -> Float
libmFModf = onTwoCFloats c_fmodf

libmAtan2f :: Float -> Float -> Float
libmAtan2f = onTwoCFloats c_atan2f

libmFminf :: Float -> Float -> Float
libmFminf = onTwoCFloats c_fminf

libmFmaxf :: Float -> Float -> Float
libmFmaxf = onTwoCFloats c_fmaxf

libmPowf :: Float -> Float -> Float
libmPowf = onTwoCFloats c_powf

libmLog :: Double -> Double
libmLog = onCDouble c_log

libmExp :: Double -> Double
libmExp = onCDouble c_exp

libmSin :: Double -> Double
libmSin = onCDouble c_sin

libmCos :: Double -> Double
libmCos = onCDouble c_cos

libmTan :: Double -> Double
libmTan = onCDouble c_tan

libmAsin :: Double -> Double
libmAsin = onCDouble c_asin

libmAcos :: Double -> Double
libmAcos = onCDouble c_acos

libmAtan :: Double -> Double
libmAtan = onCDouble c_atan

libmSinh :: Double -> Double
libmSinh = onCDouble c_sinh

libmCosh :: Double -> Double
libmCosh = onCDouble c_cosh

libmTanh :: Double -> Double
libmTanh = onCDouble c_tanh

libmLogf :: Float -> Float
libmLogf = onCFloat c_logf

libmExpf :: Float -> Float
libmExpf = onCFloat c_expf

libmSinf :: Float -> Float
libmSinf = onCFloat c_sinf

libmCosf :: Float -> Float
libmCosf = onCFloat c_cosf

libmTanf :: Float -> Float
libmTanf = onCFloat c_tanf

libmAsinf :: Float -> Float
libmAsinf = onCFloat c_asinf

libmAcosf :: Float -> Float
libmAcosf = onCFloat c_acosf

libmAtanf :: Float -> Float
libmAtanf = onCFloat c_atanf

libmSinhf :: Float -> Float
libmSinhf = onCFloat c_sinhf

libmCoshf :: Float -> Float
libmCoshf = onCFloat c_coshf

libmTanhf :: Float -> Float
libmTanhf = onCFloat c_tanhf
