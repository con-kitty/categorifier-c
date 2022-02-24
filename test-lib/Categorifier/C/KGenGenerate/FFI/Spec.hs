{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Categorifier.C.KGenGenerate.FFI.Spec
  ( -- * Function specs
    genFunSpec,
    cgStateToSpec,

    -- * Function spec helpers
    getCounts,
    specFileName,
    inputSpecSizeFunName,
    outputSpecSizeFunName,
    inputSpecFunName,
    outputSpecFunName,
    arrayCountsToSpec,
  )
where

import Categorifier.C.Codegen.FFI.Spec (FFIArrayCount (..), SBVFFISpec, Spec (..), arrayCountsToSpec)
import Categorifier.C.Prim (ArrayCount, Arrays, HasArrays (..), countLenses, _ArrayCount)
import Control.Lens (over, to, view)
import Data.List.Extra (intercalate, nubOrd)
import qualified Data.SBV.Internals as SBV
import qualified Text.Casing as Casing

cgPrimTypes :: [SBV.Kind]
cgPrimTypes =
  [ SBV.KDouble,
    SBV.KFloat,
    SBV.KBool,
    SBV.KBounded True 8,
    SBV.KBounded True 16,
    SBV.KBounded True 32,
    SBV.KBounded True 64,
    SBV.KBounded False 8,
    SBV.KBounded False 16,
    SBV.KBounded False 32,
    SBV.KBounded False 64
  ]

-- | This simply maps a kind to the appropriate array count.
sbvKindToArrayCount :: SBV.Kind -> (Int -> Int) -> Arrays ArrayCount -> Arrays ArrayCount
sbvKindToArrayCount SBV.KDouble =
  over (arrayDouble_ . _ArrayCount)
sbvKindToArrayCount SBV.KFloat =
  over (arrayFloat_ . _ArrayCount)
sbvKindToArrayCount SBV.KBool =
  over (arrayBool_ . _ArrayCount)
sbvKindToArrayCount (SBV.KBounded True 8) =
  over (arrayInt8_ . _ArrayCount)
sbvKindToArrayCount (SBV.KBounded True 16) =
  over (arrayInt16_ . _ArrayCount)
sbvKindToArrayCount (SBV.KBounded True 32) =
  over (arrayInt32_ . _ArrayCount)
sbvKindToArrayCount (SBV.KBounded True 64) =
  over (arrayInt64_ . _ArrayCount)
sbvKindToArrayCount (SBV.KBounded False 8) =
  over (arrayWord8_ . _ArrayCount)
sbvKindToArrayCount (SBV.KBounded False 16) =
  over (arrayWord16_ . _ArrayCount)
sbvKindToArrayCount (SBV.KBounded False 32) =
  over (arrayWord32_ . _ArrayCount)
sbvKindToArrayCount (SBV.KBounded False 64) =
  over (arrayWord64_ . _ArrayCount)
sbvKindToArrayCount k =
  error $
    "Argument arrays of kind '" <> show k <> "' are incompatible with the KGen calling convention!"

kindOf :: SBV.SV -> SBV.Kind
kindOf (SBV.SV k _) = k

-- | NB: This returned "spec" is after the min-size transform (see 'arrayCountsToSpec').
cgStateToSpec :: SBV.CgState -> SBVFFISpec
cgStateToSpec SBV.CgState {..} =
  Spec
    (FFIArrayCount $ foldMap (go "input" . snd) cgInputs)
    (FFIArrayCount $ foldMap (go "output" . snd) cgOutputs)
  where
    countOf k n = sbvKindToArrayCount k (+ n) mempty
    go iostr (SBV.CgAtomic (SBV.SV k (SBV.NodeId nodeId))) =
      error $
        "Scalar " <> iostr <> "s are incompatible with the KGen calling convention: '"
          <> show nodeId
          <> " :: "
          <> show k
          <> "'"
    go iostr (SBV.CgArray xs)
      | null xs = mempty
      | kindsMatch && kIsPrimitive = countOf k n
      | otherwise =
          error $
            iostr <> " arrays of kind(s) '" <> show kinds
              <> "' are incompatible with the KGen calling convention: '"
              <> show xs
              <> "'"
      where
        (headXs, tailXs) = case xs of
          (a : as) -> (a, as)
          _ -> error "WTF!  Got an empty 'CgArray'!"
        k = kindOf headXs
        n = length xs
        kindsMatch = all (\x -> kindOf x == k) tailXs
        kIsPrimitive = k `elem` cgPrimTypes
        kinds = nubOrd $ fmap kindOf xs

-- | Given a function name, this function will tell you the name (without extension) of the
-- specification file.
specFileName :: String -> String
specFileName funName = funName <> "_spec"

specFunName :: String -> String -> String
specFunName iostring funName = "get_" <> funName <> "_" <> iostring <> "_spec"

-- | Given a function name, this function will tell you the name of the input-specification function
-- in C.
inputSpecFunName :: String -> String
inputSpecFunName = specFunName "input"

-- | Given a function name, this function will tell you the name of the output-specification
-- function in C.
outputSpecFunName :: String -> String
outputSpecFunName = specFunName "output"

specSizeFunName :: String -> String -> String
specSizeFunName iostring funName = specFunName iostring funName <> "_size"

-- | Given a function name, this function will tell you the name of the input-specification-size
-- function in C.
inputSpecSizeFunName :: String -> String
inputSpecSizeFunName = specSizeFunName "input"

-- | Given a function name, this function will tell you the name of the output-specification-size
-- function in C.
outputSpecSizeFunName :: String -> String
outputSpecSizeFunName = specSizeFunName "output"

getCounts :: Num b => Arrays ArrayCount -> [b]
getCounts arrayCounts = fmap (\field -> view (field . to fromIntegral) arrayCounts) countLenses

-- | This function generates a C module and accompanying header that contain a hard-coded version of
-- the I/O specification for a function. This allows things like the plugin system and generated
-- bindings to make an extra runtime safety check before actually calling into a function compiled
-- from C and catch problems with a better message than @Segmentation fault (core dumped)@.
genFunSpec ::
  String ->
  SBVFFISpec ->
  (String, String)
genFunSpec funName funSpec = (cFile, hFile)
  where
    -- Unpack the function spec
    Spec
      (FFIArrayCount inputSpecArrays)
      (FFIArrayCount outputSpecArrays) = funSpec
    inputSpec = getCounts inputSpecArrays :: [Int]
    outputSpec = getCounts outputSpecArrays :: [Int]
    specSizeFun = flip specSizeFunName funName
    specFun = flip specFunName funName
    mkSpecSizeDecl iostring =
      "uint32_t " <> specSizeFun iostring <> "(void);\n"
    mkSpecDecl iostring =
      "uint32_t " <> specFun iostring <> "(uint32_t *spec);\n"
    mkSpecSizeDef iostring spec =
      unlines
        [ "uint32_t " <> specSizeFun iostring <> "(void) {",
          "  static const uint32_t spec_length = " <> show (length spec) <> ";",
          "  return spec_length;",
          "}",
          ""
        ]
    mkSpecDef iostring spec =
      unlines
        [ "uint32_t " <> specFun iostring <> "(uint32_t *spec) {",
          "  static const uint32_t specification[" <> show (length spec) <> "] = { "
            <> intercalate ", " (fmap show spec)
            <> " };",
          "  memcpy(spec, specification, sizeof(specification));",
          "  return " <> specSizeFun iostring <> "();",
          "}",
          ""
        ]
    cFile =
      unlines
        [ "#include <assert.h>  /* static_assert */",
          "#include \"" <> specFileName funName <> ".h\"",
          "",
          "#include \"" <> funName <> ".h\"",
          "",
          mkSpecSizeDef "input" inputSpec,
          mkSpecSizeDef "output" outputSpec,
          mkSpecDef "input" inputSpec,
          mkSpecDef "output" outputSpec
        ]
    hFile =
      headerHead
        <> unlines
          [ "#include <stdint.h>",
            "",
            mkSpecSizeDecl "input",
            mkSpecSizeDecl "output",
            mkSpecDecl "input",
            mkSpecDecl "output"
          ]
        <> headerTail
    onceName = "__" <> Casing.screamingSnake (specFileName funName) <> "_H__"
    headerHead =
      unlines
        [ "#ifndef " <> onceName,
          "#define " <> onceName,
          "",
          "#ifdef __cplusplus",
          "extern \"C\" {",
          "#endif  /* __cplusplus */",
          ""
        ]
    headerTail =
      unlines
        [ "",
          "#ifdef __cplusplus",
          "}",
          "#endif  /* __cplusplus */",
          "",
          "#endif  /* " <> onceName <> "*/"
        ]
