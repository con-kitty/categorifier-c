-- | Central module which renders all the modules into C/C++/Python code.
module Categorifier.C.CTypes.Codegen.Run
  ( renderCTypesModules,
  )
where

import Categorifier.C.CTypes.ArrayLengths (Mismatch)
import Categorifier.C.CTypes.Codegen.Arrays (ToOrFromArrays (..), toArraysModule)
import Categorifier.C.CTypes.Codegen.CborSerialisation (DecodeOrEncode (..), toCborSerialisationModule)
import Categorifier.C.CTypes.Codegen.DataLayout (toDataLayoutModule)
import Categorifier.C.CTypes.Codegen.EnumUtils (toEnumUtilsModule)
import Categorifier.C.CTypes.Codegen.Helpers (CStructOrUnion (..), OrderedTypes (..), getOrderedTypes)
import Categorifier.C.CTypes.Codegen.IsEqual (toIsEqualModule)
import Categorifier.C.CTypes.Codegen.PrintDifferences (toPrintDifferencesModule)
import Categorifier.C.CTypes.Codegen.Render.Render
  ( RenderedFile (..),
    RenderedModule (..),
    renderModule,
  )
import Categorifier.C.CTypes.Codegen.ToString (toToStringModule)
import Categorifier.C.CTypes.Codegen.TypesHeaders (toCTypesModule, toCxxTypesModule)
import Categorifier.C.CTypes.DSL.CxxAst
  ( Comment (..),
    CxxModule (..),
    CxxTarget (..),
    Define (..),
  )
import Categorifier.C.CTypes.Render (renderCNat)
import Categorifier.C.CTypes.ToCxxType (ToCxxType)
import Categorifier.C.CTypes.Types
import Categorifier.C.KTypes.C (C, toCxxTypeViaC)
import Categorifier.C.Prim (Arrays)
import Control.Monad (join)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | This is used in @from_arrays.c@ to allow a recovery value for every non-finite
-- floating-point value that might occur in the function output.
--
-- For functions with this feature enabled, if the recovery argument (of
-- type identical to the output) is non-NULL and the arrays resulting from
-- calling the generated function contain non-finite (inf or NaN)
-- floating-point values, the corresponding values from the recovery
-- argument will be inserted into the output of the arrays conversion in
-- their place.
--
-- A further argument is added for keeping track of statistics - how many
-- of each floating-point type was inf and NaN in each conversion.
data NonFiniteStats = NonFiniteStats
  { nfsDoubleInf :: Word64,
    nfsDoubleNaN :: Word64,
    nfsFloatInf :: Word64,
    nfsFloatNaN :: Word64
  }
  deriving (Generic)

instance ToCxxType C NonFiniteStats

renderCTypesModules ::
  [CxxType Proxy] ->
  Either (NonEmpty (T.Text, Arrays Mismatch)) [RenderedFile]
renderCTypesModules cxxTypes =
  fmap join . traverse (\target -> catModuleFiles . renderModule target <$> toModule target) $
    enumFrom minBound
  where
    allCxxTypes = toCxxTypeViaC (Proxy @NonFiniteStats) : cxxTypes
    -- Sort all the types.
    OrderedTypes cnats cenums cstructsOrUnions cxxStructsOrUnions carrays =
      getOrderedTypes allCxxTypes
    -- This is a very bad mechanism because it includes many types
    -- in cenums/cstructsOrUnions/cxxStructsOrUnions/carrays.
    -- That means we can only use it in modules like cbor/is_equal which do their own
    -- topological sort. A better solution would be to have either all modules do a top sort
    -- through a common interface, or replace topLevelTypes with
    -- "otherTopLevelTypes" which are guaranteed to not overlap with
    -- the above mentioned OrderedTypes.
    -- TODO(greg): fix
    topLevelTypes = allCxxTypes
    -- List of modules to be called with @map toModule (enumFrom minBound)@ or similar.
    toModule :: CxxTarget -> Either (NonEmpty (T.Text, Arrays Mismatch)) CxxModule
    toModule = \case
      CTypes -> pure $ toCTypesModule cenums cstructsOrUnions
      CxxTypes -> pure $ toCxxTypesModule cxxStructsOrUnions
      DataLayout -> pure $ toDataLayoutModule cenums cstructsOrUnions
      CborDecode ->
        pure $
          toCborSerialisationModule Decode cenums cstructsOrUnions cxxStructsOrUnions topLevelTypes
      CborEncode ->
        pure $
          toCborSerialisationModule Encode cenums cstructsOrUnions cxxStructsOrUnions topLevelTypes
      EnumUtils ->
        let toCUnion CS {} = Nothing
            toCUnion (CU r) = Just r
         in pure . toEnumUtilsModule cenums $ mapMaybe toCUnion cstructsOrUnions
      IsEqual -> pure $ toIsEqualModule cenums cstructsOrUnions cxxStructsOrUnions topLevelTypes
      PrintDifferences ->
        pure $ toPrintDifferencesModule cenums cstructsOrUnions cxxStructsOrUnions topLevelTypes
      ToString -> pure $ toToStringModule cenums cstructsOrUnions cxxStructsOrUnions
      ToArrays -> toArraysModule ToArrays' cenums cstructsOrUnions carrays
      FromArrays -> toArraysModule FromArrays' cenums cstructsOrUnions carrays
      Dimensions -> pure $ toDimensionsModule cnats

    catModuleFiles :: RenderedModule -> [RenderedFile]
    catModuleFiles RenderedModule {rmHeader = header, rmSource = msource} = case msource of
      Nothing -> [header]
      Just source -> [header, source]

-- #define a bunch of dimensions.
toDimensionsModule :: [CNat] -> CxxModule
toDimensionsModule cnats =
  CxxModule
    { moduleIncludes = [],
      moduleDefines = mapMaybe toDimensionDefine cnats,
      moduleTypedefs = [],
      moduleUsingDecls = [],
      moduleFunctions = [],
      moduleTypeLevelFunctions = []
    }
  where
    toDimensionDefine (CNatInt _) = Nothing
    toDimensionDefine cnat@(CNatType n _) = Just $ Define (Just (Comment comment)) lhs rhs
      where
        comment = T.pack (show cnat)
        lhs = renderCNat cnat
        rhs = T.pack (show n)
