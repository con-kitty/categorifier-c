{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Typeclass with generic defaults for converting Haskell data to 'CxxType'.
module Kitty.CTypes.ToCxxType
  ( ToCxxType (..),
    ToCxxTypeError (..),
    toRecordNamedCxxType,
    toCType,
    CoercibleT,
  )
where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.Coerce (Coercible)
import Data.Either (partitionEithers)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Typeable (TypeRep, Typeable, typeRep)
import qualified Data.Vector as V
import Data.Word (Word8)
import GHC.Generics
  ( C1,
    Constructor,
    D1,
    Generic (..),
    Rec0,
    S1,
    Selector,
    U1,
    conName,
    selName,
    unK1,
    unM1,
    (:*:) (..),
    (:+:) (..),
  )
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Kitty.CTypes.KTypeRep (KTypeRep, toKTypeRep)
import Kitty.CTypes.Types
  ( CBitfield (..),
    CBitfieldPrim,
    CCon,
    CConF (..),
    CEnum (..),
    CStructF (..),
    CType,
    CTypeF (..),
    CUnionF (..),
    CxxCon (..),
    CxxOrCCon (..),
    CxxStruct (..),
    CxxType (..),
    CxxUnion (..),
    DcName (..),
    IsTuple (..),
    Prim (..),
    RfName,
    SupportsKBits,
    discard,
    makeRfName,
    toBitfieldPrim,
  )
import qualified Kitty.Common.IO.Exception as Exception
import Kitty.Duoidal (traverseD)
import Kitty.KTypes.KLiteral (kliteral)
import Kitty.Recursion (hembed)
import PyF (fmt)

-- | __TODO__: This should be returned from `toCxxType` instead of thrown in various places.
data ToCxxTypeError where
  BitfieldTooBig :: DcName -> Int -> ToCxxTypeError
  CxxTypeNotCType :: forall f. CxxType f -> ToCxxTypeError
  InconsistentCons :: forall f. KTypeRep -> NE.NonEmpty (CxxOrCCon f) -> ToCxxTypeError
  MixedSelectors ::
    forall f.
    DcName ->
    NE.NonEmpty (CxxType f) ->
    NE.NonEmpty (RfName, CxxType f) ->
    ToCxxTypeError
  NoElementSelected :: forall a. KTypeRep -> NE.NonEmpty a -> ToCxxTypeError
  NotAStruct :: forall f. CxxType f -> ToCxxTypeError
  NullaryRecord :: forall f. CCon f -> ToCxxTypeError
  RecordLengthMismatch :: forall a. TypeRep -> NE.NonEmpty a -> NE.NonEmpty T.Text -> ToCxxTypeError
  SingletonUnion :: KTypeRep -> ToCxxTypeError
  TooManyFields :: Int -> ToCxxTypeError

instance Show ToCxxTypeError where
  show = \case
    BitfieldTooBig (DcName name) n ->
      [fmt|Wow! Your bitfield ({name}) is way too big ({n} fields)!|]
    CxxTypeNotCType r ->
      [fmt|incompatible types! expecting CxxTypeCType but got {show $ discard r}|]
    InconsistentCons rep allCons ->
      "internal error: inconsistent cons in " <> show rep <> ": " <> show allCons'
      where
        allCons' :: NE.NonEmpty (CxxOrCCon Proxy)
        allCons' = fmap discard allCons
    MixedSelectors name _l _r ->
      [fmt|internal error: got a mix of record and non-record selectors for {show name}|]
    NoElementSelected rep allCons ->
      [fmt|\
internal error: sum type ({show rep}) should always have exactly 1 element but got zero
length allCons: {length allCons}
|]
    NotAStruct t -> "expected struct, got " <> show (discard t)
    NullaryRecord con ->
      "You goon! Why would you use toRecordNamedCxxType on a nullary con? "
        <> "("
        <> show (discard con)
        <> ")"
    RecordLengthMismatch rep fields recordNames ->
      intercalate
        "\n"
        [ "length mismatch in updating record names for " <> show rep,
          "expected " <> show (NE.length fields),
          "got " <> show (NE.length recordNames) <> "(" <> show recordNames <> ")"
        ]
    SingletonUnion rep -> "internal error: got union with one constructor (" <> show rep <> ")"
    TooManyFields _i ->
      [fmt|While creating CxxType, got a sum type with too many fields to represent as Word8|]

instance Exception.Exception ToCxxTypeError

-- | This constraint is unfortunately needed much further downstream.  We would like to be able to
-- use the `Kitty.CTypes.CGeneric.CGenerically` wrapper newtype to guide derivation of this class as
-- part of the transition from `CxxType` to `Kitty.CTypes.CGeneric.CGeneric`.  Unfortunately, that
-- wrapper interacts poorly with our parameter @g@ here, and we can't do anything about it at the
-- use site (in "Kitty.CTypes.CGeneric.CType") because @g@ can't be referred to from the class
-- constraint.  Here we use
-- [a trick due to Ryan Scott](https://ryanglscott.github.io/2018/03/04/how-quantifiedconstraints-can-let-us-put-join-back-in-monad/) [ ](DONTLINTLINELENGTH)
-- which enables us to get around this issue.
--
-- Finally, due to quantified constraints not being able to live in type synonyms, we define this as
-- a dummy class.
class (forall x y. Coercible x y => Coercible (g x) (g y)) => CoercibleT g

-- | This instance is @OVERLAPPABLE@ so that the `Identity` instance below can overlap it.  See
-- below.
instance {-# OVERLAPPABLE #-} (forall x y. Coercible x y => Coercible (g x) (g y)) => CoercibleT g

-- | This instance @OVERLAPS@ the general one solely so that the type checker doesn't see the
-- constraint we care about (the quantified class constraint) as trivially simplifiable (see
-- [@-Wsimplifiable-class-constraints@](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#ghc-flag--Wsimplifiable-class-constraints)). [ ](DONTLINTLINELENGTH)
--
-- We don't actually care which instance is selected in the case of `Identity` -- they are exactly
-- the same.
instance {-# OVERLAPS #-} CoercibleT Identity

-- TODO(greg): use functional dependencies to have @a@ imply @f@.
--
-- I believe that in order to do @| a -> f@ with -XFunctionalDependencies, you have to commit
-- to only writing 'ToCxxType' instances for higher-kinded types. As it is, I have instances
-- for non-higher-kinded types, for example:
--
-- > instance (Applicative f, SupportsKBits f) => ToCxxType f Int64 where
-- >   toCxxType = toCxxType . fmap (pure :: Int64 -> f Int64)
--
-- I did it the other way at first but that required refactoring quite a few types to be
-- higher-kinded. Everything in the netgraph must be an instance of 'ToCxxType' right now.
-- I have two WIP patches, one makes a bunch of types higher-kinded and the other makes
-- 'ToCxxType' optional for netgraph channels. When those go in, use the functional dependency:
-- >  class SupportsKBits f => ToCxxType f a | a -> f where
class SupportsKBits f => ToCxxType f a where
  toCxxType ::
    ( Applicative g,
      Traversable g,
      CoercibleT g
    ) =>
    g a ->
    CxxType (Compose g f)
  default toCxxType ::
    ( Applicative g,
      Traversable g,
      CoercibleT g,
      Generic a,
      Typeable a,
      GToCxxType (ClassifySUE (Rep a)) f (Rep a)
      -- TODO(greg): could we move the 'SupportsKBits' constraint here?
      -- > , SupportsKBits f
      -- It seems like it's more of an implementation detail right now.  Revisit
      -- this once 'Kitty.PolyVec.PolyVec' is completely replaced by 'CType'.
    ) =>
    g a ->
    CxxType (Compose g f)
  toCxxType = either Exception.impureThrow id . gtoCxxType
  {-# INLINEABLE toCxxType #-}

------------------ struct or union -------------------
data SUE = Struct | Union | Enum'

type family ClassifySUE (a :: Type -> Type) :: SUE where
  ClassifySUE (D1 d (C1 c s)) = 'Struct
  ClassifySUE (D1 d (cx :+: cy)) = ClassifyEU (cx :+: cy)
  ClassifySUE a =
    TypeError
      ( 'Text "This is from Greg and Matt, don't get stuck on it: "
          ':$$: 'Text "ClassifySUE got unexpected: "
          ':<>: 'ShowType a
      )

type family ClassifyEU (a :: Type -> Type) :: SUE where
  ClassifyEU (C1 c U1) = 'Enum'
  ClassifyEU (C1 c fields) = 'Union
  ClassifyEU (cx :+: cy) = MergeEOU (ClassifyEU cx) (ClassifyEU cy)
  ClassifyEU a =
    TypeError
      ( 'Text "This is from Greg and Matt, don't get stuck on it: "
          ':$$: 'Text "ClassifyEU got unexpected: "
          ':<>: 'ShowType a
      )

type family MergeEOU (a :: SUE) (b :: SUE) :: SUE where
  MergeEOU 'Enum' 'Enum' = 'Enum'
  MergeEOU 'Union b = 'Union
  MergeEOU a 'Union = 'Union
  MergeEOU a b =
    TypeError
      ( 'Text "This is from Greg and Matt, don't get stuck on it: "
          ':$$: 'Text "MergeEOU got unexpected: "
          ':<>: 'ShowType a
          ':<>: 'Text ", "
          ':<>: 'ShowType b
      )

-- Make a type rep, then call the generic class method to get a CxxType.
gtoCxxType ::
  forall f g a.
  ( Applicative g,
    Traversable g,
    CoercibleT g,
    Generic a,
    Typeable a,
    GToCxxType (ClassifySUE (Rep a)) f (Rep a)
  ) =>
  g a ->
  Either ToCxxTypeError (CxxType (Compose g f))
gtoCxxType x = gtoCxxTypeWithTypeRep (Proxy @(ClassifySUE (Rep a))) ktypeRep (fmap from x)
  where
    ktypeRep = toKTypeRep (typeRep (Proxy @a))

-- Make the CxxType
class SupportsKBits f => GToCxxType (classification :: SUE) f (a :: Type -> Type) where
  gtoCxxTypeWithTypeRep ::
    (Applicative g, Traversable g, CoercibleT g) =>
    Proxy classification ->
    KTypeRep ->
    g (a p) ->
    Either ToCxxTypeError (CxxType (Compose g f))

-- | Make a list of data constructors
class SupportsKBits f => GToCons f (a :: Type -> Type) where
  gtoCons ::
    (Applicative g, Traversable g, CoercibleT g) =>
    g (Maybe (a p)) ->
    Either
      ToCxxTypeError
      (NE.NonEmpty (CxxOrCCon (Compose Proxy f)), Maybe (Int, CxxOrCCon (Compose g f)))

-- make a list of record selectors
class GToSelectors f (a :: Type -> Type) where
  gtoSelectors ::
    (Applicative g, Traversable g, CoercibleT g) =>
    g (a p) ->
    [(Maybe T.Text, CxxType (Compose g f))]

-- make one data constructor
instance
  ( Constructor c,
    GToSelectors f s,
    SupportsKBits f
  ) =>
  GToCxxType 'Struct f (D1 d (C1 c s))
  where
  gtoCxxTypeWithTypeRep ::
    forall g p.
    (Applicative g, Traversable g, CoercibleT g) =>
    Proxy 'Struct ->
    KTypeRep ->
    g (D1 d (C1 c s) p) ->
    Either ToCxxTypeError (CxxType (Compose g f))
  gtoCxxTypeWithTypeRep _ rep x = toCOrCxxStruct rep <$> c
    where
      dcname = DcName $ T.pack (conName (undefined :: C1 c s p))
      g :: D1 d (C1 c s) p -> s p
      g = unM1 . unM1
      c :: Either ToCxxTypeError (CxxOrCCon (Compose g f))
      c = selectorsToCon dcname (gtoSelectors (fmap g x))
  {-# INLINEABLE gtoCxxTypeWithTypeRep #-}

toCOrCxxStruct :: KTypeRep -> CxxOrCCon f -> CxxType f
toCOrCxxStruct rep (CCon con) = CxxTypeCType (CTypeStruct (CStruct rep con))
toCOrCxxStruct rep (CxxCon con) = CxxTypeStruct (CxxStruct rep con)

-- | On failure, returns any constructors that weren't C constructors.
toCCons :: NE.NonEmpty (CxxOrCCon f) -> Either (NE.NonEmpty (CxxCon f)) (NE.NonEmpty (CCon f))
toCCons = traverseD $
  \case
    CxxCon r -> Left $ pure r
    CCon r -> pure r

l1ToMaybe :: (a :+: b) p -> Maybe (a p)
l1ToMaybe = \case
  L1 a -> Just a
  R1 _ -> Nothing

r1ToMaybe :: (a :+: b) p -> Maybe (b p)
r1ToMaybe = \case
  L1 _ -> Nothing
  R1 b -> Just b

-- | Make a list of enum data constructors
class GToEnumCons (a :: Type -> Type) where
  gtoEnumCons :: Traversable g => g (Maybe (a p)) -> (NE.NonEmpty DcName, Maybe Int)

instance (GToEnumCons cx, GToEnumCons cy) => GToEnumCons (cx :+: cy) where
  gtoEnumCons f = (xs <> ys, i1 <|> fmap (+ length xs) i2)
    where
      (xs, i1) = gtoEnumCons $ fmap (>>= l1ToMaybe) f
      (ys, i2) = gtoEnumCons $ fmap (>>= r1ToMaybe) f

instance Constructor c => GToEnumCons (C1 c U1) where
  gtoEnumCons x = (pure . DcName $ T.pack (conName (undefined :: C1 c U1 p)), 0 <$ sequenceA x)

-- Enums
instance
  ( GToEnumCons cx,
    GToEnumCons cy,
    SupportsKBits f
  ) =>
  GToCxxType 'Enum' f (D1 d (cx :+: cy))
  where
  -- gtoCxxTypeWithTypeRep ::
  --   forall g p.
  --   (Applicative g, Traversable g) =>
  --   Proxy 'Enum' ->
  --   KTypeRep ->
  --   g (D1 d (cx :+: cy) p) ->
  --   Either ToCxxTypeError (CxxType (Compose g f))
  gtoCxxTypeWithTypeRep _ rep x = do
    -- traverse the branches and get indexed type and value information
    let dcnames :: NE.NonEmpty DcName
        mconData :: Maybe Int
        (dcnames, mconData) = gtoEnumCons (Just . unM1 <$> x)
    conTag <-
      maybe
        (Left $ NoElementSelected rep dcnames)
        (\i -> maybe (Left $ TooManyFields i) pure $ convertToWord8 i)
        mconData
    pure . CxxTypeCType . CTypeEnum . CEnum rep (V.fromList $ NE.toList dcnames) . Compose . pure $
      kliteral conTag

-- Unions
instance (GToCons f cx, GToCons f cy) => GToCxxType 'Union f (D1 d (cx :+: cy)) where
  gtoCxxTypeWithTypeRep ::
    forall g p.
    (Applicative g, Traversable g, CoercibleT g) =>
    Proxy 'Union ->
    KTypeRep ->
    g (D1 d (cx :+: cy) p) ->
    Either ToCxxTypeError (CxxType (Compose g f))
  gtoCxxTypeWithTypeRep _ rep x = do
    -- traverse the branches and get indexed type and value information
    (allCons, mconData) <- gtoCons (Just . unM1 <$> x)
    (conTag, conData) <-
      maybe
        (Left $ NoElementSelected rep allCons)
        (\(i, c) -> maybe (Left $ TooManyFields i) (pure . (,c)) $ convertToWord8 i)
        mconData
    let conTag' :: g (f Word8)
        conTag' = pure (kliteral conTag :: f Word8)
    if null $ NE.tail allCons
      then Left $ SingletonUnion rep
      else -- convert the list of cons to a C or C++ union

        either
          -- not all C cons, must be a C++ union
          ( const $
              let allCons' :: V.Vector (CxxOrCCon Proxy)
                  allCons' = V.fromList . NE.toList $ fmap discard allCons
               in pure . CxxTypeUnion $ CxxUnion rep allCons' (Compose conTag') conData
          )
          -- all C Cons, it's a C union
          ( \ccons ->
              let unsafeToCCon ::
                    CxxOrCCon (Compose g f) -> Either ToCxxTypeError (CCon (Compose g f))
                  unsafeToCCon (CCon con) = pure con
                  unsafeToCCon (CxxCon _) = Left $ InconsistentCons rep allCons
                  ccons' :: V.Vector (CCon Proxy)
                  ccons' = V.fromList . NE.toList $ discard <$> ccons
               in CxxTypeCType . CTypeUnion . CUnion rep ccons' (Compose conTag')
                    <$> unsafeToCCon conData
          )
          $ toCCons allCons
  {-# INLINEABLE gtoCxxTypeWithTypeRep #-}

instance (GToCons f cx, GToCons f cy) => GToCons f (cx :+: cy) where
  gtoCons f = do
    (r1, i1) <- gtoCons $ fmap (>>= l1ToMaybe) f
    (r2, i2) <- gtoCons $ fmap (>>= r1ToMaybe) f
    pure (r1 <> r2, i1 <|> fmap (first (+ length r1)) i2)
  {-# INLINEABLE gtoCons #-}

instance (Constructor c, GToSelectors f s, SupportsKBits f) => GToCons f (C1 c s) where
  gtoCons x = do
    let dcname = DcName $ T.pack (conName (undefined :: C1 c s p))
    proxyCon <- fmap pure . selectorsToCon dcname $ gtoSelectors (Proxy :: Proxy (s p))
    selCon <- traverse (fmap (0,) . selectorsToCon dcname . gtoSelectors . fmap unM1) $ sequenceA x
    pure (proxyCon, selCon)
  {-# INLINEABLE gtoCons #-}

convertToWord8 :: Int -> Maybe Word8
convertToWord8 kInt
  | fromIntegral kWord8 /= kInt = Nothing
  | otherwise = pure kWord8
  where
    kWord8 = fromIntegral kInt

-- | C types are detected greedily, never simplified later.
toCType :: CxxType f -> Maybe (CType f)
toCType (CxxTypeCType r) = Just r
toCType _ = Nothing

-- Here is where we detect bitfields and C types
maybeToBitfieldCon ::
  forall f g.
  (Applicative g, SupportsKBits f) =>
  DcName ->
  NE.NonEmpty (RfName, CxxType (Compose g f)) ->
  Either Int (CxxOrCCon (Compose g f))
maybeToBitfieldCon dcname cxxFields =
  case sequenceA (traverse toCType <$> cxxFields) of
    Nothing -> pure . CxxCon $ CxxNormalCon dcname cxxFields
    Just cfields ->
      if null $ NE.tail cfields -- don't make a bitfield for a single `Bool`.
        then pure . CCon $ CNormalCon dcname NotTuple $ fmap (fmap hembed) cfields
        else
          CCon <$> case sequenceA (traverse toBool <$> cfields) of
            -- By the way, tuples are only made in Instances.hs, never by GHC.Generics.
            Nothing -> pure . CNormalCon dcname NotTuple $ fmap (fmap hembed) cfields
            Just (boolFields :: NE.NonEmpty (RfName, Compose g f Bool)) ->
              let bitfieldPrim :: Either Int (CBitfieldPrim (Compose g f))
                  bitfieldPrim = toBitfieldPrim boolFields
               in CBitfieldCon . CBitfield dcname (fst <$> boolFields) <$> bitfieldPrim
  where
    toBool :: CType (Compose g f) -> Maybe (Compose g f Bool)
    toBool (CTypePrim (PrimBool x)) = Just x
    toBool _ = Nothing

-- Default field names for non-record types.
defaultFieldNames :: NE.NonEmpty RfName
defaultFieldNames =
  let toFieldName :: Int -> RfName
      toFieldName k = makeRfName [fmt|unnamed_field_{k}|]
   in toFieldName 0 NE.:| fmap toFieldName [1 ..]

-- | Convert a constructor name and a list of selectors to a Con
selectorsToCon ::
  forall f g.
  (Applicative g, SupportsKBits f) =>
  DcName ->
  [(Maybe T.Text, CxxType (Compose g f))] ->
  Either ToCxxTypeError (CxxOrCCon (Compose g f))
selectorsToCon dcname selectors = case toNonEmpties . partitionEithers $ fmap f selectors of
  -- There are 0 fields.
  (Nothing, Nothing) -> pure . CCon $ CNullaryCon dcname
  -- All fields have names
  (Nothing, Just r) -> first (BitfieldTooBig dcname) $ maybeToBitfieldCon dcname r
  -- No fields have names.
  (Just r, Nothing) ->
    first (BitfieldTooBig dcname) $ maybeToBitfieldCon dcname (NE.zip defaultFieldNames r)
  -- (impossible) some fields have names, some don't throw an error
  (Just l, Just r) -> Left $ MixedSelectors dcname l r
  where
    toNonEmpties (x, y) = (NE.nonEmpty x, NE.nonEmpty y)
    f :: (Maybe T.Text, CxxType h) -> Either (CxxType h) (RfName, CxxType h)
    f (Nothing, r) = Left r
    f (Just recordName, r) = Right (makeRfName recordName, r)

-- product types
instance (GToSelectors f sx, GToSelectors f sy) => GToSelectors f (sx :*: sy) where
  gtoSelectors ::
    forall g p.
    (Applicative g, Traversable g, CoercibleT g) =>
    g ((sx :*: sy) p) ->
    [(Maybe T.Text, CxxType (Compose g f))]
  gtoSelectors xy = gtoSelectors (fx <$> xy) <> gtoSelectors (fy <$> xy)
    where
      fx (x :*: _) = x
      fy (_ :*: y) = y
  {-# INLINEABLE gtoSelectors #-}

instance GToSelectors f U1 where
  gtoSelectors = const []

-- Record field recursion
instance (Selector s, ToCxxType f a) => GToSelectors f (S1 s (Rec0 a)) where
  gtoSelectors ::
    forall g p.
    (Applicative g, Traversable g, CoercibleT g) =>
    g (S1 s (Rec0 a) p) ->
    [(Maybe T.Text, CxxType (Compose g f))]
  gtoSelectors x =
    let name = T.pack $ selName (undefined :: S1 s (Rec0 a) p)
        mname
          | T.null name = Nothing
          | otherwise = Just name
        g :: g (S1 s (Rec0 a) p) -> g a
        g = fmap (unK1 . unM1)
     in [(mname, toCxxType (g x))]
  {-# INLINEABLE gtoSelectors #-}

-- | Use the generics mechanism to generate a C struct but replace its field names
-- with a given list. Throws a runtime error if list length is incorrect.
toRecordNamedCxxType ::
  forall f g a.
  ( Applicative g,
    Traversable g,
    CoercibleT g,
    Generic a,
    Typeable a,
    GToCxxType (ClassifySUE (Rep a)) f (Rep a)
  ) =>
  NE.NonEmpty T.Text ->
  g a ->
  Either ToCxxTypeError (CxxType (Compose g f))
toRecordNamedCxxType recordNames = updateCons <=< gtoCxxType
  where
    updateCons :: CxxType (Compose g f) -> Either ToCxxTypeError (CxxType (Compose g f))
    updateCons (CxxTypeStruct (CxxStruct rep con)) =
      CxxTypeStruct . CxxStruct rep <$> updateCxxCon con
    updateCons (CxxTypeCType (CTypeStruct (CStruct rep con))) =
      CxxTypeCType . CTypeStruct . CStruct rep <$> updateCCon con
    updateCons t = Left $ NotAStruct t
    -- zip names with fields
    updateCxxCon :: CxxCon (Compose g f) -> Either ToCxxTypeError (CxxCon (Compose g f))
    updateCxxCon (CxxNormalCon name fields)
      | NE.length fields == NE.length recordNames =
          pure $ CxxNormalCon name (NE.zipWith replaceFieldName recordNames fields)
      | otherwise = Left $ RecordLengthMismatch (typeRep (Proxy @a)) fields recordNames
    replaceFieldName newName (_oldName, field) = (makeRfName newName, field)
    updateCCon :: CCon (Compose g f) -> Either ToCxxTypeError (CCon (Compose g f))
    updateCCon (CNormalCon name tup fields)
      | NE.length fields == NE.length recordNames =
          pure $ CNormalCon name tup (NE.zipWith replaceFieldName recordNames fields)
      | otherwise = Left $ RecordLengthMismatch (typeRep (Proxy @a)) fields recordNames
    updateCCon (CBitfieldCon (CBitfield name fields value))
      | NE.length fields == NE.length recordNames =
          pure $ CBitfieldCon $ CBitfield name (makeRfName <$> recordNames) value
      | otherwise = Left $ RecordLengthMismatch (typeRep (Proxy @a)) fields recordNames
    updateCCon con@(CNullaryCon _) = Left $ NullaryRecord con
{-# INLINEABLE toRecordNamedCxxType #-}
