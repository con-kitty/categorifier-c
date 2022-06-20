{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This is separate from "Categorifier.C.Client" because we can't define Template Haskell and use it in
--   the same module. This module provides `deriveHasRep`, but it's private because importing it
--   would mean you miss the instances.
module Categorifier.C.Client.Internal
  ( deriveHasRep,
  )
where

import Categorifier.C.KTypes.C (C)
import Categorifier.C.PolyVec (PolyVec, zeroValue)
import Categorifier.Client (HasRep (..), Rep)
import qualified Categorifier.Common.IO.Exception as Exception
import Categorifier.TH (TyVarBndr, tyVarBndrName, pattern KindedTV, pattern PlainTV)
import Control.Monad ((<=<))
import Control.Monad.Trans.State.Strict (evalState, get, modify)
import Data.Bifunctor (first, second)
import Data.Bitraversable (bitraverse)
import Data.Constraint (Dict (..))
import Data.Foldable (foldl', toList)
import Data.List.Extra (groupSort)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Tuple.Extra (fst3, snd3, thd3)
import GHC.Word (Word8 (..))
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

-- so we can use @MagicHash@ for the `W8#` constructor.
{-# ANN module "HLint: ignore Avoid restricted extensions" #-}

data DeriveCallFailure
  = GadtMissingName TH.Type
  | InvalidName TH.Info
  | MisquotedName

-- | Duplicates `Categorifier.Client.deriveHasRep`, except that it represents sum types as a tagged
--   product.
deriveHasRep :: TH.Name -> TH.DecsQ
deriveHasRep name =
  either (Exception.throwIOAsException explainDeriveCallFailure) sequenceA . deriveHasRep'
    =<< TH.reify name
  where
    explainDeriveCallFailure = \case
      GadtMissingName ty ->
        "a GADT constructor with type "
          <> show ty
          <> " has no constructor name. This should be impossible."
      InvalidName info -> "expected type constructor for " <> show name <> " but got " <> show info
      MisquotedName ->
        "expected type constructor for "
          <> show name
          <> " but got Data Constructor. Did you only put one apostrophe on your thingy?"

deriveHasRep' :: TH.Info -> Either DeriveCallFailure [TH.DecQ]
deriveHasRep' = \case
  TH.DataConI {} -> Left MisquotedName
  TH.TyConI (TH.DataD ctx name tyVarBndrs _ dataCons _) -> hasRep name tyVarBndrs ctx dataCons
  TH.TyConI (TH.NewtypeD ctx name tyVarBndrs _ dataCon _) ->
    hasRep name tyVarBndrs ctx $ pure dataCon
  info -> Left $ InvalidName info
  where
    applyType name = foldl' TH.AppT (TH.ConT name) . fmap (TH.VarT . tyVarBndrName)

    nameOfProperBinder (PlainTV n) = Just n
    nameOfProperBinder (KindedTV n TH.StarT) = Just n
    nameOfProperBinder (KindedTV _ _) = Nothing

    hasRep :: TH.Name -> [TyVarBndr ()] -> TH.Cxt -> [TH.Con] -> Either DeriveCallFailure [TH.DecQ]
    hasRep typ vars ctx =
      fmap (uncurry (sums vars) <=< groupSort) . traverse (processCon (applyType typ vars) ctx)

    processCon ::
      TH.Type ->
      TH.Cxt ->
      TH.Con ->
      Either DeriveCallFailure (TH.Type, (TH.TypeQ, (TH.PatQ, TH.ExpQ), (TH.PatQ, TH.ExpQ)))
    processCon type0 ctx = \case
      TH.ForallC _ ctx' con -> processCon type0 (ctx <> ctx') con
      TH.GadtC names fieldTypes type1 ->
        maybe
          (Left $ GadtMissingName type1)
          (\conName -> pure (type1, hasRep' conName ctx $ fmap snd fieldTypes))
          $ listToMaybe names
      TH.InfixC fstField conName sndField ->
        pure (type0, hasRep' conName ctx $ fmap snd [fstField, sndField])
      TH.NormalC conName fieldTypes -> pure (type0, hasRep' conName ctx $ fmap snd fieldTypes)
      TH.RecC conName fieldTypes -> pure (type0, hasRep' conName ctx $ fmap thd3 fieldTypes)
      TH.RecGadtC names fieldTypes type1 ->
        maybe
          (Left $ GadtMissingName type1)
          (\conName -> pure (type1, hasRep' conName ctx $ fmap thd3 fieldTypes))
          $ listToMaybe names

    sums ::
      [TyVarBndr ()] -> TH.Type -> [(TH.TypeQ, (TH.PatQ, TH.ExpQ), (TH.PatQ, TH.ExpQ))] -> [TH.DecQ]
    sums vars type0 cons =
      maybe
        ( ( \(t, a, r) ->
              [ repInstD (pure type0) t,
                hasRepInstD
                  (pure type0)
                  []
                  (reverse (evalState (buildClauses pure pure $ pure a) (0 :: Word8)))
                  (evalState (buildClauses pure pure $ pure r) (0 :: Word8))
              ]
          )
            $ head cons
        )
        ( \size ->
            ( \(t, a, r) ->
                [ repInstD (pure type0) t,
                  hasRepInstD (pure type0) vars (reverse (evalState a 0)) (evalState r (0 :: Word8))
                ]
            )
              ( [t|
                  ($size, $(mkNestedPairs (\x y -> [t|($x, $y)|]) [t|()|] =<< traverse fst3 cons))
                  |],
                buildClauses
                  ( traverse $ \p -> do
                      i <- get
                      modify succ
                      pure $
                        if i == 0
                          then -- the index match isn't exhaustive, so we always use the 0th value
                          --      when it's out of bounds.
                            [p|(_, $p)|]
                          else [p|(W8# $(pure . TH.LitP $ TH.WordPrimL i), $p)|]
                  )
                  pure
                  . uncurry zip
                  . first
                    ( fmap (mkNestedPairs (\x y -> [p|($x, $y)|]) [p|()|] <=< sequenceA)
                        . diagonalize [p|_|]
                    )
                  . unzip
                  $ fmap snd3 cons,
                buildClauses
                  pure
                  ( traverse $ \e -> do
                      i <- get
                      modify succ
                      pure [|($(TH.lift i), $e)|]
                  )
                  . uncurry zip
                  . second
                    ( fmap (mkNestedPairs (\x y -> [|($x, $y)|]) [|()|] <=< sequenceA)
                        . diagonalize [|zeroValue (Proxy @C)|]
                    )
                  . unzip
                  $ fmap thd3 cons
              )
        )
        . sumSelector
        $ length cons

    diagonalize :: a -> [a] -> [[a]]
    diagonalize def = \case
      [] -> []
      (x : xs) ->
        let dtail = diagonalize def xs
         in (x : replicate (length dtail) def) : fmap (def :) dtail

    sumSelector :: Int -> Maybe TH.TypeQ
    sumSelector count = if count <= 1 then Nothing else Just [t|Word8|]

    buildClauses ::
      Applicative m =>
      ([TH.PatQ] -> m [TH.PatQ]) ->
      ([TH.ExpQ] -> m [TH.ExpQ]) ->
      [(TH.PatQ, TH.ExpQ)] ->
      m [TH.ClauseQ]
    buildClauses patFn expFn =
      fmap (fmap (flip (uncurry TH.clause) []) . uncurry zip)
        . bitraverse (fmap (fmap pure) . patFn) (fmap (fmap (fmap TH.NormalB)) . expFn)
        . unzip

    repInstD :: TH.TypeQ -> TH.TypeQ -> TH.DecQ
    repInstD type0 = TH.tySynInstD . TH.tySynEqn Nothing [t|Rep $type0|]

    hasRepInstD :: TH.TypeQ -> [TyVarBndr ()] -> [TH.ClauseQ] -> [TH.ClauseQ] -> TH.DecQ
    hasRepInstD type0 vars abstClauses reprClauses =
      TH.instanceD
        ( sequenceA $
            mapMaybe (fmap (\var -> [t|PolyVec C $(pure $ TH.VarT var)|]) . nameOfProperBinder) vars
        )
        [t|HasRep $type0|]
        [ TH.funD 'abst abstClauses,
          TH.pragInlD 'abst TH.Inline TH.FunLike TH.AllPhases,
          TH.funD 'repr reprClauses,
          TH.pragInlD 'repr TH.Inline TH.FunLike TH.AllPhases
        ]

    hasRep' :: TH.Name -> TH.Cxt -> [TH.Type] -> (TH.TypeQ, (TH.PatQ, TH.ExpQ), (TH.PatQ, TH.ExpQ))
    hasRep' conName predTypes fieldTypes =
      let vars = take (length fieldTypes) varSupply
          dicts = replicate (length predTypes) 'Dict
       in ( mkNestedPairs (\x y -> [t|($x, $y)|]) [t|()|] $
              fmap (TH.AppT $ TH.ConT ''Dict) predTypes <> fieldTypes,
            ( mkNestedPairs (\x y -> [p|($x, $y)|]) [p|()|] $
                fmap (`TH.ConP` []) dicts <> fmap TH.VarP vars,
              pure $ foldl' (\e -> TH.AppE e . TH.VarE) (TH.ConE conName) vars
            ),
            ( pure $ TH.ConP conName . fmap TH.VarP $ toList vars,
              mkNestedPairs (\x y -> [|($x, $y)|]) [|()|] $ fmap TH.ConE dicts <> fmap TH.VarE vars
            )
          )

    mkNestedPairs :: forall a. (TH.Q a -> TH.Q a -> TH.Q a) -> TH.Q a -> [a] -> TH.Q a
    mkNestedPairs mkPair unit = \case
      [] -> unit
      [x] -> pure x
      xs ->
        let (ys, zs) = splitAt (length xs `div` 2) xs
         in mkPair (mkNestedPairs mkPair unit ys) (mkNestedPairs mkPair unit zs)

    varSupply :: [TH.Name]
    varSupply = TH.mkName <$> ["hasrep_" <> i | i <- show <$> [0 :: Int ..]]
