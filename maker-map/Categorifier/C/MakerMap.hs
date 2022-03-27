{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Categorifier.C.MakerMap (cMakerMapFun) where

import qualified Categorifier.C.KTypes.ArcTan2
import qualified Categorifier.C.KTypes.FMod
import qualified Categorifier.C.KTypes.Function
import qualified Categorifier.C.KTypes.KLiteral
import qualified Categorifier.C.KTypes.Round
import qualified Categorifier.Common.IO.Exception
import Categorifier.Core.Categorify
  ( applyTyAndPredArgs,
    isTypeOrPred,
    simplifyFun,
  )
import Categorifier.Core.MakerMap
  ( MakerMapFun,
    applyEnrichedCat',
    makeMaker1,
    makeMaker2,
    makeTupleTyWithVar,
    splitNameString,
  )
import Categorifier.Core.Makers (Makers (..), isFreeIn)
import Categorifier.Core.Simplify (Transformation (..))
import Categorifier.Core.Types (CategoricalFailure (..))
import Categorifier.Duoidal (joinD, (<*\>), (=<\<))
import Control.Arrow ((&&&))
import Control.Monad.Trans.Except (throwE)
import qualified Data.Map as Map
import qualified GhcPlugins as Plugins
import qualified Language.Haskell.TH as TH
import qualified TyCoRep

cMakerMapFun :: MakerMapFun
cMakerMapFun
  dflags
  m@Makers {..}
  n
  target
  expr
  cat
  var
  args
  modu
  categorizeFun
  categorizeLambda =
    Map.fromListWith
      const
      [ -- __TODO__: This case is specific to the C backend. Because we don't seem to be
        --           able to grab `throw` and the underlying `raise#` doesn't give us
        --           an `Exception` type that we can `HasRep`, we use `impureThrow`,
        --           which lets us get a hold of the exception early enough. It can
        --           hopefully be replaced with one for `GHC.Exception.throw` once we
        --           have a source plugin.
        ( 'Categorifier.Common.IO.Exception.impureThrow,
          \case
            Plugins.Type a : Plugins.Type b : _except : rest ->
              pure $ maker1 rest =<\< mkBottom a b
            _ -> Nothing
        ),
        ( 'Categorifier.C.KTypes.ArcTan2.arctan2,
          \case
            Plugins.Type ty : _arctan2 : rest -> pure $ maker2 rest =<\< mkArcTan2 ty
            _ -> Nothing
        ),
        ( 'Categorifier.C.KTypes.FMod.fmod,
          \case
            Plugins.Type ty : _fmod : rest -> pure $ maker2 rest =<\< mkFmod ty
            _ -> Nothing
        ),
        -- __NB__: This currently requires that the provided function name is a String
        --         literal.
        ( 'Categorifier.C.KTypes.Function.kFunctionCall,
          \case
            Plugins.Type _f
              : Plugins.Type _isFunCall
              : Plugins.Type a
              : Plugins.Type x
              : _kFunCall
              : _kVariadicDevecAndApply
              : _kVariadicVectorizeFunctionInputs
              : _IsFunCall
              : _proxy
              : name
              : fn
              : rest ->
                pure $
                  if n `isFreeIn` fn
                    then
                      joinD $
                        applyEnriched' [fn] rest
                          <$> mkIndirection (nameTuple a) x name
                          <*\> mkId (nameTuple a)
                    else
                      maker1 rest
                        =<\< ( Plugins.App
                                 <$> mkIndirection a x name <*\> categorizeFun fn
                             )
            _ -> Nothing
        ),
        -- __TODO__: This requires disabling core lint, because it complaints that
        -- `KFFCall C (Foo C)` doesn't match `Foo C`. We can coerce
        -- the former into the latter (via `mkUnsafeCo`) to appease core lint, but
        -- that would require `rest` to be non-empty (because we are coercing the first
        -- element), i.e., it would no longer be partially applicable.
        ( 'Categorifier.C.KTypes.Function.kForeignFunctionCall,
          \case
            Plugins.Type _f
              : Plugins.Type _isFunCall
              : Plugins.Type a
              : Plugins.Type b
              : _kForeignFunctionCall
              : _kVariadicVectorizeFunctionInputs
              : _IsFunCall
              : _proxy
              : name
              : callee
              : fn
              : rest ->
                let i =
                      Plugins.mkCoreConApps
                        (Plugins.tupleDataCon Plugins.Boxed 2)
                        [ Plugins.Type (Plugins.exprType name),
                          Plugins.Type (Plugins.exprType callee),
                          name,
                          callee
                        ]
                 in pure $ maker1 rest =<\< mkFfcall (Plugins.exprType i) a b i fn
            _ -> Nothing
        ),
        ( -- `kliteral` for some reason is never specialized and thus can't be inlined
          -- in `mkInline`, so we need to rely on `simplifyFun`.
          -- For example, after simplifying `\x -> kliteral dict a` we expect
          -- to get `((\v -> v) `cast` <Co:5>) a`. However, sometimes `simplifyFun`
          -- somehow fails to simplify it. In that case, we simply categorize
          -- `\x -> a`, then coerce the result type.
          'Categorifier.C.KTypes.KLiteral.kliteral,
          \case
            [Plugins.Type _f, Plugins.Type _a, _kliteral, a] ->
              pure $
                simplifyFun dflags [Rules] expr >>= \case
                  -- `kliteral` failed to be simplified: after simplification, the head
                  -- is still `kliteral`.
                  Plugins.App (Plugins.collectArgs -> (Plugins.Var ident', _)) _
                    | eqNames
                        (Plugins.varName ident')
                        'Categorifier.C.KTypes.KLiteral.kliteral -> do
                        res <- categorizeLambda a
                        -- Coerce the result type. For example, `cat X Int32` is
                        -- coerced into `cat X (C Int32)`. This is only safe if
                        -- `cat X Int32` and `cat X (C Int32)` have the same representation,
                        -- which is the case with both `cat ~ C.Cat` and `cat ~ Hask`.
                        let expectedTy =
                              Plugins.mkAppTys
                                cat
                                [Plugins.varType n, Plugins.exprType expr]
                            actualTy =
                              Plugins.mkAppTys
                                cat
                                [Plugins.varType n, Plugins.exprType a]
                            co =
                              Plugins.mkUnsafeCo
                                Plugins.Representational
                                actualTy
                                expectedTy
                        pure $ Plugins.mkCast res co
                  -- `kliteral` is successfully simplified. Proceed with
                  -- `categorizeLambda`.
                  simplified -> categorizeLambda simplified
            _ -> Nothing
        ),
        ('Categorifier.C.KTypes.Round.kRoundDouble, skipCategorization)
      ]
    where
      mkNative' = do
        let tagTy =
              TyCoRep.LitTy . TyCoRep.StrTyLit . Plugins.mkFastString $
                modu <> "." <> var
            f = fst $ applyTyAndPredArgs Plugins.Var (Plugins.Var target) args
        (argTy, resTy) <-
          maybe (throwE . pure $ NotFunTy f (Plugins.exprType f)) pure $
            Plugins.splitFunTy_maybe (Plugins.exprType f)
        maker1 (dropWhile isTypeOrPred args) =<\< mkNative tagTy argTy resTy

      maker1 = makeMaker1 m categorizeLambda
      maker2 = makeMaker2 m categorizeLambda expr
      applyEnriched' = applyEnrichedCat' m categorizeLambda
      skipCategorization _ = pure mkNative'
      nameTuple = makeTupleTyWithVar n

eqNames :: Plugins.Name -> TH.Name -> Bool
eqNames name name' = splitNameString name == (TH.nameModule &&& TH.nameBase) name'
