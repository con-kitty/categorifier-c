{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Kitty.Cat.Plugin.UnconCat
  ( kittyBoxers,
    tryAutoInterpret,
  )
where

import Control.Monad ((<=<), guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.RWS.Strict (RWST (..), gets, withRWST)
import Data.Either.Extra (isRight)
import Data.Foldable (foldlM)
import Data.Tuple.Extra (fst3)
import qualified ForeignCall as Plugins
import qualified GhcPlugins
import Kitty.Plugin.Core.MakerMap (splitNameString)
import Kitty.Plugin.Core.Makers (Makers (..))
import qualified Kitty.Plugin.Core.PrimOp as PrimOp
import Kitty.Plugin.Core.Types (AutoInterpreter, CategoryStack, CategoryState (..), Lookup)
import Kitty.Plugin.Hierarchy (findId, findTyCon)

tryAutoInterpret :: Lookup AutoInterpreter
tryAutoInterpret = do
  catId <- findId "Kitty.CExpr.Cat" "cat"
  cexprTyCon <- findTyCon "Kitty.CExpr.Types.Core" "CExpr"
  targetObTyCon <- findTyCon "Kitty.CExpr.Cat.TargetOb" "TargetOb"
  pure $ \buildDictionary cat nativeFunTy target args -> runMaybeT $ do
    let canAutoInterpret = case GhcPlugins.splitTyConApp_maybe cat of
          Just (tc, _) ->
            splitNameString (GhcPlugins.tyConName tc) == (Just "Kitty.CExpr.Cat", "Cat")
          Nothing -> False

        -- This is more accurate than `GhcPlugins.eqType`; the latter often fails to see through
        -- type synonyms or type families.
        eqTypeIO :: GhcPlugins.UniqSupply -> GhcPlugins.Type -> GhcPlugins.Type -> IO Bool
        eqTypeIO uniqS a b
          | GhcPlugins.eqType (GhcPlugins.typeKind a) (GhcPlugins.typeKind b) =
            let eqPred =
                  GhcPlugins.mkTyConApp
                    GhcPlugins.eqTyCon
                    [GhcPlugins.typeKind a, a, b]
             in isRight . fst3
                  <$> runRWST
                    (runExceptT (buildDictionary eqPred))
                    ()
                    (CategoryState uniqS 0 mempty)
          | otherwise = pure False
    guard canAutoInterpret
    -- Suppose the type of `target` is
    -- `forall (f :: * -> *) (a :: *). Pred f a => In f a -> Out f a`, and the args are
    -- `@F : @A : dict : termArgs`. Then we have
    --
    --   nativeFun = target @F @A dict
    --   nativeFunTy = In F A -> Out F A
    --   targetObFun =
    --     target @F @(TargetOb A) dict', where `dict'` is built by `buildDictionary'`.
    --   targetObFunTy = In F (TargetOb A) -> Out F (TargetOb A)
    --   targetObCFunTy = TargetOb nativeFunTy = TargetOb (In F A -> Out F A)
    --
    -- Now, if `targetObFunTy ~ targetObCFunTy`, then we coerce `targetObFun`'s type into
    -- `targetObCFunTy`, pass it to `cat`, and use the resulting expression,
    -- whose type is `Cat (In F A) (Out F A)`, as the interpretation of
    -- `target @F @A dict`.
    let isCTy arg
          | Just (tc, _) <- GhcPlugins.splitTyConApp_maybe arg =
            splitNameString (GhcPlugins.tyConName tc) == (Just "Kitty.KTypes.C", "C")
          | otherwise = False
        liftTyArg arg
          | GhcPlugins.tcIsLiftedTypeKind (GhcPlugins.typeKind arg) =
            GhcPlugins.mkTyConApp targetObTyCon [GhcPlugins.liftedTypeKind, arg]
          | isCTy arg = GhcPlugins.mkTyConTy cexprTyCon
          | otherwise = arg
        buildDictionary' ::
          GhcPlugins.CoreExpr ->
          GhcPlugins.Type ->
          MaybeT CategoryStack GhcPlugins.CoreExpr
        buildDictionary' predArg ty
          | GhcPlugins.eqType (GhcPlugins.exprType predArg) ty = pure predArg
          | otherwise =
            MaybeT . ExceptT . withRWST (const ((),)) $
              runExceptT (buildDictionary ty) >>= \case
                Right res -> pure (Right (Just res))
                Left _ -> pure (Right Nothing)
    targetObFun <-
      foldlM
        ( \acc -> \case
            GhcPlugins.Type tyArg ->
              pure $ GhcPlugins.mkCoreApps acc [GhcPlugins.Type (liftTyArg tyArg)]
            predArg ->
              fmap (GhcPlugins.mkCoreApps acc . pure) . buildDictionary' predArg
                <=< MaybeT . pure . fmap fst . GhcPlugins.splitFunTy_maybe
                $ GhcPlugins.exprType acc
        )
        (GhcPlugins.Var target)
        args
    let targetObFunTy = GhcPlugins.exprType targetObFun
        targetObCFunTy =
          GhcPlugins.mkTyConApp targetObTyCon [GhcPlugins.liftedTypeKind, nativeFunTy]
    uniqS <- lift . lift $ gets csUniqSupply
    guard =<< GhcPlugins.liftIO (eqTypeIO uniqS targetObFunTy targetObCFunTy)
    (argTy, resTy) <- MaybeT . pure $ GhcPlugins.splitFunTy_maybe nativeFunTy
    let co = GhcPlugins.mkUnsafeCo GhcPlugins.Representational targetObFunTy targetObCFunTy
    pure $
      GhcPlugins.mkCoreApps
        (GhcPlugins.Var catId)
        [GhcPlugins.Type argTy, GhcPlugins.Type resTy, GhcPlugins.mkCast targetObFun co]

kittyBoxers ::
  Makers -> [(Plugins.CLabelString, (PrimOp.Boxer, [GhcPlugins.Type], GhcPlugins.Type))]
kittyBoxers makers =
  [ ( "fmod",
      ( PrimOp.Boxer
          "Kitty.KTypes.FMod"
          "fmod"
          [GhcPlugins.doubleDataCon, GhcPlugins.doubleDataCon]
          $ mkFmod makers GhcPlugins.doubleTy,
        [GhcPlugins.doubleTy, GhcPlugins.doubleTy],
        GhcPlugins.doubleTy
      )
    ),
    ( "fmodf",
      ( PrimOp.Boxer "Kitty.KTypes.FMod" "fmod" [GhcPlugins.floatDataCon, GhcPlugins.floatDataCon] $
          mkFmod makers GhcPlugins.floatTy,
        [GhcPlugins.floatTy, GhcPlugins.floatTy],
        GhcPlugins.floatTy
      )
    )
  ]
