{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Categorifier.C.UnconCat
  ( boxers,
    tryAutoInterpret,
  )
where

import Categorifier.Core.MakerMap (splitNameString)
import Categorifier.Core.Makers (Makers (..))
import qualified Categorifier.Core.PrimOp as PrimOp
import Categorifier.Core.Types (AutoInterpreter, CategoryStack, CategoryState (..), Lookup)
import qualified Categorifier.GHC.Builtin as Plugins
import qualified Categorifier.GHC.Core as Plugins
import qualified Categorifier.GHC.Types as Plugins
import Categorifier.Hierarchy (findId, findTyCon)
import Control.Monad (guard, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.RWS.Strict (RWST (..), gets, withRWST)
import Data.Either.Extra (isRight)
import Data.Foldable (foldlM)
import Data.Tuple.Extra (fst3)

tryAutoInterpret :: Lookup AutoInterpreter
tryAutoInterpret = do
  catId <- findId "Categorifier.C.CExpr.Cat" "cat"
  cexprTyCon <- findTyCon "Categorifier.C.CExpr.Types.Core" "CExpr"
  targetObTyCon <- findTyCon "Categorifier.C.CExpr.Cat.TargetOb" "TargetOb"
  pure $ \buildDictionary cat nativeFunTy target args -> runMaybeT $ do
    let canAutoInterpret = case Plugins.splitTyConApp_maybe cat of
          Just (tc, _) ->
            splitNameString (Plugins.tyConName tc) == (Just "Categorifier.C.CExpr.Cat", "Cat")
          Nothing -> False

        -- This is more accurate than `Plugins.eqType`; the latter often fails to see through
        -- type synonyms or type families.
        eqTypeIO :: Plugins.UniqSupply -> Plugins.Type -> Plugins.Type -> IO Bool
        eqTypeIO uniqS a b
          | Plugins.eqType (Plugins.typeKind a) (Plugins.typeKind b) =
              let eqPred =
                    Plugins.mkTyConApp
                      Plugins.eqTyCon
                      [Plugins.typeKind a, a, b]
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
          | Just (tc, _) <- Plugins.splitTyConApp_maybe arg =
              splitNameString (Plugins.tyConName tc) == (Just "Categorifier.C.KTypes.C", "C")
          | otherwise = False
        liftTyArg arg
          | Plugins.tcIsLiftedTypeKind (Plugins.typeKind arg) =
              Plugins.mkTyConApp targetObTyCon [Plugins.liftedTypeKind, arg]
          | isCTy arg = Plugins.mkTyConTy cexprTyCon
          | otherwise = arg
        buildDictionary' ::
          Plugins.CoreExpr ->
          Plugins.Type ->
          MaybeT CategoryStack Plugins.CoreExpr
        buildDictionary' predArg ty
          | Plugins.eqType (Plugins.exprType predArg) ty = pure predArg
          | otherwise =
              MaybeT . ExceptT . withRWST (const ((),)) $
                runExceptT (buildDictionary ty) >>= \case
                  Right res -> pure (Right (Just res))
                  Left _ -> pure (Right Nothing)
    targetObFun <-
      foldlM
        ( \acc -> \case
            Plugins.Type tyArg ->
              pure $ Plugins.mkCoreApps acc [Plugins.Type (liftTyArg tyArg)]
            predArg ->
              fmap (Plugins.mkCoreApps acc . pure) . buildDictionary' predArg
                <=< MaybeT . pure . fmap fst . Plugins.splitFunTy_maybe
                $ Plugins.exprType acc
        )
        (Plugins.Var target)
        args
    let targetObFunTy = Plugins.exprType targetObFun
        targetObCFunTy =
          Plugins.mkTyConApp targetObTyCon [Plugins.liftedTypeKind, nativeFunTy]
    uniqS <- lift . lift $ gets csUniqSupply
    guard =<< liftIO (eqTypeIO uniqS targetObFunTy targetObCFunTy)
    (argTy, resTy) <- MaybeT . pure $ Plugins.splitFunTy_maybe nativeFunTy
    let co = Plugins.mkPluginCo Plugins.Representational targetObFunTy targetObCFunTy
    pure $
      Plugins.mkCoreApps
        (Plugins.Var catId)
        [Plugins.Type argTy, Plugins.Type resTy, Plugins.mkCast targetObFun co]

boxers ::
  Makers -> [(Plugins.CLabelString, (PrimOp.Boxer, [Plugins.Type], Plugins.Type))]
boxers makers =
  [ ( "fmod",
      ( PrimOp.Boxer
          "Categorifier.KTypes.FMod"
          "fmod"
          [Plugins.doubleDataCon, Plugins.doubleDataCon]
          $ mkFmod makers Plugins.doubleTy,
        [Plugins.doubleTy, Plugins.doubleTy],
        Plugins.doubleTy
      )
    ),
    ( "fmodf",
      ( PrimOp.Boxer "Categorifier.C.KTypes.FMod" "fmod" [Plugins.floatDataCon, Plugins.floatDataCon] $
          mkFmod makers Plugins.floatTy,
        [Plugins.floatTy, Plugins.floatTy],
        Plugins.floatTy
      )
    )
  ]
