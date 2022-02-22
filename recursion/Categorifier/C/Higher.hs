{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | Higher-kinded functor interfaces.
module Categorifier.C.Higher
  ( (:~>),
    HFunctorC (..),
    HFoldableC (..),
    HTraversableC (..),

    -- * Default operations
    hmapCDefault,
    hfoldMapCDefault,
  )
where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint, Type)
import Prelude (Applicative, Monoid, (.))

type f :~> g = forall a. f a -> g a

infixr 0 :~>

-- | This is similar to what "Data.Functor.Transformer" provides. However, it is tricky to
-- implement 'Barbies.ConstraintsT' instance for 'Categorifier.C.CExpr.Types.Core.CExprF'. Because
-- the @AllT@ type would be:
--
-- @
--    type AllT c CExprF =
--      ( c Bool, c Int8, c Int16, c Int32, c Int64, c Word8, c Word16, c Word32, c Word64
--      , c Float, c Double, c CExprFunctionCall, c CExprSelectExpression
--      )
-- @
--
-- But GHC is unable to deduce @c x@ from @AllT c CExprF@ and @PrimIntegral x@.
class HFunctorC (c :: Type -> Constraint) h where
  hmapC :: c b => (forall a. c a => f a -> g a) -> h f b -> h g b

class HFoldableC (c :: Type -> Constraint) h where
  hfoldMapC ::
    (c b, Monoid m) =>
    (forall a. c a => f a -> m) ->
    h f b ->
    m

class (HFunctorC c h, HFoldableC c h) => HTraversableC c h where
  htraverseC ::
    (Applicative m, c b) =>
    -- This constraint lets us update the maps corresponding to the appropriate primitive
    -- type as we go through the expression.
    (forall a. c a => f a -> m (g a)) ->
    h f b ->
    m (h g b)

hmapCDefault ::
  forall c h f g b.
  (HTraversableC c h, c b) =>
  (forall a. c a => f a -> g a) ->
  h f b ->
  h g b
hmapCDefault f = runIdentity . htraverseC @c go
  where
    go :: forall a. c a => f a -> Identity (g a)
    go = Identity . f

hfoldMapCDefault ::
  forall c h f m b.
  (HTraversableC c h, Monoid m, c b) =>
  (forall a. c a => f a -> m) ->
  h f b ->
  m
hfoldMapCDefault f = getConst . htraverseC @c go
  where
    go :: forall a x. c a => f a -> Const m x
    go = Const . f
