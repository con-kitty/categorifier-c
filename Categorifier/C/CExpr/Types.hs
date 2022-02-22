{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Categorifier.C.CExpr.Types
  ( CExprTypeProduct (..),
    CExprTypeLens (..),
    CExprFunctionCall,
    CExprSelectExpression,
    CExprType (..),
    module X,
  )
where

import qualified Barbies
import Categorifier.C.CExpr.Types.Operations as X
import Categorifier.C.Prim (Arrays, PrimLens (..), PrimType)
import Control.Lens (Lens')
import Data.Functor.Classes (Eq1 (..))
import Data.Hashable (Hashable (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)

-- | The type of an internal function call output -- an aggregate of primitive types.
data CExprFunctionCall deriving (Show)

instance Hashable CExprFunctionCall where
  hashWithSalt = const

-- | The return type of a select (switch-case) operation -- an aggregate of primitive types.
data CExprSelectExpression deriving (Show)

instance Hashable CExprSelectExpression where
  hashWithSalt = const

-- | A value-level sum of the possible types a @CExpr@ expression may have.
data CExprType
  = CExprFunctionCallType
  | CExprSelectExpressionType
  | CExprPrimType PrimType
  deriving (Eq, Ord, Show, Read, Generic)

-- | A product of functors applied to all @CExpr@ base types (either aggregate or primitive).  This
-- is akin to the 'Arrays' type, which is a product of functors applied to all @CExpr@ primitive
-- types, but it allows us to work with function calls and select statements as well.
data CExprTypeProduct f = CExprTypeProduct
  { cexprTypeProductPrimitives :: !(Arrays f),
    cexprTypeProductFunction :: !(f CExprFunctionCall),
    cexprTypeProductSelect :: !(f CExprSelectExpression)
  }
  deriving (Generic)

deriving instance
  (Eq (f CExprFunctionCall), Eq (f CExprSelectExpression), Eq1 f) =>
  Eq (CExprTypeProduct f)

instance Barbies.FunctorB CExprTypeProduct

instance Barbies.TraversableB CExprTypeProduct

instance Barbies.ApplicativeB CExprTypeProduct

instance Barbies.DistributiveB CExprTypeProduct

instance Barbies.ConstraintsB CExprTypeProduct

instance (forall a. Semigroup (v a)) => Semigroup (CExprTypeProduct v) where
  (<>) = Barbies.bzipWith (<>)

instance (forall a. Monoid (v a)) => Monoid (CExprTypeProduct v) where
  mempty = Barbies.bpure mempty

primTypes_ :: Lens' (CExprTypeProduct f) (Arrays f)
primTypes_ inj (CExprTypeProduct prims funs sels) =
  (\p -> CExprTypeProduct p funs sels) <$> inj prims

-- | This class providing a lens into a functor of any base type is needed mainly for the graph
-- reification step: we preserve type-safety everywhere without dependent types by producing a
-- product ('CExprTypeProduct') of all the possible "next steps" (one for each base type) and
-- selecting the one we want via this lens.  As long as all the possible next steps have the same
-- (known) return type, no matter what the type @a@ providing the instance is, this is no problem.
--
-- This trick only works because we really have a closed set of base types allowed in the DSL.
--
-- There is nothing fundamental about this technique which necessitates the use of optics or the
-- @lens@ library; the interface is written in terms of a 'Lens'' because in the sharing recovery
-- algorithm, the main operations needed on the `CExprTypeProduct` structure are accessing and
-- updating in place pieces corresponding to one base type at a time, so optics seemed like a
-- natural fit.
class CExprTypeLens a where
  cexprType_ :: Lens' (CExprTypeProduct f) (f a)

instance CExprTypeLens Bool where
  cexprType_ = primTypes_ . prims_

instance CExprTypeLens Int8 where
  cexprType_ = primTypes_ . prims_

instance CExprTypeLens Int16 where
  cexprType_ = primTypes_ . prims_

instance CExprTypeLens Int32 where
  cexprType_ = primTypes_ . prims_

instance CExprTypeLens Int64 where
  cexprType_ = primTypes_ . prims_

instance CExprTypeLens Word8 where
  cexprType_ = primTypes_ . prims_

instance CExprTypeLens Word16 where
  cexprType_ = primTypes_ . prims_

instance CExprTypeLens Word32 where
  cexprType_ = primTypes_ . prims_

instance CExprTypeLens Word64 where
  cexprType_ = primTypes_ . prims_

instance CExprTypeLens Float where
  cexprType_ = primTypes_ . prims_

instance CExprTypeLens Double where
  cexprType_ = primTypes_ . prims_

instance CExprTypeLens CExprFunctionCall where
  cexprType_ inj (CExprTypeProduct prims funCalls selects) =
    go <$> inj funCalls
    where
      go fc = CExprTypeProduct prims fc selects

instance CExprTypeLens CExprSelectExpression where
  cexprType_ inj (CExprTypeProduct prims funCalls selects) =
    CExprTypeProduct prims funCalls <$> inj selects
