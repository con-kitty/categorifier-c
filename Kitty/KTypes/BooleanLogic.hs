-- | Typeclasses used for simultaniously supporting numeric types
-- and symbolic types used for codegen and analysis.
module Kitty.KTypes.BooleanLogic
  ( KAnd (..),
  )
where

infixr 3 .&&

infixr 2 .||

class KAnd f where
  (.&&) :: f Bool -> f Bool -> f Bool

  (.||) :: f Bool -> f Bool -> f Bool

  kNot :: f Bool -> f Bool
