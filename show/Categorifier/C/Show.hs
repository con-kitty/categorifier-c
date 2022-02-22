-- | Helpers for writing `showsPrec` implementations that respect precedence properly. You should,
--   of course, still prefer @deriving `Show`@ (or, better yet, eschewing `Show` all together)
--   whenever possible.
--
-- > import qualified Categorifier.C.Show as Show
-- >
-- > data MyType a
-- >  = MyType a :+: MyType a
-- >  | MyType a :*: MyType a
-- >  | Const a
-- >  | Convert (a -> a) a -- ^ this prevents @deriving `Show`@
-- >
-- > infixl 6 :+:
-- > infixl 7 :*:
-- >
-- > instance Show a => Show (MyType a) where
-- >   showsPrec p = \case
-- >     x :+: y = Show.infixlPrec p 6 (Show.operand x) ":+:" (Show.operand y)
-- >     x :*: y = Show.infixlPrec p 7 (Show.operand x) ":*:" (Show.operand y)
-- >     Const x = Show.appPrec p "Const" [Show.arg x]
-- >     Convert f x = Show.appPrec p "Convert" [Show.skipArg f, Show.arg x]
--
--   The pattern of the @infix*@ functions is that the first argument is the current precedence from
--  `showsPrec`, the second is the precedence from the fixity delaration, then
--   argument-operator-argument.
module Categorifier.C.Show
  ( infixPrec,
    infixlPrec,
    infixrPrec,
    appPrec,
    operand,
    skipOperand,
    arg,
    skipArg,
    record,
  )
where

import Data.List (intersperse)
import Data.Monoid (Endo (..))

-- | For `show`ing operators with an @infix@ fixity declaration.
infixPrec ::
  -- | The precedence passed in from `showsPrec`
  Int ->
  -- | The precedence from the fixity declaration
  Int ->
  -- | The left argument to the operator
  (Int -> ShowS) ->
  -- | The display of the operator
  String ->
  -- | The right argument to the operator
  (Int -> ShowS) ->
  ShowS
infixPrec p n x op y =
  showParen (p > n) $
    x (n + 1) . showString (" " <> op <> " ") . y (n + 1)

-- | For `show`ing operators with an @infixl@ fixity declaration.
infixlPrec ::
  -- | The precedence passed in from `showsPrec`
  Int ->
  -- | The precedence from the fixity declaration
  Int ->
  -- | The left argument to the operator
  (Int -> ShowS) ->
  -- | The display of the operator
  String ->
  -- | The right argument to the operator
  (Int -> ShowS) ->
  ShowS
infixlPrec p n x op y =
  showParen (p > n) $
    x n . showString (" " <> op <> " ") . y (n + 1)

-- | For `show`ing operators with an @infixr@ fixity declaration.
infixrPrec ::
  -- | The precedence passed in from `showsPrec`
  Int ->
  -- | The precedence from the fixity declaration
  Int ->
  -- | The left argument to the operator
  (Int -> ShowS) ->
  -- | The display of the operator
  String ->
  -- | The right argument to the operator
  (Int -> ShowS) ->
  ShowS
infixrPrec p n x op y =
  showParen (p > n) $ x (n + 1) . showString (" " <> op <> " ") . y n

-- | For `show`ing non-operator data constructors.
appPrec :: Int -> String -> [ShowS] -> ShowS
appPrec p app args =
  showParen (p > 10) $ showString app . foldMap (showString " " .) args

-- | For `show`ing arguments to operators.
operand :: Show a => a -> Int -> ShowS
operand = flip showsPrec

skipOperand :: a -> Int -> ShowS
skipOperand _ _ = showString "_"

-- | For `show`ing arguments to a non-operator data constructor.
arg :: Show a => a -> ShowS
arg = showsPrec 11

-- | For `show`ing arguments to a non-operator data constructor.
skipArg :: a -> ShowS
skipArg _ = showString "_"

-- | Print a non-operator constructor as a record.
--
--  __NB__: This doesn't take a precedence, because records bind too damned tightly.
record :: String -> [(String, ShowS)] -> ShowS
record rcd fields =
  showString rcd
    . showString " {"
    . appEndo
      ( mconcat . intersperse (Endo $ showString ", ") $
          fmap (\(k, v) -> Endo $ ((k <> " = ") <>) . v) fields
      )
    . showString "}"
