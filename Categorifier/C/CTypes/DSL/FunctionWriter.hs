{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Categorifier.C.CTypes.DSL.FunctionWriter
  ( FunWriter,

    -- * Function building
    cexprFunctionFromParams,
    Func1,
    Func2,
    Func3,
    Func4,

    -- * Creation
    unsafeNewNamed,

    -- * Assignment
    (=:),

    -- * Misc
    force_,
    comment,
    cxxLambda,
    unionCast,
    unsafeAsIdentifier,

    -- * Control structure
    ret,
    retVoid,
    if_,
    ifElse_,

    -- * Loops
    cxxForLoopWithTypeNamed,
    loop,
    loopWithType,

    -- * Case an Enums
    caseUnion,
    caseUnionTag,
    caseEnum,

    -- * Functions on std::map
    traverseStdMap,
    zipMap,
    zipMapAll,

    -- * Internal, should not be exposed. TODO
    runFunWriter,
    forLoop,
    increment,
    newDefaultNamed,
    newDefaultCxxNamed,
    zeroCArrayNamed,
    emptyCArrayNamed,
  )
where

import Categorifier.C.CTypes.DSL.CxxAst
  ( CExpr (..),
    CFunction (..),
    CTypeWithBackdoor (..),
    ConditionBlock (..),
    Identifier (..),
    Param (..),
    StdFunc (..),
    TapeElement (..),
    ToCTypeWithBackdoor (..),
    (#!),
  )
import qualified Categorifier.C.CTypes.Render as R
import Categorifier.C.CTypes.Types
import Control.Monad (replicateM)
import Control.Monad.Trans.RWS.Lazy (RWST (..), get, put, runRWS, tell)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Data.Vector as V
import PyF (fmt)

-- This is just a monadic binding to list temporaries names
newtype FunWriterState = FunWriterState
  { fwsUniqueIdentifierIndex :: Int
  }

-- | Generate an unique identifier
makeUniqueId :: Monad m => FunWriterT m Identifier
makeUniqueId = do
  state0 <- get
  let k = fwsUniqueIdentifierIndex state0
  put (state0 {fwsUniqueIdentifierIndex = k + 1})
  pure (Identifier ("unique_variable_" <> T.pack (show k)))

-- monad for writing imperative C code
-- Written as a transformer for now, perhaps we'll have use of it later
-- Morally we are only using the Writer and State of the RWS.
type FunWriterT = RWST () [TapeElement] FunWriterState

type FunWriter = FunWriterT Identity

-- | Run a FunWriter and only returns the associated tape element
runFunWriter :: FunWriter () -> [TapeElement]
runFunWriter fun = let (_a, _s, w) = runRWS fun () (FunWriterState 0) in w

-- | Run a sub block of code
runSubFunWriter :: FunWriter t -> FunWriter ([TapeElement], t)
runSubFunWriter fun = do
  currentState <- get
  let (a, newState, w) = runRWS fun () currentState
  put newState
  pure (w, a)

-- | Select a name for a symbol with a provided default name
-- | No name conflict check are done
-- | TODO: remove this function when the convertion to fully generated
--   name is done
unsafeNewNamed :: ToCTypeWithBackdoor t => T.Text -> t -> CExpr -> FunWriter CExpr
unsafeNewNamed name t val = do
  let var = Identifier name
  tell [Declare (toCTypeWithBackdoor t) var val]
  pure (Ident var)

-- | TODO: C++ default build (or not initialised...)
newDefaultNamed :: ToCTypeWithBackdoor t => T.Text -> t -> FunWriter CExpr
newDefaultNamed name t = do
  let var = Identifier name
  tell [DeclareDefault (toCTypeWithBackdoor t) var]
  pure (Ident var)

newDefaultCxxNamed :: ToCTypeWithBackdoor t => T.Text -> t -> FunWriter CExpr
newDefaultCxxNamed name t = do
  let var = Identifier name
  tell [DeclareCxxDefault (toCTypeWithBackdoor t) var]
  pure (Ident var)

-- | Assignment
(=:) :: CExpr -> CExpr -> FunWriter ()
(=:) lhs rhs = tell [Assign lhs rhs]

infix 1 =:

-- | @if_ a b@ will run @b@ if @a@ is true
if_ :: CExpr -> FunWriter () -> FunWriter ()
if_ predicate expr = do
  (block, ()) <- runSubFunWriter expr
  tell [SingleIf predicate block]

-- | @if_ a b@ will run @b@ if @a@ is true
ifElse_ :: CExpr -> FunWriter () -> FunWriter () -> FunWriter ()
ifElse_ predicate exprT exprF = do
  (blockT, ()) <- runSubFunWriter exprT
  (blockF, ()) <- runSubFunWriter exprF
  tell [If [ConditionBlock predicate blockT] (Just blockF)]

-- TODO: this should be typed in some way

-- | Return an expression
ret :: CExpr -> FunWriter ()
ret v = tell [Return v]

-- | Return nothing
retVoid :: FunWriter ()
retVoid = tell [ReturnVoid]

-- | Write a comment inside a function
comment :: T.Text -> FunWriter ()
comment t = tell [Comment' t]

-- | A 'loop a b' will repeat @b@ @a@ times
loop :: CExpr -> (CExpr -> FunWriter ()) -> FunWriter ()
loop bound = loopWithType (CTypePrim' (PrimInt64 Proxy)) bound

-- | TODO: remove that once the transition is done
loopWithType :: ToCTypeWithBackdoor t => t -> CExpr -> (CExpr -> FunWriter ()) -> FunWriter ()
loopWithType t bound f = do
  (initBlock, ident) <- runSubFunWriter $ unsafeNewNamed "k" (toCTypeWithBackdoor t) (LiteralInt 0)
  (step, ()) <- runSubFunWriter $ increment ident
  (block, ()) <- runSubFunWriter (f ident)
  let cond = ident :< bound
  tell [ForLoop initBlock cond step block]

-- | For loop.
-- TODO(guillaume) this is too general and we want to avoid exposing this function in the public API
forLoop :: FunWriter a -> (a -> CExpr) -> (a -> FunWriter ()) -> (a -> FunWriter ()) -> FunWriter ()
forLoop initBlock cond step body = do
  (init', vars) <- runSubFunWriter initBlock
  (step', ()) <- runSubFunWriter (step vars)
  (body', ()) <- runSubFunWriter (body vars)
  tell [ForLoop init' (cond vars) step' body']

-- This is not exposed, it just increment a value
increment :: CExpr -> FunWriter ()
increment e = tell [PlusPlus e]

-- | Convert a list of 'Param' to a function which takes 'CExpr' with the correct names.
--   @cexprFunctionFromParams [Param ... "x", Param ... "y"] $ \x y ->@ will call the
--   lambda with @x@ and @y@ as 'CExpr' containing identifier for @x@
--   and @y@.
-- The match between the number of argument of @e@ and the length of
-- the '[Param]' list is ensured at runtime by 'PolyCallable'.
cexprFunctionFromParams :: PolyCallable e CExpr (FunWriter ()) => [Param] -> e -> [TapeElement]
cexprFunctionFromParams params = runFunWriter . polyCall (Ident . pId <$> params)

-- Type alias to represents naked 'CFunction' with different number of arguments
-- TODO: We should not expose '[TapeElement]' function, in the future
-- this will be deduced automatically.
type Func1 = (CExpr -> FunWriter ()) -> [TapeElement]

type Func2 = (CExpr -> CExpr -> FunWriter ()) -> [TapeElement]

type Func3 = (CExpr -> CExpr -> CExpr -> FunWriter ()) -> [TapeElement]

type Func4 = (CExpr -> CExpr -> CExpr -> CExpr -> FunWriter ()) -> [TapeElement]

-- | @unionCast t0 t1 v@ will cast @t0@ to @t1@ through an union.
unionCast :: (ToCTypeWithBackdoor t, ToCTypeWithBackdoor t') => t -> t' -> CExpr -> FunWriter CExpr
unionCast (toCTypeWithBackdoor -> cFrom) (toCTypeWithBackdoor -> cTo) e = do
  out <- makeUniqueId
  tell [UnionCast cFrom cTo e out]
  pure (Ident out)

-- | Force an expression to be evaluated.
--   This is mostly for expression with side effect, such as function call with side effect
--   We'd like an API to describe side effect function and remove the usage of force
force_ :: CExpr -> FunWriter ()
force_ e = tell [ForceExpr e]

-- | This is a cxxLambda
--   Warning. The capture variables are only working if the lambda is
--   used close to its creation
cxxLambda ::
  PolyCallable e CExpr (FunWriter ()) => [Identifier] -> [CxxUnionCon Proxy] -> e -> CExpr
cxxLambda capture argsTypes f = Lambda capture (zip argsTypes argsNames) block
  where
    (argsNames, _, block) = runRWS lambdaWriter () (FunWriterState 0)
    lambdaWriter = do
      -- TODO: special case. Lambda with one argument uses xc
      -- This is introduce to allow the new codegen to match the old one
      -- remove it once the new is used.
      args <-
        if length argsTypes == 1
          then pure [Identifier "xc"]
          else replicateM (length argsTypes) makeUniqueId
      () <- polyCall (Ident <$> args) f
      pure args

-- | A polycallable class takes a list and call a N-ary function using the arguments from the list
-- or fails with @error@
class PolyCallable expr t ret where
  polyCall :: [t] -> expr -> ret

instance PolyCallable ret t ret where
  polyCall l e = case l of
    [] -> e
    _ -> error "Calling a 0-ary function with the wrong number of arguments."

instance PolyCallable (t -> ret) t ret where
  polyCall l e = case l of
    [x] -> e x
    _ -> error "Calling a 1-ary function with the wrong number of arguments."

instance PolyCallable (t -> t -> ret) t ret where
  polyCall l e = case l of
    [x, y] -> e x y
    _ -> error "Calling a 2-ary function with the wrong number of arguments."

instance PolyCallable (t -> t -> t -> ret) t ret where
  polyCall l e = case l of
    [x, y, z] -> e x y z
    _ -> error "Calling a 3-ary function with the wrong number of arguments."

instance PolyCallable (t -> t -> t -> t -> ret) t ret where
  polyCall l e = case l of
    [x, y, z, w] -> e x y z w
    _ -> error "Calling a 4-ary function with the wrong number of arguments."

-- | Unsafe function, transform a CExpr to an identifier
unsafeAsIdentifier :: CExpr -> Identifier
unsafeAsIdentifier (Ident i) = i
unsafeAsIdentifier _ = error "This is not an identifier"

-- | C++ foreach loop
cxxForLoopWithTypeNamed ::
  ToCTypeWithBackdoor t =>
  -- | Name of the loop variable
  T.Text ->
  -- | Type of the loop variable
  t ->
  -- | Expression to loop on
  CExpr ->
  -- | Loop body
  (CExpr -> FunWriter ()) ->
  FunWriter ()
cxxForLoopWithTypeNamed name t expr block = do
  -- TODO: put back the automatic naming
  -- identifier <- makeUniqueId
  let identifier = Identifier name
  (block', ()) <- runSubFunWriter (block (Ident identifier))
  tell [ForCxx (toCTypeWithBackdoor t) identifier expr block']

-- | Handle all the cases of an union
caseUnion ::
  -- | The union type
  CUnion Proxy ->
  -- | The union value
  CExpr ->
  -- | A map of function from union constructor
  M.Map (CCon Proxy) CFunction ->
  -- | A callback function which takes a function for this constructor and the constructor name
  (CFunction -> RfName -> T.Text -> FunWriter ()) ->
  -- | The default case
  FunWriter () ->
  FunWriter ()
caseUnion cunion var conFunMap block defaultBlock = do
  let tagMember = makeRfName $ R.renderCUnionTagMember cunion
  cases <- for (V.toList (cuCons cunion)) $ \ccon -> case M.lookup ccon conFunMap of
    Nothing -> error $ "missing CCon function " <> show ccon
    Just conIsEqualFun -> do
      let memberName = makeRfName $ R.renderCUnionMemberName ccon
          conType = R.renderCUnionConType (CUnionCon cunion ccon)
      (c, ()) <- runSubFunWriter $ block conIsEqualFun memberName conType
      pure (R.renderCUnionTagLiteral cunion ccon, c)
  (defaultCase, ()) <- runSubFunWriter defaultBlock
  tell [SwitchCase (Just (R.renderCUnionTagType cunion)) (var :. tagMember) cases defaultCase]

-- | Handle all the cases of an union, based on the union tag
caseUnionTag ::
  -- | The union type
  CUnion Proxy ->
  -- | The union value
  CExpr ->
  -- | Callback: tagLiteral as string
  (T.Text -> FunWriter ()) ->
  -- | The default case
  FunWriter () ->
  FunWriter ()
caseUnionTag cunion var block defaultBlock = do
  cases <- for (V.toList (cuCons cunion)) $ \ccon -> do
    let tagLiteral = R.renderCUnionTagLiteral cunion ccon
    (c, ()) <- runSubFunWriter $ block tagLiteral
    pure (R.renderCUnionTagLiteral cunion ccon, c)
  (defaultCase, ()) <- runSubFunWriter defaultBlock
  tell [SwitchCase (Just (R.renderCUnionTagType cunion)) var cases defaultCase]

-- | Handle all the case of an enum
caseEnum ::
  -- | The enum type
  CEnum Proxy ->
  -- | The enum value
  CExpr ->
  -- | Text representation of the enum to a block
  (T.Text -> FunWriter ()) ->
  -- | Default case (should not happen ;)
  FunWriter () ->
  FunWriter ()
caseEnum cenum value block defaultBlock = do
  cases <- for (allEnumCons cenum) $ \con -> do
    (c, ()) <- runSubFunWriter $ block (R.renderEnumConLiteral con)
    pure (R.renderEnumConLiteral con, c)
  (defaultCase, ()) <- runSubFunWriter defaultBlock
  tell [SwitchCase (Just (R.renderCEnumType cenum)) value cases defaultCase]

-- TODO(greg): This isn't really right but should work for most common keys.
keyToString :: CxxType Proxy -> CExpr -> CExpr
keyToString keyType it = case keyType of
  CxxTypePrim (PrimString _) -> it :-> #first
  CxxTypeCType (CTypePrim _) -> StdToString #! [it :-> #first]
  _ -> error "I don't know what kind of key it is."

-- | Do an action on all key of a C++ map
traverseStdMap ::
  -- | Key type
  CxxType Proxy ->
  -- | Value type
  CxxType Proxy ->
  -- | The map itself (TODO(grep/guillaume): write a dedicated type)
  CExpr ->
  -- | Callback function, key -> value -> codeblock
  (CExpr -> CExpr -> FunWriter ()) ->
  FunWriter ()
traverseStdMap keyType valueType m onKey = do
  let itType =
        let rKeyType = R.renderCxxType keyType
            rValType = R.renderCxxType valueType
         in CTypeBackdoor [fmt|typename std::map<{rKeyType}, {rValType}>::const_iterator|]
  it <- unsafeNewNamed "it" itType (m :. #begin #! [])
  forLoop (pure ()) (\() -> it :!= (m :. #end #! [])) (\() -> increment it) $
    \() -> onKey (keyToString keyType it) (it :-> #second)

-- | Zip over two map, key by key, stop when the first map is exhausted
zipMap ::
  -- | Key type
  CxxType Proxy ->
  -- | Value type
  CxxType Proxy ->
  -- | Map X
  CExpr ->
  -- | Map Y
  CExpr ->
  -- | Callback function (keyA, keyB) -> (valA, valB) -> block`
  ((CExpr, CExpr) -> (CExpr, CExpr) -> FunWriter ()) ->
  FunWriter ()
zipMap keyType valueType x y onKey = do
  let itType =
        let rKeyType = R.renderCxxType keyType
            rValType = R.renderCxxType valueType
         in CTypeBackdoor [fmt|typename std::map<{rKeyType}, {rValType}>::const_iterator|]
  itX <- unsafeNewNamed "it_x" itType (x :. #begin #! [])
  itY <- unsafeNewNamed "it_y" itType (y :. #begin #! [])
  forLoop
    (pure ())
    (\() -> itX :!= (x :. #end #! []))
    ( \() -> do
        increment itX
        increment itY
    )
    $ \() -> onKey (itX :-> #first, itY :-> #first) (itX :-> #second, itY :-> #second)

-- | Zip over two map, handling all elements
zipMapAll ::
  -- | Key type
  CxxType Proxy ->
  -- | Value type
  CxxType Proxy ->
  -- | First map X
  CExpr ->
  -- | Second map Y
  CExpr ->
  -- | 'f key valA valB': Called for keys which are in both maps
  (CExpr -> CExpr -> CExpr -> FunWriter ()) ->
  -- | 'f key valA': Called for key only in map X
  (CExpr -> CExpr -> FunWriter ()) ->
  -- | 'f key valB': Called for key only in map Y
  (CExpr -> CExpr -> FunWriter ()) ->
  FunWriter ()
zipMapAll keyType valueType x y onBothKey onKeyX onKeyY = do
  let itType =
        let rKeyType = R.renderCxxType keyType
            rValType = R.renderCxxType valueType
         in CTypeBackdoor [fmt|typename std::map<{rKeyType}, {rValType}>::const_iterator|]
  itX <- unsafeNewNamed "it_x" itType (x :. #begin #! [])
  forLoop (pure ()) (\() -> itX :!= (x :. #end #! [])) (\() -> increment itX) $ \() -> do
    itY <- unsafeNewNamed "it_y" itType (y :. #find #! [itX :-> #first])
    ifElse_
      (itY :== (y :. #end #! []))
      ( do
          comment "Print keys of x that are not in y."
          onKeyX (keyToString keyType itX) (itY :-> #second)
      )
      ( do
          comment "For keys in both maps, call the function on the values."
          onBothKey (keyToString keyType itX) (itX :-> #second) (itY :-> #second)
      )
  comment "Print keys of y that are not in x."
  forLoop
    (unsafeNewNamed "it_y" itType (y :. #begin #! []))
    (\itY -> itY :!= (y :. #end #! []))
    increment
    $ \itY ->
      if_ (x :. #find #! [itY :-> #first] :== (x :. #end #! [])) $
        onKeyY (keyToString keyType itY) (itY :-> #second)

zeroCArrayNamed :: Identifier -> CType Proxy -> Int -> FunWriter ()
zeroCArrayNamed ident ctype size =
  tell [TapeBackdoor [fmt|{R.renderCType ctype} {unIdentifier ident}[{size}] = {{0}};|]]

-- | A C array default initialized. e.g int foo[5];
emptyCArrayNamed :: Identifier -> CType Proxy -> Int -> FunWriter ()
emptyCArrayNamed ident ctype size =
  tell [TapeBackdoor [fmt|{R.renderCType ctype} {unIdentifier ident}[{size}];|]]
