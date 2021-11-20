{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Lightweight wrapper around 'TypeRep' that lets us filter out unwanted parts.
-- Due to dependency loops we are filtering out C
-- by string processing :(.
-- I tried it the "right way" way at first, filtering out C by comparing to
-- @typeRep (Proxy @C)@, but when you don't construct
-- this greedily when you make a CxxType, it leads to difficulties when processing
-- recursive data types like trees later.
module Kitty.CTypes.KTypeRep
  ( KTypeRep,
    toKTypeRep,
    splitKTyConApp,
    ctypeNameFromKTypeRep,
    primTyConMap,
    isTupleKTypeRep,
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Typeable
  ( TyCon,
    TypeRep,
    splitTyConApp,
    tyConModule,
    tyConName,
    tyConPackage,
    typeRep,
    typeRepTyCon,
  )
import GHC.Generics (Generic)
import Kitty.Prim (allPrimTypes, getIsPrimitiveRep, getPrimCName)
import PyF (fmt)

primTyConMap :: M.Map TyCon T.Text
primTyConMap = M.fromListWith const $ fmap tyConAndName allPrimTypes
  where
    tyConAndName p = (typeRepTyCon $ getIsPrimitiveRep p, getPrimCName p)
{-# INLINEABLE primTyConMap #-}

-- | Same structure as 'TypeRep" from "Data.Typeable".
-- We can't use their representation because we need to do some unsafe stripping
-- of 'TyCon's like C.
data KTypeRep
  = UnsafeKTypeRep TyCon [KTypeRep]
  deriving (Eq, Ord, Generic)

instance NFData KTypeRep

-- | For minor introspection.
splitKTyConApp :: KTypeRep -> (TyCon, [KTypeRep])
splitKTyConApp (UnsafeKTypeRep tyCon apps) = (tyCon, apps)
{-# INLINEABLE splitKTyConApp #-}

-- | Construct a KTypeRep by filtering out unwanted things like C.
toKTypeRep :: TypeRep -> KTypeRep
toKTypeRep x = case cleanupKTypeRep x of
  Nothing -> error $ "cleanupKTypeRep made type rep vanish: " <> show x
  Just r -> r
{-# INLINEABLE toKTypeRep #-}

cleanupKTypeRep :: TypeRep -> Maybe KTypeRep
cleanupKTypeRep rep
  | -- TODO(greg): clean up code duplication
    isC tyCon = case apps of
    [] -> Nothing -- C just disappears
    [x] -> cleanupKTypeRep x -- C a -> a
    _ -> error [fmt|Con' C with too many apps: {show rep}|]
  | otherwise = Just $ UnsafeKTypeRep tyCon $ mapMaybe cleanupKTypeRep apps
  where
    (tyCon, apps) = splitTyConApp rep

validationErrorMessage :: TyCon -> String
validationErrorMessage tyCon =
  [fmt|
Error while cleanup up KTypeRep!

  TyCon '{show tyCon}' from package {tyConPackage tyCon}
  came from unexpected module {tyConModule tyCon}.

If you've moved '{show tyCon}' then please update 'KTypeRep.hs' to reflect the change.

Right now we identify C by showing a TyCon and comparing to strings "C", etc.
The right way is do this would be to check against `typeRep (Proxy @Foo)` directly but
this causes recursive dependencies. I get the impression hat you have to check greedily
in order to prevent the unsanitized version leaking.

We could check both the tyConName and the tyConModule, but if we get the module wrong
we'll miss a replacement causing all kinds of errors in C-land.

We could replace every tyConName that matches but I don't want to replace any besides the one from
the typeclass hierarchy.

So instead I just added an assertion that the tyConName must come from the expected module.

TODO(greg): fix this.
|]

isC :: TyCon -> Bool
-- if we had C in scope
--isC tyCon = tyCon == typeRepTyCon (typeRep (Proxy @C))
isC tyCon
  | show tyCon /= "C" = False
  | tyConModule tyCon == "Kitty.KTypes.C" = True
  | otherwise = error (validationErrorMessage tyCon)

-- The canonical struct name
ctypeNameFromKTypeRep :: KTypeRep -> T.Text
ctypeNameFromKTypeRep rep@(UnsafeKTypeRep tyCon apps) = case M.lookup tyCon primTyConMap of
  Nothing -> ctypeNameFromKTypeRep' rep
  Just name -> case apps of
    [] -> name
    _ ->
      error [fmt|got primitive tyCon {show tyCon} with c name {name} but non-zero apps {show apps}|]
{-# INLINEABLE ctypeNameFromKTypeRep #-}

ctypeNameFromKTypeRep' :: KTypeRep -> T.Text
ctypeNameFromKTypeRep' (UnsafeKTypeRep tyCon apps) =
  cleanUpTyCon tyCon <> foldMap ctypeNameFromKTypeRep' apps
  where
    cleanUpTyCon :: TyCon -> T.Text
    cleanUpTyCon = removeQuestionableCharacters . T.pack . show
    removeQuestionableCharacters :: T.Text -> T.Text
    removeQuestionableCharacters =
      T.replace "_S_S_S_S_Z" "4"
        . T.replace "_S_S_S_Z" "3"
        . T.replace "_S_S_Z" "2"
        . T.replace "_S_Z" "1"
        . T.replace "_Z" "0"
        . T.replace "'" "_"
        . T.replace " " "_"
        . T.replace ":" "_"
        . T.replace "." "_"
        . T.replace "[]" "List"
        . T.replace "()" "Unit"
        . T.replace "(,)" "Tuple2"
        . T.replace "(,,)" "Tuple3"
        . T.replace "(,,,)" "Tuple4"
        . T.replace "(,,,,)" "Tuple5"
        . T.replace "(,,,,,)" "Tuple6"

-- The rest of the module is the show instance.
-- It is blindly adapted from Data.Typeable.Internal.
instance Show KTypeRep where
  showsPrec = showKTypeRep

isListTyCon :: TyCon -> Bool
isListTyCon tc = tc == typeRepTyCon (typeRep (Proxy @[]))

isTupleTyCon :: TyCon -> Bool
isTupleTyCon tc
  | ('(' : ',' : _) <- tyConName tc = True
  | otherwise = False

isTupleKTypeRep :: KTypeRep -> Bool
isTupleKTypeRep (UnsafeKTypeRep tyCon _) = isTupleTyCon tyCon

showArgs :: Show a => ShowS -> [a] -> ShowS
showArgs _ [] = id
showArgs _ [a] = showsPrec 10 a
showArgs sep (a : as) = showsPrec 10 a . sep . showArgs sep as

isOperatorTyCon :: TyCon -> Bool
isOperatorTyCon tc
  | symb : _ <- tyConName tc, symb `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String) = True
  | otherwise = False

showTyCon :: TyCon -> ShowS
showTyCon tycon = showParen (isOperatorTyCon tycon) (shows tycon)

showKTypeRep :: Int -> KTypeRep -> ShowS
showKTypeRep _ (UnsafeKTypeRep tyCon []) = showTyCon tyCon
showKTypeRep p (UnsafeKTypeRep tyCon apps)
  | isListTyCon tyCon, [ty] <- apps = showChar '[' . shows ty . showChar ']'
  | isTupleTyCon tyCon = showChar '(' . showArgs (showChar ',') apps . showChar ')'
  | otherwise = showParen (p > 9) $ showsPrec 8 tyCon . showChar ' ' . showsPrec 10 apps
