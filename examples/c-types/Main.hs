{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Categorifier.C.CTypes.Codegen.Render.Render (RenderedFile (..))
import Categorifier.C.CTypes.Codegen.Run (renderCTypesModules)
import Categorifier.C.CTypes.ToCxxType (ToCxxType)
import Categorifier.C.CTypes.Types (CxxType)
import Categorifier.C.KTypes.C (C, toCxxTypeViaC)
import Categorifier.C.KTypes.Sum.Maybe (KMaybe)
import Data.Foldable (for_)
import Data.Int (Int32)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.FilePath ((</>))

data Foo = Foo
  { iInt32 :: C Int32,
    iMaybeDouble :: KMaybe C (C Double)
  }
  deriving (Generic)

instance ToCxxType C Foo

data Bar = Bar
  { oWord64 :: C Word64,
    oPair :: (C Float, C Double),
    oBool :: Bool
  }
  deriving (Generic)

instance ToCxxType C Bar

-- This generates c_types.h, to_arrays, from_arrays and other helper modules
-- in the given dir.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] -> genCTypeModules dir
    _ -> fail "invalid args"

genCTypeModules :: String -> IO ()
genCTypeModules dir = do
  let cxxTypes :: [CxxType Proxy]
      cxxTypes =
        [ toCxxTypeViaC (Proxy @Foo),
          toCxxTypeViaC (Proxy @Bar)
        ]
      res = renderCTypesModules cxxTypes
  case res of
    Left err -> fail $ show err
    Right modules -> do
      for_ modules $ \(RenderedFile fileName contents) -> do
        let path = dir </> Text.unpack fileName
        Text.writeFile path contents
        putStrLn $ "Generated " <> path
