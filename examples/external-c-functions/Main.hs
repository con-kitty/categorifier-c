{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Categorifier.C.CTypes.Codegen.Render.Render (RenderedFile (..))
import Categorifier.C.CTypes.Codegen.Run (renderCTypesModules)
import Categorifier.C.CTypes.Types (CxxType)
import Categorifier.C.KTypes.C (toCxxTypeViaC)
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment (getArgs)
import System.FilePath ((</>))
import F (fCategorified)
import Categorifier.C.Generate (writeCFiles)
import CTypes (CInput, COutput)

genCTypeModules :: String -> IO ()
genCTypeModules dir = do
  let cxxTypes :: [CxxType Proxy]
      cxxTypes =
        [ toCxxTypeViaC (Proxy @CInput),
          toCxxTypeViaC (Proxy @COutput)
        ]
      res = renderCTypesModules cxxTypes
  case res of
    Left err -> fail $ show err
    Right modules -> do
      for_ modules $ \(RenderedFile fileName contents) -> do
        let path = dir </> Text.unpack fileName
        Text.writeFile path contents
        putStrLn $ "Generated " <> path

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["gen-ctypes", dir] -> genCTypeModules dir
    [dir, name] -> writeCFiles (Text.pack dir) (Text.pack name) fCategorified
    _ -> fail "invalid args"
