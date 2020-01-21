module Main where

import Data.ByteString.Lazy(writeFile)
import Lib
import Prelude hiding (writeFile)
import System.Environment(getArgs)
import System.IO(IOMode(WriteMode), withFile)
import Text.Blaze.Renderer.Utf8 (renderMarkup)

main :: IO ()
main = do
  [p] <- getArgs
  putStrLn $ "Writing to " ++ p
  writeFile p $ renderMarkup $ Lib.page
