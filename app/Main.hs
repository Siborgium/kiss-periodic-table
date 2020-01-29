-- Copyright 2020 Sergey Smirnykh

-- This file is part of kiss-periodic-table

-- kiss-periodic-table is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- kiss-periodic-table is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with kiss-periodic-table. If not, see <https:-- www.gnu.org/licenses/>.

module Main where

import Data.ByteString.Lazy (writeFile)
import Prelude hiding (writeFile)
import System.Environment(getArgs)
import System.IO(IOMode(WriteMode), withFile)
import Text.Blaze.Renderer.Utf8 (renderMarkup)

import RenderOld (page)

main :: IO ()
main = do
  args <- getArgs
  let p = if null args then "index.html" else head args
  putStrLn $ "Writing to " ++ p
  writeFile p $ renderMarkup $ page "Periodic table"
