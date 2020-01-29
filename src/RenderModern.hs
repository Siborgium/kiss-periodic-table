
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

module RenderModern(page) where

import Control.Monad (forM_)
import Prelude hiding (div, span)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Elements as E
import Tables(actinides, lanthanides, modernTable) 

data MElement = M E.Element

page :: String -> Html
page title = docTypeHtml $ do
  H.head $ do
    H.title $ toHtml title
  H.body $ do
    toHtml "Placeholder for now"
