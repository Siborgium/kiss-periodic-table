
module Lib where

import Control.Monad (forM_)
import Prelude hiding (div)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Table as T

instance ToMarkup T.Resist where
  toMarkup t = html $ toHtml $ show t

instance ToMarkup CommonElement where
  toMarkup t = html $ do
    h1 ! class_ (groupAttr $ T.group t) $ toHtml $ T.symbol t
    div ! class_ (stringValue "mass-resist") $ do
      div ! class_ (stringValue "mass") $ toHtml $ T.mass t 
      div ! class_ (stringValue "resist") $ toHtml $ T.resist t
    p $ toHtml $ T.name t

instance ToMarkup Element where
  toMarkup (Element e) = toMarkup e
  toMarkup HSpecial = html $ h1 $ toHtml "{H}"
  toMarkup HigherOxyde = html $ toHtml "HigherOxydePlaceholder"
  toMarkup HydroCompound = html $ toHtml "HydroCompoundPlaceholder"
  toMarkup Placeholder = html $ toHtml "%"

page :: Html
page = docTypeHtml $ do
  H.head $ do
    H.title $ toHtml "Periodic table"
    H.style $ css
  body $ do
    h2 $ toHtml "Periodic table"
    H.table $ do
      tr $ (th $ toHtml "") >> forM_ groups groupName -- draw groups
      forM_ (zip T.table periods) $ \(row, p) -> do
        tr $ do
          th $ toHtml p
          forM_ row (td . toHtml)

css :: Html
css = toHtml $ "h1{align:center} table,th,td{border:1px solid black} table{width:60%}\
                \.mass-resist{width:100%}.mass{text-align:left}.resist{text-align:right}\
                \.sconfgroup{background-color:#fb81ad}.pconfgroup{background-color:#efdb78}\
                \.dconfgroup{background-color:#82c9f9}.fconfgroup{background-color:#ac86bf}\
                \.groupleft{text-align:left}.groupright{text-align:right}"

elemClass :: Element -> Attribute
elemClass (Element t) = class_ (confAttr $ T.conf t)
elemClass _ = class_ $ stringValue "placeholderClass"

confAttr :: ValentElectrons -> AttributeValue
confAttr v = case v of S -> stringValue "sconfgroup"
                       P -> stringValue "pconfgroup"
                       D -> stringValue "dconfgroup"
                       F -> stringValue "fconfgroup"

groupAttr :: Group -> AttributeValue
groupAttr (Group n A) = stringValue $ "groupleft"
groupAttr (Group n B) = stringValue $ "groupright"

groupName :: Int -> Html
groupName g = (if g == 8 then th ! (colspan $ stringValue "3") else th) $ toHtml $ "A " ++ (show g) ++ " B"

groups :: [Int]
groups = [1..8]

periods :: [Html]
periods = Prelude.map (toHtml . \p -> (if p <= 7 then show else periodsImpl) p) [1..11]
  where periodsImpl 8 = "Higher oxydes"
        periodsImpl 9 = "Hydrogen compounds"
        periodsImpl 10 = "Lantanoids"
        periodsImpl 11 = "Actinoids"
