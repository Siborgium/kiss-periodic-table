
module Page where

import Control.Monad (forM_)
import Prelude hiding (div)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Elements as T
import Table(actinides, lanthanides, table) 

page :: Html
page = docTypeHtml $ do
  H.head $ do
    H.title $ toHtml "Periodic table"
    H.style $ css
  body $ do
    h2 $ toHtml "Periodic table"
    H.table $ do
      tr $ do
        rowHead ""
        forM_ groups groupName -- draw groups
      forM_ (zip Table.table rowIndices) $ \(row, r) -> do
        tr $ Page.period r $ rowData row
    br
    H.table $ do
      row "Lanthanides" lanthanides
      row "Actinides" actinides 

row h d = tr $ (rowHead h >> rowData d)

rowHead :: String -> Html
rowHead = th . toHtml

rowData :: [Element] -> Html
rowData d = forM_ d $ \t -> td ! elemClass t $ toHtml t

eSym :: Html -> Html
eSym = div ! class_ (stringValue "symbol")

eName :: Html -> Html
eName = div ! class_ (stringValue "name")

eSpec :: Html -> Html
eSpec = div ! class_ (stringValue "spec")

css :: Html
css = toHtml $ "html{font:small sans-serif}\
                \th,td{border:1px solid black;}\
                \td{min-width:5%}\
                \th{min-width:5%}\
                \table{display:block;align:center;max-width:70%;max-height:70%;border-collapse:collapse;}\
                \.mass-resist{display:grid;grid-template-columns:1fr 1fr;font-size:small}\
                \.mass{text-align:left}\
                \.resist{text-align:right}\
                \.symbol-number{display:grid;grid-template-columns:1fr 1fr;font-size:small;\
                \font-weight:bold;font-size:medium}\
                \.sconfgroup{background-color:#fb81ad}.pconfgroup{background-color:#efdb78}\
                \.dconfgroup{background-color:#82c9f9}.fconfgroup{background-color:#ac86bf}\
                \.groupleft{text-align:left}.groupright{text-align:right}\
                \.spec{font-weight:bold;font-size:normal;text-align:center}"

elemClass :: Element -> Attribute
elemClass (Element t) = class_ (confAttr $ T.conf t)
elemClass HSpecial = class_ (stringValue "sconfgroup")
elemClass _ = class_ $ stringValue "placeholderClass"

confAttr :: ValentElectrons -> AttributeValue
confAttr v = case v of S -> stringValue "sconfgroup"
                       P -> stringValue "pconfgroup"
                       D -> stringValue "dconfgroup"
                       F -> stringValue "fconfgroup"

groupAttr :: Group -> AttributeValue
groupAttr (Group n A) = stringValue $ "groupleft"
groupAttr (Group n B) = stringValue $ "groupright"

groupName :: (Int, String) -> Html
groupName (g, n) = (if g == 8 then th ! (colspan $ stringValue "3") else th) $ toHtml $ "A " ++ n ++ " B"

groups :: [(Int, String)]
groups = zip [1..8] ["I", "II", "III", "IV", "V", "VI", "VII", "VIII"]


data RowStyle = Wd Int | Sn
data Row = N String RowStyle | U

rowIndices :: [Row]
rowIndices = [N "1" Sn, N "2" Sn, N "3" Sn, N "4" (Wd 2), U, N "5" (Wd 2), U, N "6" (Wd 2), U, N "7" (Wd 2), U, N "Higher oxydes" Sn, N "Hydrogen compounds" Sn]

period :: Row -> Html -> Html
period r h = case r of
    U -> h
    N n (Wd i) -> periodWideImpl n i
    N n _ -> periodSingleImpl n
  where periodWideImpl n i = (th ! (rowspan $ stringValue $ show i) $ toHtml n) >> h
        periodSingleImpl n = (th $ toHtml n) >> h

instance ToMarkup T.Resist where
  toMarkup t = html $ toHtml $ show t

instance ToMarkup CommonElement where
  toMarkup t = html $ do
    div ! class_ (stringValue "symbol-number") $ markCommonElemTitle t
    div ! class_ (stringValue "mass-resist") $ do
      div ! class_ (stringValue "mass") $ toHtml $ show $ T.mass t 
      div ! class_ (stringValue "resist") $ toHtml $ T.resist t
    eName $ toHtml $ T.name t

markCommonElemTitle :: CommonElement -> Html
markCommonElemTitle e = implMark (T.group e) (T.symbol e, show $ T.number e)
  where implMark g d = do
          let (a, b) = flipGroup g d
          div ! class_ (stringValue "groupleft") $ toHtml a
          div ! class_ (stringValue "groupright") $ toHtml b
        flipGroup (Group _ A) x = x
        flipGroup (Group _ B) (x, y) = (y, x)

instance ToMarkup Element where
  toMarkup (Element e) = toMarkup e
  toMarkup HSpecial = html $ eSpec $ toHtml "{H}"
  toMarkup (Ro n m) = html $ eSpec $ do
    toHtml "R"
    sub $ toHtml $ if n == 1 then "" else show n
    toHtml "O"
    sub $ toHtml $ if m == 1 then "" else show m
  toMarkup HydroCompound = html $ eSpec $ toHtml "RnHm"
  toMarkup Placeholder = html $ toHtml "%"
  toMarkup Empty = html $ toHtml ""


