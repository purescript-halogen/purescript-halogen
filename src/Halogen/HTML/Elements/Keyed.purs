module Halogen.HTML.Elements.Keyed
  ( article, article_
  , colgroup, colgroup_
  , dialog, dialog_
  , div, div_
  , dl, dl_
  , fieldset, fieldset_
  , footer, footer_
  , form, form_
  , header, header_
  , menu, menu_
  , ol, ol_
  , table, table_
  , tbody, tbody_
  , tfoot, tfoot_
  , thead, thead_
  , tr, tr_
  , ul, ul_
  ) where

import Data.Tuple (Tuple)

import DOM.HTML.Indexed as I

import Halogen.HTML.Core (ElemName(..), HTML)
import Halogen.HTML.Elements (keyed)
import Halogen.HTML.Properties (IProp)

type KeyedNode r w i
   = Array (IProp r i)
  -> Array (Tuple String (HTML w i))
  -> HTML w i

article :: forall w i. KeyedNode I.HTMLarticle w i
article = keyed (ElemName "article")

article_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
article_ = article []

colgroup :: forall w i. KeyedNode I.HTMLcolgroup w i
colgroup = keyed (ElemName "colgroup")

colgroup_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
colgroup_ = colgroup []

dialog :: forall w i. KeyedNode I.HTMLdialog w i
dialog = keyed (ElemName "dialog")

dialog_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
dialog_ = dialog []

div :: forall w i. KeyedNode I.HTMLdiv w i
div = keyed (ElemName "div")

div_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
div_ = div []

dl :: forall w i. KeyedNode I.HTMLdl w i
dl = keyed (ElemName "dl")

dl_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
dl_ = dl []

fieldset :: forall w i. KeyedNode I.HTMLfieldset w i
fieldset = keyed (ElemName "fieldset")

fieldset_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
fieldset_ = fieldset []

footer :: forall w i. KeyedNode I.HTMLfooter w i
footer = keyed (ElemName "footer")

footer_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
footer_ = footer []

form :: forall w i. KeyedNode I.HTMLform w i
form = keyed (ElemName "form")

form_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
form_ = form []

header :: forall w i. KeyedNode I.HTMLheader w i
header = keyed (ElemName "header")

header_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
header_ = header []

menu :: forall w i. KeyedNode I.HTMLmenu w i
menu = keyed (ElemName "menu")

menu_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
menu_ = menu []

ol :: forall w i. KeyedNode I.HTMLol w i
ol = keyed (ElemName "ol")

ol_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
ol_ = ol []

table :: forall w i. KeyedNode I.HTMLtable w i
table = keyed (ElemName "table")

table_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
table_ = table []

tbody :: forall w i. KeyedNode I.HTMLtbody w i
tbody = keyed (ElemName "tbody")

tbody_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
tbody_ = tbody []

tfoot :: forall w i. KeyedNode I.HTMLtfoot w i
tfoot = keyed (ElemName "tfoot")

tfoot_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
tfoot_ = tfoot []

thead :: forall w i. KeyedNode I.HTMLthead w i
thead = keyed (ElemName "thead")

thead_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
thead_ = thead []

tr :: forall w i. KeyedNode I.HTMLtr w i
tr = keyed (ElemName "tr")

tr_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
tr_ = tr []

ul :: forall w i. KeyedNode I.HTMLul w i
ul = keyed (ElemName "ul")

ul_ :: forall w i. Array (Tuple String (HTML w i)) -> HTML w i
ul_ = ul []
