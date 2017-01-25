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

import Halogen.HTML.Core (ElemName(..), HTML)
import Halogen.HTML.Properties (IProp, I, GlobalProperties, InteractiveEvents)

import Halogen.HTML.Elements (keyed)

type KeyedNode r p i
   = Array (IProp (InteractiveEvents (GlobalProperties r)) i)
  -> Array (Tuple String (HTML p i))
  -> HTML p i

article :: forall p i. KeyedNode () p i
article = keyed (ElemName "article")

article_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
article_ = article []

colgroup :: forall p i. KeyedNode (span :: I) p i
colgroup = keyed (ElemName "colgroup")

colgroup_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
colgroup_ = colgroup []

dialog :: forall p i. KeyedNode (open :: I) p i
dialog = keyed (ElemName "dialog")

dialog_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
dialog_ = dialog []

div :: forall p i. KeyedNode (onScroll :: I) p i
div = keyed (ElemName "div")

div_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
div_ = div []

dl :: forall p i. KeyedNode (onScroll :: I) p i
dl = keyed (ElemName "dl")

dl_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
dl_ = dl []

fieldset :: forall p i. KeyedNode (disabled :: I, form :: I, onScroll :: I) p i
fieldset = keyed (ElemName "fieldset")

fieldset_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
fieldset_ = fieldset []

footer :: forall p i. KeyedNode () p i
footer = keyed (ElemName "footer")

footer_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
footer_ = footer []

form :: forall p i. KeyedNode (acceptCharset :: I, action :: I, autocomplete :: I, enctype :: I, method :: I, onReset :: I, novalidate :: I, onScroll :: I, onSubmit :: I, target :: I) p i
form = keyed (ElemName "form")

form_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
form_ = form []

header :: forall p i. KeyedNode () p i
header = keyed (ElemName "header")

header_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
header_ = header []

menu :: forall p i. KeyedNode (label :: I, onScroll :: I, menuType :: I) p i
menu = keyed (ElemName "menu")

menu_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
menu_ = menu []

ol :: forall p i. KeyedNode (onScroll :: I, reversed :: I, start :: I, olType :: I) p i
ol = keyed (ElemName "ol")

ol_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
ol_ = ol []

table :: forall p i. KeyedNode (sortable :: I) p i
table = keyed (ElemName "table")

table_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
table_ = table []

tbody :: forall p i. KeyedNode (onScroll :: I) p i
tbody = keyed (ElemName "tbody")

tbody_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
tbody_ = tbody []

tfoot :: forall p i. KeyedNode (onScroll :: I) p i
tfoot = keyed (ElemName "tfoot")

tfoot_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
tfoot_ = tfoot []

thead :: forall p i. KeyedNode () p i
thead = keyed (ElemName "thead")

thead_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
thead_ = thead []

tr :: forall p i. KeyedNode () p i
tr = keyed (ElemName "tr")

tr_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
tr_ = tr []

ul :: forall p i. KeyedNode (onScroll :: I) p i
ul = keyed (ElemName "ul")

ul_ :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
ul_ = ul []
