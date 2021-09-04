module Halogen.HTML.Elements
  ( Node
  , Leaf
  , element
  , elementNS
  , keyed
  , keyedNS
  , withKeys
  , withKeys_
  , a
  , a_
  , abbr
  , abbr_
  , address
  , address_
  , area
  , article
  , article_
  , aside
  , aside_
  , audio
  , audio_
  , b
  , b_
  , base
  , bdi
  , bdi_
  , bdo
  , bdo_
  , blockquote
  , blockquote_
  , body
  , body_
  , br
  , br_
  , button
  , button_
  , canvas
  , caption
  , caption_
  , cite
  , cite_
  , code
  , code_
  , col
  , colgroup
  , colgroup_
  , command
  , datalist
  , datalist_
  , dd
  , dd_
  , del
  , del_
  , details
  , details_
  , dfn
  , dfn_
  , dialog
  , dialog_
  , div
  , div_
  , dl
  , dl_
  , dt
  , dt_
  , em
  , em_
  , embed
  , embed_
  , fieldset
  , fieldset_
  , figcaption
  , figcaption_
  , figure
  , figure_
  , footer
  , footer_
  , form
  , form_
  , h1
  , h1_
  , h2
  , h2_
  , h3
  , h3_
  , h4
  , h4_
  , h5
  , h5_
  , h6
  , h6_
  , head
  , head_
  , header
  , header_
  , hr
  , hr_
  , html
  , html_
  , i
  , i_
  , iframe
  , img
  , input
  , ins
  , ins_
  , kbd
  , kbd_
  , label
  , label_
  , legend
  , legend_
  , li
  , li_
  , link
  , main
  , main_
  , map
  , map_
  , mark
  , mark_
  , menu
  , menu_
  , menuitem
  , menuitem_
  , meta
  , meter
  , meter_
  , nav
  , nav_
  , noscript
  , noscript_
  , object
  , object_
  , ol
  , ol_
  , optgroup
  , optgroup_
  , option
  , option_
  , output
  , output_
  , p
  , p_
  , param
  , pre
  , pre_
  , progress
  , progress_
  , q
  , q_
  , rp
  , rp_
  , rt
  , rt_
  , ruby
  , ruby_
  , samp
  , samp_
  , script
  , script_
  , section
  , section_
  , select
  , select_
  , small
  , small_
  , source
  , span
  , span_
  , strong
  , strong_
  , style
  , style_
  , sub
  , sub_
  , summary
  , summary_
  , sup
  , sup_
  , table
  , table_
  , tbody
  , tbody_
  , td
  , td_
  , textarea
  , tfoot
  , tfoot_
  , th
  , th_
  , thead
  , thead_
  , time
  , time_
  , title
  , title_
  , tr
  , tr_
  , track
  , u
  , u_
  , ul
  , ul_
  , var
  , var_
  , video
  , video_
  , wbr
  ) where

import Prelude ((#), (>>>), pure)
import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple)

import DOM.HTML.Indexed as I

import Halogen.HTML.Core (ElemName(..), HTML(..), Namespace, Prop)
import Halogen.HTML.Core as Core
import Halogen.HTML.Properties (IProp)
import Halogen.Query.Input (Input)
import Halogen.VDom as VDom

import Unsafe.Coerce (unsafeCoerce)

-- | An HTML element that admits children.
type Node r w i = Array (IProp r i) -> Array (HTML w i) -> HTML w i

-- | An HTML element that does not admit children.
type Leaf r w i = Array (IProp r i) -> HTML w i

-- | Creates an HTML element that expects indexed properties.
element :: forall r w i. ElemName -> Array (IProp r i) -> Array (HTML w i) -> HTML w i
element =
  Core.element Nothing #
    ( unsafeCoerce
        :: (ElemName -> Array (Prop i) -> Array (HTML w i) -> HTML w i)
        -> ElemName
        -> Array (IProp r i)
        -> Array (HTML w i)
        -> HTML w i
    )

-- | Creates a Namespaced HTML element that expects indexed properties.
elementNS :: forall r w i. Namespace -> ElemName -> Array (IProp r i) -> Array (HTML w i) -> HTML w i
elementNS =
  pure >>> Core.element >>>
    ( unsafeCoerce
        :: (ElemName -> Array (Prop i) -> Array (HTML w i) -> HTML w i)
        -> ElemName
        -> Array (IProp r i)
        -> Array (HTML w i)
        -> HTML w i
    )

-- | Creates an HTML element that expects indexed properties, with keyed
-- | children.
keyed :: forall r w i. ElemName -> Array (IProp r i) -> Array (Tuple String (HTML w i)) -> HTML w i
keyed =
  Core.keyed Nothing #
    ( unsafeCoerce
        :: (ElemName -> Array (Prop i) -> Array (Tuple String (HTML w i)) -> HTML w i)
        -> ElemName
        -> Array (IProp r i)
        -> Array (Tuple String (HTML w i))
        -> HTML w i
    )

-- | Creates a Namespaced HTML element that expects indexed properties, with
-- | keyed children.
keyedNS :: forall r w i. Namespace -> ElemName -> Array (IProp r i) -> Array (Tuple String (HTML w i)) -> HTML w i
keyedNS =
  pure >>> Core.keyed >>>
    ( unsafeCoerce
        :: (ElemName -> Array (Prop i) -> Array (Tuple String (HTML w i)) -> HTML w i)
        -> ElemName
        -> Array (IProp r i)
        -> Array (Tuple String (HTML w i))
        -> HTML w i
    )

withKeys :: forall r w i. (Array (IProp r i) -> Array (HTML w i) -> HTML w i) -> Array (IProp r i) -> Array (Tuple String (HTML w i)) -> HTML w i
withKeys ctor props children =
  case ctor props [] of
    HTML (VDom.Elem x y z _) -> HTML (VDom.Keyed x y z (coe children))
    h -> h
  where
  coe :: Array (Tuple String (HTML w i)) -> Array (Tuple String (VDom.VDom (Array (Prop (Input i))) w))
  coe = unsafeCoerce

withKeys_ :: forall w i. (Array (HTML w i) -> HTML w i) -> Array (Tuple String (HTML w i)) -> HTML w i
withKeys_ ctor children =
  case ctor [] of
    HTML (VDom.Elem x y z _) -> HTML (VDom.Keyed x y z (coe children))
    h -> h
  where
  coe :: Array (Tuple String (HTML w i)) -> Array (Tuple String (VDom.VDom (Array (Prop (Input i))) w))
  coe = unsafeCoerce

a :: forall w i. Node I.HTMLa w i
a = element (ElemName "a")

a_ :: forall w i. Array (HTML w i) -> HTML w i
a_ = a []

abbr :: forall w i. Node I.HTMLabbr w i
abbr = element (ElemName "abbr")

abbr_ :: forall w i. Array (HTML w i) -> HTML w i
abbr_ = abbr []

address :: forall w i. Node I.HTMLaddress w i
address = element (ElemName "address")

address_ :: forall w i. Array (HTML w i) -> HTML w i
address_ = address []

area :: forall w i. Leaf I.HTMLarea w i
area props = element (ElemName "area") props []

article :: forall w i. Node I.HTMLarticle w i
article = element (ElemName "article")

article_ :: forall w i. Array (HTML w i) -> HTML w i
article_ = article []

aside :: forall w i. Node I.HTMLaside w i
aside = element (ElemName "aside")

aside_ :: forall w i. Array (HTML w i) -> HTML w i
aside_ = aside []

audio :: forall w i. Node I.HTMLaudio w i
audio = element (ElemName "audio")

audio_ :: forall w i. Array (HTML w i) -> HTML w i
audio_ = audio []

b :: forall w i. Node I.HTMLb w i
b = element (ElemName "b")

b_ :: forall w i. Array (HTML w i) -> HTML w i
b_ = b []

base :: forall w i. Leaf I.HTMLbase w i
base props = element (ElemName "base") props []

bdi :: forall w i. Node I.HTMLbdi w i
bdi = element (ElemName "bdi")

bdi_ :: forall w i. Array (HTML w i) -> HTML w i
bdi_ = bdi []

bdo :: forall w i. Node I.HTMLbdo w i
bdo = element (ElemName "bdo")

bdo_ :: forall w i. Array (HTML w i) -> HTML w i
bdo_ = bdo []

blockquote :: forall w i. Node I.HTMLblockquote w i
blockquote = element (ElemName "blockquote")

blockquote_ :: forall w i. Array (HTML w i) -> HTML w i
blockquote_ = blockquote []

body :: forall w i. Node I.HTMLbody w i
body = element (ElemName "body")

body_ :: forall w i. Array (HTML w i) -> HTML w i
body_ = body []

br :: forall w i. Leaf I.HTMLbr w i
br props = element (ElemName "br") props []

br_ :: forall w i. HTML w i
br_ = br []

button :: forall w i. Node I.HTMLbutton w i
button = element (ElemName "button")

button_ :: forall w i. Array (HTML w i) -> HTML w i
button_ = button []

canvas :: forall w i. Leaf I.HTMLcanvas w i
canvas props = element (ElemName "canvas") props []

caption :: forall w i. Node I.HTMLcaption w i
caption = element (ElemName "caption")

caption_ :: forall w i. Array (HTML w i) -> HTML w i
caption_ = caption []

cite :: forall w i. Node I.HTMLcite w i
cite = element (ElemName "cite")

cite_ :: forall w i. Array (HTML w i) -> HTML w i
cite_ = cite []

code :: forall w i. Node I.HTMLcode w i
code = element (ElemName "code")

code_ :: forall w i. Array (HTML w i) -> HTML w i
code_ = code []

col :: forall w i. Leaf I.HTMLcol w i
col props = element (ElemName "col") props []

colgroup :: forall w i. Node I.HTMLcolgroup w i
colgroup = element (ElemName "colgroup")

colgroup_ :: forall w i. Array (HTML w i) -> HTML w i
colgroup_ = colgroup []

command :: forall w i. Leaf I.HTMLcommand w i
command props = element (ElemName "command") props []

datalist :: forall w i. Node I.HTMLdatalist w i
datalist = element (ElemName "datalist")

datalist_ :: forall w i. Array (HTML w i) -> HTML w i
datalist_ = datalist []

dd :: forall w i. Node I.HTMLdd w i
dd = element (ElemName "dd")

dd_ :: forall w i. Array (HTML w i) -> HTML w i
dd_ = dd []

del :: forall w i. Node I.HTMLdel w i
del = element (ElemName "del")

del_ :: forall w i. Array (HTML w i) -> HTML w i
del_ = del []

details :: forall w i. Node I.HTMLdetails w i
details = element (ElemName "details")

details_ :: forall w i. Array (HTML w i) -> HTML w i
details_ = details []

dfn :: forall w i. Node I.HTMLdfn w i
dfn = element (ElemName "dfn")

dfn_ :: forall w i. Array (HTML w i) -> HTML w i
dfn_ = dfn []

dialog :: forall w i. Node I.HTMLdialog w i
dialog = element (ElemName "dialog")

dialog_ :: forall w i. Array (HTML w i) -> HTML w i
dialog_ = dialog []

div :: forall w i. Node I.HTMLdiv w i
div = element (ElemName "div")

div_ :: forall w i. Array (HTML w i) -> HTML w i
div_ = div []

dl :: forall w i. Node I.HTMLdl w i
dl = element (ElemName "dl")

dl_ :: forall w i. Array (HTML w i) -> HTML w i
dl_ = dl []

dt :: forall w i. Node (I.HTMLdt) w i
dt = element (ElemName "dt")

dt_ :: forall w i. Array (HTML w i) -> HTML w i
dt_ = dt []

em :: forall w i. Node I.HTMLem w i
em = element (ElemName "em")

em_ :: forall w i. Array (HTML w i) -> HTML w i
em_ = em []

embed :: forall w i. Node I.HTMLembed w i
embed = element (ElemName "embed")

embed_ :: forall w i. Array (HTML w i) -> HTML w i
embed_ = embed []

fieldset :: forall w i. Node I.HTMLfieldset w i
fieldset = element (ElemName "fieldset")

fieldset_ :: forall w i. Array (HTML w i) -> HTML w i
fieldset_ = fieldset []

figcaption :: forall w i. Node I.HTMLfigcaption w i
figcaption = element (ElemName "figcaption")

figcaption_ :: forall w i. Array (HTML w i) -> HTML w i
figcaption_ = figcaption []

figure :: forall w i. Node I.HTMLfigure w i
figure = element (ElemName "figure")

figure_ :: forall w i. Array (HTML w i) -> HTML w i
figure_ = figure []

footer :: forall w i. Node I.HTMLfooter w i
footer = element (ElemName "footer")

footer_ :: forall w i. Array (HTML w i) -> HTML w i
footer_ = footer []

form :: forall w i. Node I.HTMLform w i
form = element (ElemName "form")

form_ :: forall w i. Array (HTML w i) -> HTML w i
form_ = form []

h1 :: forall w i. Node I.HTMLh1 w i
h1 = element (ElemName "h1")

h1_ :: forall w i. Array (HTML w i) -> HTML w i
h1_ = h1 []

h2 :: forall w i. Node I.HTMLh2 w i
h2 = element (ElemName "h2")

h2_ :: forall w i. Array (HTML w i) -> HTML w i
h2_ = h2 []

h3 :: forall w i. Node I.HTMLh3 w i
h3 = element (ElemName "h3")

h3_ :: forall w i. Array (HTML w i) -> HTML w i
h3_ = h3 []

h4 :: forall w i. Node I.HTMLh4 w i
h4 = element (ElemName "h4")

h4_ :: forall w i. Array (HTML w i) -> HTML w i
h4_ = h4 []

h5 :: forall w i. Node I.HTMLh5 w i
h5 = element (ElemName "h5")

h5_ :: forall w i. Array (HTML w i) -> HTML w i
h5_ = h5 []

h6 :: forall w i. Node I.HTMLh6 w i
h6 = element (ElemName "h6")

h6_ :: forall w i. Array (HTML w i) -> HTML w i
h6_ = h6 []

head :: forall w i. Node I.HTMLhead w i
head = element (ElemName "head")

head_ :: forall w i. Array (HTML w i) -> HTML w i
head_ = head []

header :: forall w i. Node I.HTMLheader w i
header = element (ElemName "header")

header_ :: forall w i. Array (HTML w i) -> HTML w i
header_ = header []

hr :: forall w i. Leaf I.HTMLhr w i
hr props = element (ElemName "hr") props []

hr_ :: forall w i. HTML w i
hr_ = hr []

html :: forall w i. Node I.HTMLhtml w i
html = element (ElemName "html")

html_ :: forall w i. Array (HTML w i) -> HTML w i
html_ = html []

i :: forall w i. Node I.HTMLi w i
i = element (ElemName "i")

i_ :: forall w i. Array (HTML w i) -> HTML w i
i_ = i []

iframe :: forall w i. Leaf I.HTMLiframe w i
iframe props = element (ElemName "iframe") props []

img :: forall w i. Leaf I.HTMLimg w i
img props = element (ElemName "img") props []

input :: forall w i. Leaf I.HTMLinput w i
input props = element (ElemName "input") props []

ins :: forall w i. Node I.HTMLins w i
ins = element (ElemName "ins")

ins_ :: forall w i. Array (HTML w i) -> HTML w i
ins_ = ins []

kbd :: forall w i. Node I.HTMLkbd w i
kbd = element (ElemName "kbd")

kbd_ :: forall w i. Array (HTML w i) -> HTML w i
kbd_ = kbd []

label :: forall w i. Node I.HTMLlabel w i
label = element (ElemName "label")

label_ :: forall w i. Array (HTML w i) -> HTML w i
label_ = label []

legend :: forall w i. Node I.HTMLlegend w i
legend = element (ElemName "legend")

legend_ :: forall w i. Array (HTML w i) -> HTML w i
legend_ = legend []

li :: forall w i. Node I.HTMLli w i
li = element (ElemName "li")

li_ :: forall w i. Array (HTML w i) -> HTML w i
li_ = li []

link :: forall w i. Leaf I.HTMLlink w i
link props = element (ElemName "link") props []

main :: forall w i. Node I.HTMLmain w i
main = element (ElemName "main")

main_ :: forall w i. Array (HTML w i) -> HTML w i
main_ = main []

map :: forall w i. Node I.HTMLmap w i
map = element (ElemName "map")

map_ :: forall w i. Array (HTML w i) -> HTML w i
map_ = map []

mark :: forall w i. Node I.HTMLmark w i
mark = element (ElemName "mark")

mark_ :: forall w i. Array (HTML w i) -> HTML w i
mark_ = mark []

menu :: forall w i. Node I.HTMLmenu w i
menu = element (ElemName "menu")

menu_ :: forall w i. Array (HTML w i) -> HTML w i
menu_ = menu []

menuitem :: forall w i. Node I.HTMLmenuitem w i
menuitem = element (ElemName "menuitem")

menuitem_ :: forall w i. Array (HTML w i) -> HTML w i
menuitem_ = menuitem []

meta :: forall w i. Leaf I.HTMLmeta w i
meta props = element (ElemName "meta") props []

meter :: forall w i. Node I.HTMLmeter w i
meter = element (ElemName "meter")

meter_ :: forall w i. Array (HTML w i) -> HTML w i
meter_ = meter []

nav :: forall w i. Node I.HTMLnav w i
nav = element (ElemName "nav")

nav_ :: forall w i. Array (HTML w i) -> HTML w i
nav_ = nav []

noscript :: forall w i. Node I.HTMLnoscript w i
noscript = element (ElemName "noscript")

noscript_ :: forall w i. Array (HTML w i) -> HTML w i
noscript_ = noscript []

object :: forall w i. Node I.HTMLobject w i
object = element (ElemName "object")

object_ :: forall w i. Array (HTML w i) -> HTML w i
object_ = object []

ol :: forall w i. Node I.HTMLol w i
ol = element (ElemName "ol")

ol_ :: forall w i. Array (HTML w i) -> HTML w i
ol_ = ol []

optgroup :: forall w i. Node I.HTMLoptgroup w i
optgroup = element (ElemName "optgroup")

optgroup_ :: forall w i. Array (HTML w i) -> HTML w i
optgroup_ = optgroup []

option :: forall w i. Node I.HTMLoption w i
option = element (ElemName "option")

option_ :: forall w i. Array (HTML w i) -> HTML w i
option_ = option []

output :: forall w i. Node I.HTMLoutput w i
output = element (ElemName "output")

output_ :: forall w i. Array (HTML w i) -> HTML w i
output_ = output []

p :: forall w i. Node I.HTMLp w i
p = element (ElemName "p")

p_ :: forall w i. Array (HTML w i) -> HTML w i
p_ = p []

param :: forall w i. Leaf I.HTMLparam w i
param props = element (ElemName "param") props []

pre :: forall w i. Node I.HTMLpre w i
pre = element (ElemName "pre")

pre_ :: forall w i. Array (HTML w i) -> HTML w i
pre_ = pre []

progress :: forall w i. Node I.HTMLprogress w i
progress = element (ElemName "progress")

progress_ :: forall w i. Array (HTML w i) -> HTML w i
progress_ = progress []

q :: forall w i. Node I.HTMLq w i
q = element (ElemName "q")

q_ :: forall w i. Array (HTML w i) -> HTML w i
q_ = q []

rp :: forall w i. Node I.HTMLrp w i
rp = element (ElemName "rp")

rp_ :: forall w i. Array (HTML w i) -> HTML w i
rp_ = rp []

rt :: forall w i. Node I.HTMLrt w i
rt = element (ElemName "rt")

rt_ :: forall w i. Array (HTML w i) -> HTML w i
rt_ = rt []

ruby :: forall w i. Node I.HTMLruby w i
ruby = element (ElemName "ruby")

ruby_ :: forall w i. Array (HTML w i) -> HTML w i
ruby_ = ruby []

samp :: forall w i. Node I.HTMLsamp w i
samp = element (ElemName "samp")

samp_ :: forall w i. Array (HTML w i) -> HTML w i
samp_ = samp []

script :: forall w i. Node I.HTMLscript w i
script = element (ElemName "script")

script_ :: forall w i. Array (HTML w i) -> HTML w i
script_ = script []

section :: forall w i. Node I.HTMLsection w i
section = element (ElemName "section")

section_ :: forall w i. Array (HTML w i) -> HTML w i
section_ = section []

select :: forall w i. Node I.HTMLselect w i
select = element (ElemName "select")

select_ :: forall w i. Array (HTML w i) -> HTML w i
select_ = select []

small :: forall w i. Node I.HTMLsmall w i
small = element (ElemName "small")

small_ :: forall w i. Array (HTML w i) -> HTML w i
small_ = small []

source :: forall w i. Leaf I.HTMLsource w i
source props = element (ElemName "source") props []

span :: forall w i. Node I.HTMLspan w i
span = element (ElemName "span")

span_ :: forall w i. Array (HTML w i) -> HTML w i
span_ = span []

strong :: forall w i. Node I.HTMLstrong w i
strong = element (ElemName "strong")

strong_ :: forall w i. Array (HTML w i) -> HTML w i
strong_ = strong []

style :: forall w i. Node I.HTMLstyle w i
style = element (ElemName "style")

style_ :: forall w i. Array (HTML w i) -> HTML w i
style_ = style []

sub :: forall w i. Node I.HTMLsub w i
sub = element (ElemName "sub")

sub_ :: forall w i. Array (HTML w i) -> HTML w i
sub_ = sub []

summary :: forall w i. Node I.HTMLsummary w i
summary = element (ElemName "summary")

summary_ :: forall w i. Array (HTML w i) -> HTML w i
summary_ = summary []

sup :: forall w i. Node I.HTMLsup w i
sup = element (ElemName "sup")

sup_ :: forall w i. Array (HTML w i) -> HTML w i
sup_ = sup []

table :: forall w i. Node I.HTMLtable w i
table = element (ElemName "table")

table_ :: forall w i. Array (HTML w i) -> HTML w i
table_ = table []

tbody :: forall w i. Node I.HTMLtbody w i
tbody = element (ElemName "tbody")

tbody_ :: forall w i. Array (HTML w i) -> HTML w i
tbody_ = tbody []

td :: forall w i. Node I.HTMLtd w i
td = element (ElemName "td")

td_ :: forall w i. Array (HTML w i) -> HTML w i
td_ = td []

textarea :: forall w i. Leaf I.HTMLtextarea w i
textarea es = element (ElemName "textarea") es []

tfoot :: forall w i. Node I.HTMLtfoot w i
tfoot = element (ElemName "tfoot")

tfoot_ :: forall w i. Array (HTML w i) -> HTML w i
tfoot_ = tfoot []

th :: forall w i. Node I.HTMLth w i
th = element (ElemName "th")

th_ :: forall w i. Array (HTML w i) -> HTML w i
th_ = th []

thead :: forall w i. Node I.HTMLthead w i
thead = element (ElemName "thead")

thead_ :: forall w i. Array (HTML w i) -> HTML w i
thead_ = thead []

time :: forall w i. Node I.HTMLtime w i
time = element (ElemName "time")

time_ :: forall w i. Array (HTML w i) -> HTML w i
time_ = time []

title :: forall w i. Node I.HTMLtitle w i
title = element (ElemName "title")

title_ :: forall w i. Array (HTML w i) -> HTML w i
title_ = title []

tr :: forall w i. Node I.HTMLtr w i
tr = element (ElemName "tr")

tr_ :: forall w i. Array (HTML w i) -> HTML w i
tr_ = tr []

track :: forall w i. Leaf I.HTMLtrack w i
track props = element (ElemName "track") props []

u :: forall w i. Node I.HTMLu w i
u = element (ElemName "u")

u_ :: forall w i. Array (HTML w i) -> HTML w i
u_ = u []

ul :: forall w i. Node I.HTMLul w i
ul = element (ElemName "ul")

ul_ :: forall w i. Array (HTML w i) -> HTML w i
ul_ = ul []

var :: forall w i. Node I.HTMLvar w i
var = element (ElemName "var")

var_ :: forall w i. Array (HTML w i) -> HTML w i
var_ = var []

video :: forall w i. Node I.HTMLvideo w i
video = element (ElemName "video")

video_ :: forall w i. Array (HTML w i) -> HTML w i
video_ = video []

wbr :: forall w i. Leaf I.HTMLwbr w i
wbr props = element (ElemName "wbr") props []
