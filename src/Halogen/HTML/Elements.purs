-- | Smart constructors for HTML5 elements.
module Halogen.HTML.Elements
  ( text
  , a             , a_
  , abbr          , abbr_
  , acronym       , acronym_
  , address       , address_
  , applet        , applet_
  , area
  , article       , article_
  , aside         , aside_
  , audio         , audio_
  , b             , b_
  , base
  , basefont      , basefont_
  , bdi           , bdi_
  , bdo           , bdo_
  , big           , big_
  , blockquote    , blockquote_
  , body          , body_
  , br
  , button        , button_
  , canvas
  , caption       , caption_
  , center        , center_
  , cite          , cite_
  , code          , code_
  , col
  , colgroup      , colgroup_
  , command
  , datalist      , datalist_
  , dd            , dd_
  , del           , del_
  , details       , details_
  , dfn           , dfn_
  , dialog        , dialog_
  , dir           , dir_
  , div           , div_
  , dl            , dl_
  , dt            , dt_
  , em            , em_
  , embed         , embed_
  , fieldset      , fieldset_
  , figcaption    , figcaption_
  , figure        , figure_
  , font          , font_
  , footer        , footer_
  , form          , form_
  , frame         , frame_
  , frameset      , frameset_
  , h1            , h1_
  , h2            , h2_
  , h3            , h3_
  , h4            , h4_
  , h5            , h5_
  , h6            , h6_
  , head          , head_
  , header        , header_
  , hr
  , html          , html_
  , i             , i_
  , iframe
  , img
  , input
  , ins           , ins_
  , kbd           , kbd_
  , keygen
  , label         , label_
  , legend        , legend_
  , li            , li_
  , link
  , main          , main_
  , map           , map_
  , mark          , mark_
  , menu          , menu_
  , menuitem      , menuitem_
  , meta
  , meter         , meter_
  , nav           , nav_
  , noframes      , noframes_
  , noscript      , noscript_
  , object        , object_
  , ol            , ol_
  , optgroup      , optgroup_
  , option        , option_
  , output        , output_
  , p             , p_
  , param
  , pre           , pre_
  , progress      , progress_
  , q             , q_
  , rp            , rp_
  , rt            , rt_
  , ruby          , ruby_
  , s             , s_
  , samp          , samp_
  , script        , script_
  , section       , section_
  , select        , select_
  , small         , small_
  , source
  , span          , span_
  , strike        , strike_
  , strong        , strong_
  , style         , style_
  , sub           , sub_
  , summary       , summary_
  , sup           , sup_
  , table         , table_
  , tbody         , tbody_
  , td            , td_
  , textarea      , textarea_
  , tfoot         , tfoot_
  , th            , th_
  , thead         , thead_
  , time          , time_
  , title         , title_
  , tr            , tr_
  , track
  , tt            , tt_
  , u             , u_
  , ul            , ul_
  , var           , var_
  , video         , video_
  , wbr
  ) where

import Prelude hiding (sub, div, map)
import Halogen.HTML.Core (HTML(..), Prop(), TagName(), tagName, element)

text :: forall p i. String -> HTML p i
text = Text

a :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
a xs = element (tagName "a") xs

a_ :: forall p i. Array (HTML p i) -> HTML p i
a_ = a []

abbr :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
abbr xs = element (tagName "abbr") xs

abbr_ :: forall p i. Array (HTML p i) -> HTML p i
abbr_ = abbr []

acronym :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
acronym xs = element (tagName "acronym") xs

acronym_ :: forall p i. Array (HTML p i) -> HTML p i
acronym_ = acronym []

address :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
address xs = element (tagName "address") xs

address_ :: forall p i. Array (HTML p i) -> HTML p i
address_ = address []

applet :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
applet xs = element (tagName "applet") xs

applet_ :: forall p i. Array (HTML p i) -> HTML p i
applet_ = applet []

area :: forall p i. Array (Prop i) -> HTML p i
area props = element (tagName "area") props []

article :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
article xs = element (tagName "article") xs

article_ :: forall p i. Array (HTML p i) -> HTML p i
article_ = article []

aside :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
aside xs = element (tagName "aside") xs

aside_ :: forall p i. Array (HTML p i) -> HTML p i
aside_ = aside []

audio :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
audio xs = element (tagName "audio") xs

audio_ :: forall p i. Array (HTML p i) -> HTML p i
audio_ = audio []

b :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
b xs = element (tagName "b") xs

b_ :: forall p i. Array (HTML p i) -> HTML p i
b_ = b []

base :: forall p i. Array (Prop i) -> HTML p i
base props = element (tagName "base") props []

basefont :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
basefont xs = element (tagName "basefont") xs

basefont_ :: forall p i. Array (HTML p i) -> HTML p i
basefont_ = basefont []

bdi :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
bdi xs = element (tagName "bdi") xs

bdi_ :: forall p i. Array (HTML p i) -> HTML p i
bdi_ = bdi []

bdo :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
bdo xs = element (tagName "bdo") xs

bdo_ :: forall p i. Array (HTML p i) -> HTML p i
bdo_ = bdo []

big :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
big xs = element (tagName "big") xs

big_ :: forall p i. Array (HTML p i) -> HTML p i
big_ = big []

blockquote :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
blockquote xs = element (tagName "blockquote") xs

blockquote_ :: forall p i. Array (HTML p i) -> HTML p i
blockquote_ = blockquote []

body :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
body xs = element (tagName "body") xs

body_ :: forall p i. Array (HTML p i) -> HTML p i
body_ = body []

br :: forall p i. Array (Prop i) -> HTML p i
br props = element (tagName "br") props []

button :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
button xs = element (tagName "button") xs

button_ :: forall p i. Array (HTML p i) -> HTML p i
button_ = button []

canvas :: forall p i. Array (Prop i) -> HTML p i
canvas props = element (tagName "canvas") props []

caption :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
caption xs = element (tagName "caption") xs

caption_ :: forall p i. Array (HTML p i) -> HTML p i
caption_ = caption []

center :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
center xs = element (tagName "center") xs

center_ :: forall p i. Array (HTML p i) -> HTML p i
center_ = center []

cite :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
cite xs = element (tagName "cite") xs

cite_ :: forall p i. Array (HTML p i) -> HTML p i
cite_ = cite []

code :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
code xs = element (tagName "code") xs

code_ :: forall p i. Array (HTML p i) -> HTML p i
code_ = code []

col :: forall p i. Array (Prop i) -> HTML p i
col props = element (tagName "col") props []

colgroup :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
colgroup xs = element (tagName "colgroup") xs

colgroup_ :: forall p i. Array (HTML p i) -> HTML p i
colgroup_ = colgroup []

command :: forall p i. Array (Prop i) -> HTML p i
command props = element (tagName "command") props []

datalist :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
datalist xs = element (tagName "datalist") xs

datalist_ :: forall p i. Array (HTML p i) -> HTML p i
datalist_ = datalist []

dd :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
dd xs = element (tagName "dd") xs

dd_ :: forall p i. Array (HTML p i) -> HTML p i
dd_ = dd []

del :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
del xs = element (tagName "del") xs

del_ :: forall p i. Array (HTML p i) -> HTML p i
del_ = del []

details :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
details xs = element (tagName "details") xs

details_ :: forall p i. Array (HTML p i) -> HTML p i
details_ = details []

dfn :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
dfn xs = element (tagName "dfn") xs

dfn_ :: forall p i. Array (HTML p i) -> HTML p i
dfn_ = dfn []

dialog :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
dialog xs = element (tagName "dialog") xs

dialog_ :: forall p i. Array (HTML p i) -> HTML p i
dialog_ = dialog []

dir :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
dir xs = element (tagName "dir") xs

dir_ :: forall p i. Array (HTML p i) -> HTML p i
dir_ = dir []

div :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
div xs = element (tagName "div") xs

div_ :: forall p i. Array (HTML p i) -> HTML p i
div_ = div []

dl :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
dl xs = element (tagName "dl") xs

dl_ :: forall p i. Array (HTML p i) -> HTML p i
dl_ = dl []

dt :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
dt xs = element (tagName "dt") xs

dt_ :: forall p i. Array (HTML p i) -> HTML p i
dt_ = dt []

em :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
em = element (tagName "em")

em_ :: forall p i. Array (HTML p i) -> HTML p i
em_ = em []

embed :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
embed xs = element (tagName "embed") xs

embed_ :: forall p i. Array (HTML p i) -> HTML p i
embed_ = embed []

fieldset :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
fieldset xs = element (tagName "fieldset") xs

fieldset_ :: forall p i. Array (HTML p i) -> HTML p i
fieldset_ = fieldset []

figcaption :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
figcaption xs = element (tagName "figcaption") xs

figcaption_ :: forall p i. Array (HTML p i) -> HTML p i
figcaption_ = figcaption []

figure :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
figure xs = element (tagName "figure") xs

figure_ :: forall p i. Array (HTML p i) -> HTML p i
figure_ = figure []

font :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
font xs = element (tagName "font") xs

font_ :: forall p i. Array (HTML p i) -> HTML p i
font_ = font []

footer :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
footer xs = element (tagName "footer") xs

footer_ :: forall p i. Array (HTML p i) -> HTML p i
footer_ = footer []

form :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
form xs = element (tagName "form") xs

form_ :: forall p i. Array (HTML p i) -> HTML p i
form_ = form []

frame :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
frame xs = element (tagName "frame") xs

frame_ :: forall p i. Array (HTML p i) -> HTML p i
frame_ = frame []

frameset :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
frameset xs = element (tagName "frameset") xs

frameset_ :: forall p i. Array (HTML p i) -> HTML p i
frameset_ = frameset []

h1 :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
h1 xs = element (tagName "h1") xs

h1_ :: forall p i. Array (HTML p i) -> HTML p i
h1_ = h1 []

h2 :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
h2 xs = element (tagName "h2") xs

h2_ :: forall p i. Array (HTML p i) -> HTML p i
h2_ = h2 []

h3 :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
h3 xs = element (tagName "h3") xs

h3_ :: forall p i. Array (HTML p i) -> HTML p i
h3_ = h3 []

h4 :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
h4 xs = element (tagName "h4") xs

h4_ :: forall p i. Array (HTML p i) -> HTML p i
h4_ = h4 []

h5 :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
h5 xs = element (tagName "h5") xs

h5_ :: forall p i. Array (HTML p i) -> HTML p i
h5_ = h5 []

h6 :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
h6 xs = element (tagName "h6") xs

h6_ :: forall p i. Array (HTML p i) -> HTML p i
h6_ = h6 []

head :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
head xs = element (tagName "head") xs

head_ :: forall p i. Array (HTML p i) -> HTML p i
head_ = head []

header :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
header xs = element (tagName "header") xs

header_ :: forall p i. Array (HTML p i) -> HTML p i
header_ = header []

hr :: forall p i. Array (Prop i) -> HTML p i
hr props = element (tagName "hr") props []

html :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
html xs = element (tagName "html") xs

html_ :: forall p i. Array (HTML p i) -> HTML p i
html_ = html []

i :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
i xs = element (tagName "i") xs

i_ :: forall p i. Array (HTML p i) -> HTML p i
i_ = i []

iframe :: forall p i. Array (Prop i) -> HTML p i
iframe props = element (tagName "iframe") props []

img :: forall p i. Array (Prop i) -> HTML p i
img props = element (tagName "img") props []

input :: forall p i. Array (Prop i) -> HTML p i
input props = element (tagName "input") props []

ins :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
ins xs = element (tagName "ins") xs

ins_ :: forall p i. Array (HTML p i) -> HTML p i
ins_ = ins []

kbd :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
kbd xs = element (tagName "kbd") xs

kbd_ :: forall p i. Array (HTML p i) -> HTML p i
kbd_ = kbd []

keygen :: forall p i. Array (Prop i) -> HTML p i
keygen props = element (tagName "keygen") props []

label :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
label xs = element (tagName "label") xs

label_ :: forall p i. Array (HTML p i) -> HTML p i
label_ = label []

legend :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
legend xs = element (tagName "legend") xs

legend_ :: forall p i. Array (HTML p i) -> HTML p i
legend_ = legend []

li :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
li xs = element (tagName "li") xs

li_ :: forall p i. Array (HTML p i) -> HTML p i
li_ = li []

link :: forall p i. Array (Prop i) -> HTML p i
link props = element (tagName "link") props []

main :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
main xs = element (tagName "main") xs

main_ :: forall p i. Array (HTML p i) -> HTML p i
main_ = main []

map :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
map xs = element (tagName "map") xs

map_ :: forall p i. Array (HTML p i) -> HTML p i
map_ = map []

mark :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
mark xs = element (tagName "mark") xs

mark_ :: forall p i. Array (HTML p i) -> HTML p i
mark_ = mark []

menu :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
menu xs = element (tagName "menu") xs

menu_ :: forall p i. Array (HTML p i) -> HTML p i
menu_ = menu []

menuitem :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
menuitem xs = element (tagName "menuitem") xs

menuitem_ :: forall p i. Array (HTML p i) -> HTML p i
menuitem_ = menuitem []

meta :: forall p i. Array (Prop i) -> HTML p i
meta props = element (tagName "meta") props []

meter :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
meter xs = element (tagName "meter") xs

meter_ :: forall p i. Array (HTML p i) -> HTML p i
meter_ = meter []

nav :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
nav xs = element (tagName "nav") xs

nav_ :: forall p i. Array (HTML p i) -> HTML p i
nav_ = nav []

noframes :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
noframes xs = element (tagName "noframes") xs

noframes_ :: forall p i. Array (HTML p i) -> HTML p i
noframes_ = noframes []

noscript :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
noscript xs = element (tagName "noscript") xs

noscript_ :: forall p i. Array (HTML p i) -> HTML p i
noscript_ = noscript []

object :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
object xs = element (tagName "object") xs

object_ :: forall p i. Array (HTML p i) -> HTML p i
object_ = object []

ol :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
ol xs = element (tagName "ol") xs

ol_ :: forall p i. Array (HTML p i) -> HTML p i
ol_ = ol []

optgroup :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
optgroup xs = element (tagName "optgroup") xs

optgroup_ :: forall p i. Array (HTML p i) -> HTML p i
optgroup_ = optgroup []

option :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
option xs = element (tagName "option") xs

option_ :: forall p i. Array (HTML p i) -> HTML p i
option_ = option []

output :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
output xs = element (tagName "output") xs

output_ :: forall p i. Array (HTML p i) -> HTML p i
output_ = output []

p :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
p xs = element (tagName "p") xs

p_ :: forall p i. Array (HTML p i) -> HTML p i
p_ = p []

param :: forall p i. Array (Prop i) -> HTML p i
param props = element (tagName "param") props []

pre :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
pre xs = element (tagName "pre") xs

pre_ :: forall p i. Array (HTML p i) -> HTML p i
pre_ = pre []

progress :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
progress xs = element (tagName "progress") xs

progress_ :: forall p i. Array (HTML p i) -> HTML p i
progress_ = progress []

q :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
q xs = element (tagName "q") xs

q_ :: forall p i. Array (HTML p i) -> HTML p i
q_ = q []

rp :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
rp xs = element (tagName "rp") xs

rp_ :: forall p i. Array (HTML p i) -> HTML p i
rp_ = rp []

rt :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
rt xs = element (tagName "rt") xs

rt_ :: forall p i. Array (HTML p i) -> HTML p i
rt_ = rt []

ruby :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
ruby xs = element (tagName "ruby") xs

ruby_ :: forall p i. Array (HTML p i) -> HTML p i
ruby_ = ruby []

s :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
s xs = element (tagName "s") xs

s_ :: forall p i. Array (HTML p i) -> HTML p i
s_ = s []

samp :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
samp xs = element (tagName "samp") xs

samp_ :: forall p i. Array (HTML p i) -> HTML p i
samp_ = samp []

script :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
script xs = element (tagName "script") xs

script_ :: forall p i. Array (HTML p i) -> HTML p i
script_ = script []

section :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
section xs = element (tagName "section") xs

section_ :: forall p i. Array (HTML p i) -> HTML p i
section_ = section []

select :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
select xs = element (tagName "select") xs

select_ :: forall p i. Array (HTML p i) -> HTML p i
select_ = select []

small :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
small xs = element (tagName "small") xs

small_ :: forall p i. Array (HTML p i) -> HTML p i
small_ = small []

source :: forall p i. Array (Prop i) -> HTML p i
source props = element (tagName "source") props []

span :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
span xs = element (tagName "span") xs

span_ :: forall p i. Array (HTML p i) -> HTML p i
span_ = span []

strike :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
strike xs = element (tagName "strike") xs

strike_ :: forall p i. Array (HTML p i) -> HTML p i
strike_ = strike []

strong :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
strong xs = element (tagName "strong") xs

strong_ :: forall p i. Array (HTML p i) -> HTML p i
strong_ = strong []

style :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
style xs = element (tagName "style") xs

style_ :: forall p i. Array (HTML p i) -> HTML p i
style_ = style []

sub :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
sub xs = element (tagName "sub") xs

sub_ :: forall p i. Array (HTML p i) -> HTML p i
sub_ = sub []

summary :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
summary xs = element (tagName "summary") xs

summary_ :: forall p i. Array (HTML p i) -> HTML p i
summary_ = summary []

sup :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
sup xs = element (tagName "sup") xs

sup_ :: forall p i. Array (HTML p i) -> HTML p i
sup_ = sup []

table :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
table xs = element (tagName "table") xs

table_ :: forall p i. Array (HTML p i) -> HTML p i
table_ = table []

tbody :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
tbody xs = element (tagName "tbody") xs

tbody_ :: forall p i. Array (HTML p i) -> HTML p i
tbody_ = tbody []

td :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
td xs = element (tagName "td") xs

td_ :: forall p i. Array (HTML p i) -> HTML p i
td_ = td []

textarea :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
textarea xs = element (tagName "textarea") xs

textarea_ :: forall p i. Array (HTML p i) -> HTML p i
textarea_ = textarea []

tfoot :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
tfoot xs = element (tagName "tfoot") xs

tfoot_ :: forall p i. Array (HTML p i) -> HTML p i
tfoot_ = tfoot []

th :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
th xs = element (tagName "th") xs

th_ :: forall p i. Array (HTML p i) -> HTML p i
th_ = th []

thead :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
thead xs = element (tagName "thead") xs

thead_ :: forall p i. Array (HTML p i) -> HTML p i
thead_ = thead []

time :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
time xs = element (tagName "time") xs

time_ :: forall p i. Array (HTML p i) -> HTML p i
time_ = time []

title :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
title xs = element (tagName "title") xs

title_ :: forall p i. Array (HTML p i) -> HTML p i
title_ = title []

tr :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
tr xs = element (tagName "tr") xs

tr_ :: forall p i. Array (HTML p i) -> HTML p i
tr_ = tr []

track :: forall p i. Array (Prop i) -> HTML p i
track props = element (tagName "track") props []

tt :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
tt xs = element (tagName "tt") xs

tt_ :: forall p i. Array (HTML p i) -> HTML p i
tt_ = tt []

u :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
u xs = element (tagName "u") xs

u_ :: forall p i. Array (HTML p i) -> HTML p i
u_ = u []

ul :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
ul xs = element (tagName "ul") xs

ul_ :: forall p i. Array (HTML p i) -> HTML p i
ul_ = ul []

var :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
var xs = element (tagName "var") xs

var_ :: forall p i. Array (HTML p i) -> HTML p i
var_ = var []

video :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
video xs = element (tagName "video") xs

video_ :: forall p i. Array (HTML p i) -> HTML p i
video_ = video []

wbr :: forall p i. Array (Prop i) -> HTML p i
wbr props = element (tagName "wbr") props []
