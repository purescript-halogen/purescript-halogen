-- | This module defines the HTML types required by the Halogen library, and provides
-- | smart constructors for HTML5 elements.

module Halogen.HTML
  ( HTML(..)
  
  , text
  , element
  
  , TagName()
  , tagName
  , runTagName
  
  -- Elements
  
  , a             , a_
  , abbr          , abbr_
  , acronym       , acronym_
  , address       , address_
  , applet        , applet_
  , area          , area_
  , article       , article_
  , aside         , aside_
  , audio         , audio_
  , b             , b_
  , base          , base_
  , basefont      , basefont_
  , bdi           , bdi_
  , bdo           , bdo_
  , big           , big_
  , blockquote    , blockquote_
  , body          , body_
  , br            , br_
  , button        , button_
  , canvas        , canvas_
  , caption       , caption_
  , center        , center_
  , cite          , cite_
  , code          , code_
  , col           , col_
  , colgroup      , colgroup_
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
  , hr            , hr_
  , html          , html_
  , i             , i_
  , iframe        , iframe_
  , img           , img_
  , input         , input_
  , ins           , ins_
  , kbd           , kbd_
  , keygen        , keygen_
  , label         , label_
  , legend        , legend_
  , li            , li_
  , link          , link_
  , main          , main_
  , map           , map_
  , mark          , mark_
  , menu          , menu_
  , menuitem      , menuitem_
  , meta          , meta_
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
  , param         , param_
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
  , source        , source_
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
  , track         , track_
  , tt            , tt_
  , u             , u_
  , ul            , ul_
  , var           , var_
  , video         , video_
  , wbr           , wbr_
  ) where

import Data.Void
import Data.Maybe
import Data.Tuple
import Data.Foreign
import Data.Function
import Data.Monoid
import Data.StrMap (StrMap())
import Data.String (joinWith)
import Data.Foldable (for_, foldMap)

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Control.Monad.ST

import Halogen.Internal.VirtualDOM

import qualified Halogen.HTML.Attributes as A

-- | A type-safe wrapper for a HTML tag name
newtype TagName = TagName String

-- | Create a tag name
tagName :: String -> TagName
tagName = TagName

-- | Unwrap a `TagName` to get the tag name as a `String`.
runTagName :: TagName -> String
runTagName (TagName s) = s

-- | An initial encoding of HTML nodes.
data HTML i
  = Text String
  | Element TagName [A.Attr i] [HTML i]

instance functorHTML :: Functor HTML where
  (<$>) f = go
    where 
    go (Text s) = Text s
    go (Element name attrs els) = Element name ((f <$>) <$> attrs) (go <$> els)

text :: forall i. String -> HTML i
text = Text

element :: forall i. TagName -> [A.Attr i] -> [HTML i] -> HTML i
element = Element

a :: forall i. [A.Attr i] -> [HTML i] -> HTML i
a xs = element (tagName "a") xs

a_ :: forall i. [HTML i] -> HTML i
a_ = a mempty

abbr :: forall i. [A.Attr i] -> [HTML i] -> HTML i
abbr xs = element (tagName "abbr") xs

abbr_ :: forall i. [HTML i] -> HTML i
abbr_ = abbr mempty

acronym :: forall i. [A.Attr i] -> [HTML i] -> HTML i
acronym xs = element (tagName "acronym") xs

acronym_ :: forall i. [HTML i] -> HTML i
acronym_ = acronym mempty

address :: forall i. [A.Attr i] -> [HTML i] -> HTML i
address xs = element (tagName "address") xs

address_ :: forall i. [HTML i] -> HTML i
address_ = address mempty

applet :: forall i. [A.Attr i] -> [HTML i] -> HTML i
applet xs = element (tagName "applet") xs

applet_ :: forall i. [HTML i] -> HTML i
applet_ = applet mempty

area :: forall i. [A.Attr i] -> [HTML i] -> HTML i
area xs = element (tagName "area") xs

area_ :: forall i. [HTML i] -> HTML i
area_ = area mempty

article :: forall i. [A.Attr i] -> [HTML i] -> HTML i
article xs = element (tagName "article") xs

article_ :: forall i. [HTML i] -> HTML i
article_ = article mempty

aside :: forall i. [A.Attr i] -> [HTML i] -> HTML i
aside xs = element (tagName "aside") xs

aside_ :: forall i. [HTML i] -> HTML i
aside_ = aside mempty

audio :: forall i. [A.Attr i] -> [HTML i] -> HTML i
audio xs = element (tagName "audio") xs

audio_ :: forall i. [HTML i] -> HTML i
audio_ = audio mempty

b :: forall i. [A.Attr i] -> [HTML i] -> HTML i
b xs = element (tagName "b") xs

b_ :: forall i. [HTML i] -> HTML i
b_ = b mempty

base :: forall i. [A.Attr i] -> [HTML i] -> HTML i
base xs = element (tagName "base") xs

base_ :: forall i. [HTML i] -> HTML i
base_ = base mempty

basefont :: forall i. [A.Attr i] -> [HTML i] -> HTML i
basefont xs = element (tagName "basefont") xs

basefont_ :: forall i. [HTML i] -> HTML i
basefont_ = basefont mempty

bdi :: forall i. [A.Attr i] -> [HTML i] -> HTML i
bdi xs = element (tagName "bdi") xs

bdi_ :: forall i. [HTML i] -> HTML i
bdi_ = bdi mempty

bdo :: forall i. [A.Attr i] -> [HTML i] -> HTML i
bdo xs = element (tagName "bdo") xs

bdo_ :: forall i. [HTML i] -> HTML i
bdo_ = bdo mempty

big :: forall i. [A.Attr i] -> [HTML i] -> HTML i
big xs = element (tagName "big") xs

big_ :: forall i. [HTML i] -> HTML i
big_ = big mempty

blockquote :: forall i. [A.Attr i] -> [HTML i] -> HTML i
blockquote xs = element (tagName "blockquote") xs

blockquote_ :: forall i. [HTML i] -> HTML i
blockquote_ = blockquote mempty

body :: forall i. [A.Attr i] -> [HTML i] -> HTML i
body xs = element (tagName "body") xs

body_ :: forall i. [HTML i] -> HTML i
body_ = body mempty

br :: forall i. [A.Attr i] -> [HTML i] -> HTML i
br xs = element (tagName "br") xs

br_ :: forall i. [HTML i] -> HTML i
br_ = br mempty

button :: forall i. [A.Attr i] -> [HTML i] -> HTML i
button xs = element (tagName "button") xs

button_ :: forall i. [HTML i] -> HTML i
button_ = button mempty

canvas :: forall i. [A.Attr i] -> [HTML i] -> HTML i
canvas xs = element (tagName "canvas") xs

canvas_ :: forall i. [HTML i] -> HTML i
canvas_ = canvas mempty

caption :: forall i. [A.Attr i] -> [HTML i] -> HTML i
caption xs = element (tagName "caption") xs

caption_ :: forall i. [HTML i] -> HTML i
caption_ = caption mempty

center :: forall i. [A.Attr i] -> [HTML i] -> HTML i
center xs = element (tagName "center") xs

center_ :: forall i. [HTML i] -> HTML i
center_ = center mempty

cite :: forall i. [A.Attr i] -> [HTML i] -> HTML i
cite xs = element (tagName "cite") xs

cite_ :: forall i. [HTML i] -> HTML i
cite_ = cite mempty

code :: forall i. [A.Attr i] -> [HTML i] -> HTML i
code xs = element (tagName "code") xs

code_ :: forall i. [HTML i] -> HTML i
code_ = code mempty

col :: forall i. [A.Attr i] -> [HTML i] -> HTML i
col xs = element (tagName "col") xs

col_ :: forall i. [HTML i] -> HTML i
col_ = col mempty

colgroup :: forall i. [A.Attr i] -> [HTML i] -> HTML i
colgroup xs = element (tagName "colgroup") xs

colgroup_ :: forall i. [HTML i] -> HTML i
colgroup_ = colgroup mempty

datalist :: forall i. [A.Attr i] -> [HTML i] -> HTML i
datalist xs = element (tagName "datalist") xs

datalist_ :: forall i. [HTML i] -> HTML i
datalist_ = datalist mempty

dd :: forall i. [A.Attr i] -> [HTML i] -> HTML i
dd xs = element (tagName "dd") xs

dd_ :: forall i. [HTML i] -> HTML i
dd_ = dd mempty

del :: forall i. [A.Attr i] -> [HTML i] -> HTML i
del xs = element (tagName "del") xs

del_ :: forall i. [HTML i] -> HTML i
del_ = del mempty

details :: forall i. [A.Attr i] -> [HTML i] -> HTML i
details xs = element (tagName "details") xs

details_ :: forall i. [HTML i] -> HTML i
details_ = details mempty

dfn :: forall i. [A.Attr i] -> [HTML i] -> HTML i
dfn xs = element (tagName "dfn") xs

dfn_ :: forall i. [HTML i] -> HTML i
dfn_ = dfn mempty

dialog :: forall i. [A.Attr i] -> [HTML i] -> HTML i
dialog xs = element (tagName "dialog") xs

dialog_ :: forall i. [HTML i] -> HTML i
dialog_ = dialog mempty

dir :: forall i. [A.Attr i] -> [HTML i] -> HTML i
dir xs = element (tagName "dir") xs

dir_ :: forall i. [HTML i] -> HTML i
dir_ = dir mempty

div :: forall i. [A.Attr i] -> [HTML i] -> HTML i
div xs = element (tagName "div") xs

div_ :: forall i. [HTML i] -> HTML i
div_ = div mempty

dl :: forall i. [A.Attr i] -> [HTML i] -> HTML i
dl xs = element (tagName "dl") xs

dl_ :: forall i. [HTML i] -> HTML i
dl_ = dl mempty

dt :: forall i. [A.Attr i] -> [HTML i] -> HTML i
dt xs = element (tagName "dt") xs

dt_ :: forall i. [HTML i] -> HTML i
dt_ = dt mempty

em :: forall i. [A.Attr i] -> [HTML i] -> HTML i
em = element (tagName "em")

em_ :: forall i. [HTML i] -> HTML i
em_ = em mempty

embed :: forall i. [A.Attr i] -> [HTML i] -> HTML i
embed xs = element (tagName "embed") xs

embed_ :: forall i. [HTML i] -> HTML i
embed_ = embed mempty

fieldset :: forall i. [A.Attr i] -> [HTML i] -> HTML i
fieldset xs = element (tagName "fieldset") xs

fieldset_ :: forall i. [HTML i] -> HTML i
fieldset_ = fieldset mempty

figcaption :: forall i. [A.Attr i] -> [HTML i] -> HTML i
figcaption xs = element (tagName "figcaption") xs

figcaption_ :: forall i. [HTML i] -> HTML i
figcaption_ = figcaption mempty

figure :: forall i. [A.Attr i] -> [HTML i] -> HTML i
figure xs = element (tagName "figure") xs

figure_ :: forall i. [HTML i] -> HTML i
figure_ = figure mempty

font :: forall i. [A.Attr i] -> [HTML i] -> HTML i
font xs = element (tagName "font") xs

font_ :: forall i. [HTML i] -> HTML i
font_ = font mempty

footer :: forall i. [A.Attr i] -> [HTML i] -> HTML i
footer xs = element (tagName "footer") xs

footer_ :: forall i. [HTML i] -> HTML i
footer_ = footer mempty

form :: forall i. [A.Attr i] -> [HTML i] -> HTML i
form xs = element (tagName "form") xs

form_ :: forall i. [HTML i] -> HTML i
form_ = form mempty

frame :: forall i. [A.Attr i] -> [HTML i] -> HTML i
frame xs = element (tagName "frame") xs

frame_ :: forall i. [HTML i] -> HTML i
frame_ = frame mempty

frameset :: forall i. [A.Attr i] -> [HTML i] -> HTML i
frameset xs = element (tagName "frameset") xs

frameset_ :: forall i. [HTML i] -> HTML i
frameset_ = frameset mempty

h1 :: forall i. [A.Attr i] -> [HTML i] -> HTML i
h1 xs = element (tagName "h1") xs

h1_ :: forall i. [HTML i] -> HTML i
h1_ = h1 mempty

h2 :: forall i. [A.Attr i] -> [HTML i] -> HTML i
h2 xs = element (tagName "h2") xs

h2_ :: forall i. [HTML i] -> HTML i
h2_ = h2 mempty

h3 :: forall i. [A.Attr i] -> [HTML i] -> HTML i
h3 xs = element (tagName "h3") xs

h3_ :: forall i. [HTML i] -> HTML i
h3_ = h3 mempty

h4 :: forall i. [A.Attr i] -> [HTML i] -> HTML i
h4 xs = element (tagName "h4") xs

h4_ :: forall i. [HTML i] -> HTML i
h4_ = h4 mempty

h5 :: forall i. [A.Attr i] -> [HTML i] -> HTML i
h5 xs = element (tagName "h5") xs

h5_ :: forall i. [HTML i] -> HTML i
h5_ = h5 mempty

h6 :: forall i. [A.Attr i] -> [HTML i] -> HTML i
h6 xs = element (tagName "h6") xs

h6_ :: forall i. [HTML i] -> HTML i
h6_ = h6 mempty

head :: forall i. [A.Attr i] -> [HTML i] -> HTML i
head xs = element (tagName "head") xs

head_ :: forall i. [HTML i] -> HTML i
head_ = head mempty

header :: forall i. [A.Attr i] -> [HTML i] -> HTML i
header xs = element (tagName "header") xs

header_ :: forall i. [HTML i] -> HTML i
header_ = header mempty

hr :: forall i. [A.Attr i] -> [HTML i] -> HTML i
hr xs = element (tagName "hr") xs

hr_ :: forall i. [HTML i] -> HTML i
hr_ = hr mempty

html :: forall i. [A.Attr i] -> [HTML i] -> HTML i
html xs = element (tagName "html") xs

html_ :: forall i. [HTML i] -> HTML i
html_ = html mempty

i :: forall i. [A.Attr i] -> [HTML i] -> HTML i
i xs = element (tagName "i") xs

i_ :: forall i. [HTML i] -> HTML i
i_ = i mempty

iframe :: forall i. [A.Attr i] -> [HTML i] -> HTML i
iframe xs = element (tagName "iframe") xs

iframe_ :: forall i. [HTML i] -> HTML i
iframe_ = iframe mempty

img :: forall i. [A.Attr i] -> [HTML i] -> HTML i
img xs = element (tagName "img") xs

img_ :: forall i. [HTML i] -> HTML i
img_ = img mempty

input :: forall i. [A.Attr i] -> [HTML i] -> HTML i
input xs = element (tagName "input") xs

input_ :: forall i. [HTML i] -> HTML i
input_ = input mempty

ins :: forall i. [A.Attr i] -> [HTML i] -> HTML i
ins xs = element (tagName "ins") xs

ins_ :: forall i. [HTML i] -> HTML i
ins_ = ins mempty

kbd :: forall i. [A.Attr i] -> [HTML i] -> HTML i
kbd xs = element (tagName "kbd") xs

kbd_ :: forall i. [HTML i] -> HTML i
kbd_ = kbd mempty

keygen :: forall i. [A.Attr i] -> [HTML i] -> HTML i
keygen xs = element (tagName "keygen") xs

keygen_ :: forall i. [HTML i] -> HTML i
keygen_ = keygen mempty

label :: forall i. [A.Attr i] -> [HTML i] -> HTML i
label xs = element (tagName "label") xs

label_ :: forall i. [HTML i] -> HTML i
label_ = label mempty

legend :: forall i. [A.Attr i] -> [HTML i] -> HTML i
legend xs = element (tagName "legend") xs

legend_ :: forall i. [HTML i] -> HTML i
legend_ = legend mempty

li :: forall i. [A.Attr i] -> [HTML i] -> HTML i
li xs = element (tagName "li") xs

li_ :: forall i. [HTML i] -> HTML i
li_ = li mempty

link :: forall i. [A.Attr i] -> [HTML i] -> HTML i
link xs = element (tagName "link") xs

link_ :: forall i. [HTML i] -> HTML i
link_ = link mempty

main :: forall i. [A.Attr i] -> [HTML i] -> HTML i
main xs = element (tagName "main") xs

main_ :: forall i. [HTML i] -> HTML i
main_ = main mempty

map :: forall i. [A.Attr i] -> [HTML i] -> HTML i
map xs = element (tagName "map") xs

map_ :: forall i. [HTML i] -> HTML i
map_ = map mempty

mark :: forall i. [A.Attr i] -> [HTML i] -> HTML i
mark xs = element (tagName "mark") xs

mark_ :: forall i. [HTML i] -> HTML i
mark_ = mark mempty

menu :: forall i. [A.Attr i] -> [HTML i] -> HTML i
menu xs = element (tagName "menu") xs

menu_ :: forall i. [HTML i] -> HTML i
menu_ = menu mempty

menuitem :: forall i. [A.Attr i] -> [HTML i] -> HTML i
menuitem xs = element (tagName "menuitem") xs

menuitem_ :: forall i. [HTML i] -> HTML i
menuitem_ = menuitem mempty

meta :: forall i. [A.Attr i] -> [HTML i] -> HTML i
meta xs = element (tagName "meta") xs

meta_ :: forall i. [HTML i] -> HTML i
meta_ = meta mempty

meter :: forall i. [A.Attr i] -> [HTML i] -> HTML i
meter xs = element (tagName "meter") xs

meter_ :: forall i. [HTML i] -> HTML i
meter_ = meter mempty

nav :: forall i. [A.Attr i] -> [HTML i] -> HTML i
nav xs = element (tagName "nav") xs

nav_ :: forall i. [HTML i] -> HTML i
nav_ = nav mempty

noframes :: forall i. [A.Attr i] -> [HTML i] -> HTML i
noframes xs = element (tagName "noframes") xs

noframes_ :: forall i. [HTML i] -> HTML i
noframes_ = noframes mempty

noscript :: forall i. [A.Attr i] -> [HTML i] -> HTML i
noscript xs = element (tagName "noscript") xs

noscript_ :: forall i. [HTML i] -> HTML i
noscript_ = noscript mempty

object :: forall i. [A.Attr i] -> [HTML i] -> HTML i
object xs = element (tagName "object") xs

object_ :: forall i. [HTML i] -> HTML i
object_ = object mempty

ol :: forall i. [A.Attr i] -> [HTML i] -> HTML i
ol xs = element (tagName "ol") xs

ol_ :: forall i. [HTML i] -> HTML i
ol_ = ol mempty

optgroup :: forall i. [A.Attr i] -> [HTML i] -> HTML i
optgroup xs = element (tagName "optgroup") xs

optgroup_ :: forall i. [HTML i] -> HTML i
optgroup_ = optgroup mempty

option :: forall i. [A.Attr i] -> [HTML i] -> HTML i
option xs = element (tagName "option") xs

option_ :: forall i. [HTML i] -> HTML i
option_ = option mempty

output :: forall i. [A.Attr i] -> [HTML i] -> HTML i
output xs = element (tagName "output") xs

output_ :: forall i. [HTML i] -> HTML i
output_ = output mempty

p :: forall i. [A.Attr i] -> [HTML i] -> HTML i
p xs = element (tagName "p") xs

p_ :: forall i. [HTML i] -> HTML i
p_ = p mempty

param :: forall i. [A.Attr i] -> [HTML i] -> HTML i
param xs = element (tagName "param") xs

param_ :: forall i. [HTML i] -> HTML i
param_ = param mempty

pre :: forall i. [A.Attr i] -> [HTML i] -> HTML i
pre xs = element (tagName "pre") xs

pre_ :: forall i. [HTML i] -> HTML i
pre_ = pre mempty

progress :: forall i. [A.Attr i] -> [HTML i] -> HTML i
progress xs = element (tagName "progress") xs

progress_ :: forall i. [HTML i] -> HTML i
progress_ = progress mempty

q :: forall i. [A.Attr i] -> [HTML i] -> HTML i
q xs = element (tagName "q") xs

q_ :: forall i. [HTML i] -> HTML i
q_ = q mempty

rp :: forall i. [A.Attr i] -> [HTML i] -> HTML i
rp xs = element (tagName "rp") xs

rp_ :: forall i. [HTML i] -> HTML i
rp_ = rp mempty

rt :: forall i. [A.Attr i] -> [HTML i] -> HTML i
rt xs = element (tagName "rt") xs

rt_ :: forall i. [HTML i] -> HTML i
rt_ = rt mempty

ruby :: forall i. [A.Attr i] -> [HTML i] -> HTML i
ruby xs = element (tagName "ruby") xs

ruby_ :: forall i. [HTML i] -> HTML i
ruby_ = ruby mempty

s :: forall i. [A.Attr i] -> [HTML i] -> HTML i
s xs = element (tagName "s") xs

s_ :: forall i. [HTML i] -> HTML i
s_ = s mempty

samp :: forall i. [A.Attr i] -> [HTML i] -> HTML i
samp xs = element (tagName "samp") xs

samp_ :: forall i. [HTML i] -> HTML i
samp_ = samp mempty

script :: forall i. [A.Attr i] -> [HTML i] -> HTML i
script xs = element (tagName "script") xs

script_ :: forall i. [HTML i] -> HTML i
script_ = script mempty

section :: forall i. [A.Attr i] -> [HTML i] -> HTML i
section xs = element (tagName "section") xs

section_ :: forall i. [HTML i] -> HTML i
section_ = section mempty

select :: forall i. [A.Attr i] -> [HTML i] -> HTML i
select xs = element (tagName "select") xs

select_ :: forall i. [HTML i] -> HTML i
select_ = select mempty

small :: forall i. [A.Attr i] -> [HTML i] -> HTML i
small xs = element (tagName "small") xs

small_ :: forall i. [HTML i] -> HTML i
small_ = small mempty

source :: forall i. [A.Attr i] -> [HTML i] -> HTML i
source xs = element (tagName "source") xs

source_ :: forall i. [HTML i] -> HTML i
source_ = source mempty

span :: forall i. [A.Attr i] -> [HTML i] -> HTML i
span xs = element (tagName "span") xs

span_ :: forall i. [HTML i] -> HTML i
span_ = span mempty

strike :: forall i. [A.Attr i] -> [HTML i] -> HTML i
strike xs = element (tagName "strike") xs

strike_ :: forall i. [HTML i] -> HTML i
strike_ = strike mempty

strong :: forall i. [A.Attr i] -> [HTML i] -> HTML i
strong xs = element (tagName "strong") xs

strong_ :: forall i. [HTML i] -> HTML i
strong_ = strong mempty

style :: forall i. [A.Attr i] -> [HTML i] -> HTML i
style xs = element (tagName "style") xs

style_ :: forall i. [HTML i] -> HTML i
style_ = style mempty

sub :: forall i. [A.Attr i] -> [HTML i] -> HTML i
sub xs = element (tagName "sub") xs

sub_ :: forall i. [HTML i] -> HTML i
sub_ = sub mempty

summary :: forall i. [A.Attr i] -> [HTML i] -> HTML i
summary xs = element (tagName "summary") xs

summary_ :: forall i. [HTML i] -> HTML i
summary_ = summary mempty

sup :: forall i. [A.Attr i] -> [HTML i] -> HTML i
sup xs = element (tagName "sup") xs

sup_ :: forall i. [HTML i] -> HTML i
sup_ = sup mempty

table :: forall i. [A.Attr i] -> [HTML i] -> HTML i
table xs = element (tagName "table") xs

table_ :: forall i. [HTML i] -> HTML i
table_ = table mempty

tbody :: forall i. [A.Attr i] -> [HTML i] -> HTML i
tbody xs = element (tagName "tbody") xs

tbody_ :: forall i. [HTML i] -> HTML i
tbody_ = tbody mempty

td :: forall i. [A.Attr i] -> [HTML i] -> HTML i
td xs = element (tagName "td") xs

td_ :: forall i. [HTML i] -> HTML i
td_ = td mempty

textarea :: forall i. [A.Attr i] -> [HTML i] -> HTML i
textarea xs = element (tagName "textarea") xs

textarea_ :: forall i. [HTML i] -> HTML i
textarea_ = textarea mempty

tfoot :: forall i. [A.Attr i] -> [HTML i] -> HTML i
tfoot xs = element (tagName "tfoot") xs

tfoot_ :: forall i. [HTML i] -> HTML i
tfoot_ = tfoot mempty

th :: forall i. [A.Attr i] -> [HTML i] -> HTML i
th xs = element (tagName "th") xs

th_ :: forall i. [HTML i] -> HTML i
th_ = th mempty

thead :: forall i. [A.Attr i] -> [HTML i] -> HTML i
thead xs = element (tagName "thead") xs

thead_ :: forall i. [HTML i] -> HTML i
thead_ = thead mempty

time :: forall i. [A.Attr i] -> [HTML i] -> HTML i
time xs = element (tagName "time") xs

time_ :: forall i. [HTML i] -> HTML i
time_ = time mempty

title :: forall i. [A.Attr i] -> [HTML i] -> HTML i
title xs = element (tagName "title") xs

title_ :: forall i. [HTML i] -> HTML i
title_ = title mempty

tr :: forall i. [A.Attr i] -> [HTML i] -> HTML i
tr xs = element (tagName "tr") xs

tr_ :: forall i. [HTML i] -> HTML i
tr_ = tr mempty

track :: forall i. [A.Attr i] -> [HTML i] -> HTML i
track xs = element (tagName "track") xs

track_ :: forall i. [HTML i] -> HTML i
track_ = track mempty

tt :: forall i. [A.Attr i] -> [HTML i] -> HTML i
tt xs = element (tagName "tt") xs

tt_ :: forall i. [HTML i] -> HTML i
tt_ = tt mempty

u :: forall i. [A.Attr i] -> [HTML i] -> HTML i
u xs = element (tagName "u") xs

u_ :: forall i. [HTML i] -> HTML i
u_ = u mempty

ul :: forall i. [A.Attr i] -> [HTML i] -> HTML i
ul xs = element (tagName "ul") xs

ul_ :: forall i. [HTML i] -> HTML i
ul_ = ul mempty

var :: forall i. [A.Attr i] -> [HTML i] -> HTML i
var xs = element (tagName "var") xs

var_ :: forall i. [HTML i] -> HTML i
var_ = var mempty

video :: forall i. [A.Attr i] -> [HTML i] -> HTML i
video xs = element (tagName "video") xs

video_ :: forall i. [HTML i] -> HTML i
video_ = video mempty

wbr :: forall i. [A.Attr i] -> [HTML i] -> HTML i
wbr xs = element (tagName "wbr") xs

wbr_ :: forall i. [HTML i] -> HTML i
wbr_ = wbr mempty
