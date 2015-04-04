-- | This module defines the HTML types required by the Halogen library, and provides
-- | smart constructors for HTML5 elements.

module Halogen.HTML
  ( HTML(..)
  
  , text
  , placeholder
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
import Data.Bifunctor

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
data HTML p i
  = Text String
  | Element TagName [A.Attr i] [HTML p i]
  | Placeholder p

instance bifunctorHTML :: Bifunctor HTML where
  bimap f g = go
    where 
    go (Text s) = Text s
    go (Element name attrs els) = Element name ((g <$>) <$> attrs) (go <$> els)
    go (Placeholder p) = Placeholder (f p)

instance functorHTML :: Functor (HTML p) where
  (<$>) = rmap

text :: forall p i. String -> HTML p i
text = Text

element :: forall p i. TagName -> [A.Attr i] -> [HTML p i] -> HTML p i
element = Element

placeholder :: forall p i. p -> HTML p i
placeholder = Placeholder

-- | Replace placeholder nodes with HTML documents.
graft :: forall a b i. HTML a i -> (a -> HTML b i) -> HTML b i 
graft (Placeholder a) f = f a
graft (Element name attr els) f = Element name attr ((`graft` f) <$> els)
graft (Text s) _ = Text s

a :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
a xs = element (tagName "a") xs

a_ :: forall p i. [HTML p i] -> HTML p i
a_ = a mempty

abbr :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
abbr xs = element (tagName "abbr") xs

abbr_ :: forall p i. [HTML p i] -> HTML p i
abbr_ = abbr mempty

acronym :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
acronym xs = element (tagName "acronym") xs

acronym_ :: forall p i. [HTML p i] -> HTML p i
acronym_ = acronym mempty

address :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
address xs = element (tagName "address") xs

address_ :: forall p i. [HTML p i] -> HTML p i
address_ = address mempty

applet :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
applet xs = element (tagName "applet") xs

applet_ :: forall p i. [HTML p i] -> HTML p i
applet_ = applet mempty

area :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
area xs = element (tagName "area") xs

area_ :: forall p i. [HTML p i] -> HTML p i
area_ = area mempty

article :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
article xs = element (tagName "article") xs

article_ :: forall p i. [HTML p i] -> HTML p i
article_ = article mempty

aside :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
aside xs = element (tagName "aside") xs

aside_ :: forall p i. [HTML p i] -> HTML p i
aside_ = aside mempty

audio :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
audio xs = element (tagName "audio") xs

audio_ :: forall p i. [HTML p i] -> HTML p i
audio_ = audio mempty

b :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
b xs = element (tagName "b") xs

b_ :: forall p i. [HTML p i] -> HTML p i
b_ = b mempty

base :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
base xs = element (tagName "base") xs

base_ :: forall p i. [HTML p i] -> HTML p i
base_ = base mempty

basefont :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
basefont xs = element (tagName "basefont") xs

basefont_ :: forall p i. [HTML p i] -> HTML p i
basefont_ = basefont mempty

bdi :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
bdi xs = element (tagName "bdi") xs

bdi_ :: forall p i. [HTML p i] -> HTML p i
bdi_ = bdi mempty

bdo :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
bdo xs = element (tagName "bdo") xs

bdo_ :: forall p i. [HTML p i] -> HTML p i
bdo_ = bdo mempty

big :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
big xs = element (tagName "big") xs

big_ :: forall p i. [HTML p i] -> HTML p i
big_ = big mempty

blockquote :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
blockquote xs = element (tagName "blockquote") xs

blockquote_ :: forall p i. [HTML p i] -> HTML p i
blockquote_ = blockquote mempty

body :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
body xs = element (tagName "body") xs

body_ :: forall p i. [HTML p i] -> HTML p i
body_ = body mempty

br :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
br xs = element (tagName "br") xs

br_ :: forall p i. [HTML p i] -> HTML p i
br_ = br mempty

button :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
button xs = element (tagName "button") xs

button_ :: forall p i. [HTML p i] -> HTML p i
button_ = button mempty

canvas :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
canvas xs = element (tagName "canvas") xs

canvas_ :: forall p i. [HTML p i] -> HTML p i
canvas_ = canvas mempty

caption :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
caption xs = element (tagName "caption") xs

caption_ :: forall p i. [HTML p i] -> HTML p i
caption_ = caption mempty

center :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
center xs = element (tagName "center") xs

center_ :: forall p i. [HTML p i] -> HTML p i
center_ = center mempty

cite :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
cite xs = element (tagName "cite") xs

cite_ :: forall p i. [HTML p i] -> HTML p i
cite_ = cite mempty

code :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
code xs = element (tagName "code") xs

code_ :: forall p i. [HTML p i] -> HTML p i
code_ = code mempty

col :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
col xs = element (tagName "col") xs

col_ :: forall p i. [HTML p i] -> HTML p i
col_ = col mempty

colgroup :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
colgroup xs = element (tagName "colgroup") xs

colgroup_ :: forall p i. [HTML p i] -> HTML p i
colgroup_ = colgroup mempty

datalist :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
datalist xs = element (tagName "datalist") xs

datalist_ :: forall p i. [HTML p i] -> HTML p i
datalist_ = datalist mempty

dd :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
dd xs = element (tagName "dd") xs

dd_ :: forall p i. [HTML p i] -> HTML p i
dd_ = dd mempty

del :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
del xs = element (tagName "del") xs

del_ :: forall p i. [HTML p i] -> HTML p i
del_ = del mempty

details :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
details xs = element (tagName "details") xs

details_ :: forall p i. [HTML p i] -> HTML p i
details_ = details mempty

dfn :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
dfn xs = element (tagName "dfn") xs

dfn_ :: forall p i. [HTML p i] -> HTML p i
dfn_ = dfn mempty

dialog :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
dialog xs = element (tagName "dialog") xs

dialog_ :: forall p i. [HTML p i] -> HTML p i
dialog_ = dialog mempty

dir :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
dir xs = element (tagName "dir") xs

dir_ :: forall p i. [HTML p i] -> HTML p i
dir_ = dir mempty

div :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
div xs = element (tagName "div") xs

div_ :: forall p i. [HTML p i] -> HTML p i
div_ = div mempty

dl :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
dl xs = element (tagName "dl") xs

dl_ :: forall p i. [HTML p i] -> HTML p i
dl_ = dl mempty

dt :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
dt xs = element (tagName "dt") xs

dt_ :: forall p i. [HTML p i] -> HTML p i
dt_ = dt mempty

em :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
em = element (tagName "em")

em_ :: forall p i. [HTML p i] -> HTML p i
em_ = em mempty

embed :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
embed xs = element (tagName "embed") xs

embed_ :: forall p i. [HTML p i] -> HTML p i
embed_ = embed mempty

fieldset :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
fieldset xs = element (tagName "fieldset") xs

fieldset_ :: forall p i. [HTML p i] -> HTML p i
fieldset_ = fieldset mempty

figcaption :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
figcaption xs = element (tagName "figcaption") xs

figcaption_ :: forall p i. [HTML p i] -> HTML p i
figcaption_ = figcaption mempty

figure :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
figure xs = element (tagName "figure") xs

figure_ :: forall p i. [HTML p i] -> HTML p i
figure_ = figure mempty

font :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
font xs = element (tagName "font") xs

font_ :: forall p i. [HTML p i] -> HTML p i
font_ = font mempty

footer :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
footer xs = element (tagName "footer") xs

footer_ :: forall p i. [HTML p i] -> HTML p i
footer_ = footer mempty

form :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
form xs = element (tagName "form") xs

form_ :: forall p i. [HTML p i] -> HTML p i
form_ = form mempty

frame :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
frame xs = element (tagName "frame") xs

frame_ :: forall p i. [HTML p i] -> HTML p i
frame_ = frame mempty

frameset :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
frameset xs = element (tagName "frameset") xs

frameset_ :: forall p i. [HTML p i] -> HTML p i
frameset_ = frameset mempty

h1 :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
h1 xs = element (tagName "h1") xs

h1_ :: forall p i. [HTML p i] -> HTML p i
h1_ = h1 mempty

h2 :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
h2 xs = element (tagName "h2") xs

h2_ :: forall p i. [HTML p i] -> HTML p i
h2_ = h2 mempty

h3 :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
h3 xs = element (tagName "h3") xs

h3_ :: forall p i. [HTML p i] -> HTML p i
h3_ = h3 mempty

h4 :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
h4 xs = element (tagName "h4") xs

h4_ :: forall p i. [HTML p i] -> HTML p i
h4_ = h4 mempty

h5 :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
h5 xs = element (tagName "h5") xs

h5_ :: forall p i. [HTML p i] -> HTML p i
h5_ = h5 mempty

h6 :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
h6 xs = element (tagName "h6") xs

h6_ :: forall p i. [HTML p i] -> HTML p i
h6_ = h6 mempty

head :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
head xs = element (tagName "head") xs

head_ :: forall p i. [HTML p i] -> HTML p i
head_ = head mempty

header :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
header xs = element (tagName "header") xs

header_ :: forall p i. [HTML p i] -> HTML p i
header_ = header mempty

hr :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
hr xs = element (tagName "hr") xs

hr_ :: forall p i. [HTML p i] -> HTML p i
hr_ = hr mempty

html :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
html xs = element (tagName "html") xs

html_ :: forall p i. [HTML p i] -> HTML p i
html_ = html mempty

i :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
i xs = element (tagName "i") xs

i_ :: forall p i. [HTML p i] -> HTML p i
i_ = i mempty

iframe :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
iframe xs = element (tagName "iframe") xs

iframe_ :: forall p i. [HTML p i] -> HTML p i
iframe_ = iframe mempty

img :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
img xs = element (tagName "img") xs

img_ :: forall p i. [HTML p i] -> HTML p i
img_ = img mempty

input :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
input xs = element (tagName "input") xs

input_ :: forall p i. [HTML p i] -> HTML p i
input_ = input mempty

ins :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
ins xs = element (tagName "ins") xs

ins_ :: forall p i. [HTML p i] -> HTML p i
ins_ = ins mempty

kbd :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
kbd xs = element (tagName "kbd") xs

kbd_ :: forall p i. [HTML p i] -> HTML p i
kbd_ = kbd mempty

keygen :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
keygen xs = element (tagName "keygen") xs

keygen_ :: forall p i. [HTML p i] -> HTML p i
keygen_ = keygen mempty

label :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
label xs = element (tagName "label") xs

label_ :: forall p i. [HTML p i] -> HTML p i
label_ = label mempty

legend :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
legend xs = element (tagName "legend") xs

legend_ :: forall p i. [HTML p i] -> HTML p i
legend_ = legend mempty

li :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
li xs = element (tagName "li") xs

li_ :: forall p i. [HTML p i] -> HTML p i
li_ = li mempty

link :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
link xs = element (tagName "link") xs

link_ :: forall p i. [HTML p i] -> HTML p i
link_ = link mempty

main :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
main xs = element (tagName "main") xs

main_ :: forall p i. [HTML p i] -> HTML p i
main_ = main mempty

map :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
map xs = element (tagName "map") xs

map_ :: forall p i. [HTML p i] -> HTML p i
map_ = map mempty

mark :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
mark xs = element (tagName "mark") xs

mark_ :: forall p i. [HTML p i] -> HTML p i
mark_ = mark mempty

menu :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
menu xs = element (tagName "menu") xs

menu_ :: forall p i. [HTML p i] -> HTML p i
menu_ = menu mempty

menuitem :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
menuitem xs = element (tagName "menuitem") xs

menuitem_ :: forall p i. [HTML p i] -> HTML p i
menuitem_ = menuitem mempty

meta :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
meta xs = element (tagName "meta") xs

meta_ :: forall p i. [HTML p i] -> HTML p i
meta_ = meta mempty

meter :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
meter xs = element (tagName "meter") xs

meter_ :: forall p i. [HTML p i] -> HTML p i
meter_ = meter mempty

nav :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
nav xs = element (tagName "nav") xs

nav_ :: forall p i. [HTML p i] -> HTML p i
nav_ = nav mempty

noframes :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
noframes xs = element (tagName "noframes") xs

noframes_ :: forall p i. [HTML p i] -> HTML p i
noframes_ = noframes mempty

noscript :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
noscript xs = element (tagName "noscript") xs

noscript_ :: forall p i. [HTML p i] -> HTML p i
noscript_ = noscript mempty

object :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
object xs = element (tagName "object") xs

object_ :: forall p i. [HTML p i] -> HTML p i
object_ = object mempty

ol :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
ol xs = element (tagName "ol") xs

ol_ :: forall p i. [HTML p i] -> HTML p i
ol_ = ol mempty

optgroup :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
optgroup xs = element (tagName "optgroup") xs

optgroup_ :: forall p i. [HTML p i] -> HTML p i
optgroup_ = optgroup mempty

option :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
option xs = element (tagName "option") xs

option_ :: forall p i. [HTML p i] -> HTML p i
option_ = option mempty

output :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
output xs = element (tagName "output") xs

output_ :: forall p i. [HTML p i] -> HTML p i
output_ = output mempty

p :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
p xs = element (tagName "p") xs

p_ :: forall p i. [HTML p i] -> HTML p i
p_ = p mempty

param :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
param xs = element (tagName "param") xs

param_ :: forall p i. [HTML p i] -> HTML p i
param_ = param mempty

pre :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
pre xs = element (tagName "pre") xs

pre_ :: forall p i. [HTML p i] -> HTML p i
pre_ = pre mempty

progress :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
progress xs = element (tagName "progress") xs

progress_ :: forall p i. [HTML p i] -> HTML p i
progress_ = progress mempty

q :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
q xs = element (tagName "q") xs

q_ :: forall p i. [HTML p i] -> HTML p i
q_ = q mempty

rp :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
rp xs = element (tagName "rp") xs

rp_ :: forall p i. [HTML p i] -> HTML p i
rp_ = rp mempty

rt :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
rt xs = element (tagName "rt") xs

rt_ :: forall p i. [HTML p i] -> HTML p i
rt_ = rt mempty

ruby :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
ruby xs = element (tagName "ruby") xs

ruby_ :: forall p i. [HTML p i] -> HTML p i
ruby_ = ruby mempty

s :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
s xs = element (tagName "s") xs

s_ :: forall p i. [HTML p i] -> HTML p i
s_ = s mempty

samp :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
samp xs = element (tagName "samp") xs

samp_ :: forall p i. [HTML p i] -> HTML p i
samp_ = samp mempty

script :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
script xs = element (tagName "script") xs

script_ :: forall p i. [HTML p i] -> HTML p i
script_ = script mempty

section :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
section xs = element (tagName "section") xs

section_ :: forall p i. [HTML p i] -> HTML p i
section_ = section mempty

select :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
select xs = element (tagName "select") xs

select_ :: forall p i. [HTML p i] -> HTML p i
select_ = select mempty

small :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
small xs = element (tagName "small") xs

small_ :: forall p i. [HTML p i] -> HTML p i
small_ = small mempty

source :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
source xs = element (tagName "source") xs

source_ :: forall p i. [HTML p i] -> HTML p i
source_ = source mempty

span :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
span xs = element (tagName "span") xs

span_ :: forall p i. [HTML p i] -> HTML p i
span_ = span mempty

strike :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
strike xs = element (tagName "strike") xs

strike_ :: forall p i. [HTML p i] -> HTML p i
strike_ = strike mempty

strong :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
strong xs = element (tagName "strong") xs

strong_ :: forall p i. [HTML p i] -> HTML p i
strong_ = strong mempty

style :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
style xs = element (tagName "style") xs

style_ :: forall p i. [HTML p i] -> HTML p i
style_ = style mempty

sub :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
sub xs = element (tagName "sub") xs

sub_ :: forall p i. [HTML p i] -> HTML p i
sub_ = sub mempty

summary :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
summary xs = element (tagName "summary") xs

summary_ :: forall p i. [HTML p i] -> HTML p i
summary_ = summary mempty

sup :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
sup xs = element (tagName "sup") xs

sup_ :: forall p i. [HTML p i] -> HTML p i
sup_ = sup mempty

table :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
table xs = element (tagName "table") xs

table_ :: forall p i. [HTML p i] -> HTML p i
table_ = table mempty

tbody :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
tbody xs = element (tagName "tbody") xs

tbody_ :: forall p i. [HTML p i] -> HTML p i
tbody_ = tbody mempty

td :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
td xs = element (tagName "td") xs

td_ :: forall p i. [HTML p i] -> HTML p i
td_ = td mempty

textarea :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
textarea xs = element (tagName "textarea") xs

textarea_ :: forall p i. [HTML p i] -> HTML p i
textarea_ = textarea mempty

tfoot :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
tfoot xs = element (tagName "tfoot") xs

tfoot_ :: forall p i. [HTML p i] -> HTML p i
tfoot_ = tfoot mempty

th :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
th xs = element (tagName "th") xs

th_ :: forall p i. [HTML p i] -> HTML p i
th_ = th mempty

thead :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
thead xs = element (tagName "thead") xs

thead_ :: forall p i. [HTML p i] -> HTML p i
thead_ = thead mempty

time :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
time xs = element (tagName "time") xs

time_ :: forall p i. [HTML p i] -> HTML p i
time_ = time mempty

title :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
title xs = element (tagName "title") xs

title_ :: forall p i. [HTML p i] -> HTML p i
title_ = title mempty

tr :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
tr xs = element (tagName "tr") xs

tr_ :: forall p i. [HTML p i] -> HTML p i
tr_ = tr mempty

track :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
track xs = element (tagName "track") xs

track_ :: forall p i. [HTML p i] -> HTML p i
track_ = track mempty

tt :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
tt xs = element (tagName "tt") xs

tt_ :: forall p i. [HTML p i] -> HTML p i
tt_ = tt mempty

u :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
u xs = element (tagName "u") xs

u_ :: forall p i. [HTML p i] -> HTML p i
u_ = u mempty

ul :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
ul xs = element (tagName "ul") xs

ul_ :: forall p i. [HTML p i] -> HTML p i
ul_ = ul mempty

var :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
var xs = element (tagName "var") xs

var_ :: forall p i. [HTML p i] -> HTML p i
var_ = var mempty

video :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
video xs = element (tagName "video") xs

video_ :: forall p i. [HTML p i] -> HTML p i
video_ = video mempty

wbr :: forall p i. [A.Attr i] -> [HTML p i] -> HTML p i
wbr xs = element (tagName "wbr") xs

wbr_ :: forall p i. [HTML p i] -> HTML p i
wbr_ = wbr mempty
