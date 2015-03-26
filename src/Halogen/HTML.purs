-- | This module defines the HTML types required by the Halogen library, and provides
-- | smart constructors for HTML5 elements.

module Halogen.HTML
  ( AttrRepr
  , Attr()
  , runAttr
  
  , attr
  , handler
  
  , HTMLRepr
  
  , text
  , placeholder
  , element
  
  , TagName()
  , tagName
  , runTagName
  
  , AttributeName()
  , attributeName
  , runAttributeName
  
  , EventName()
  , eventName
  , runEventName
  
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
import Halogen.HTML.Events.Types
import Halogen.HTML.Events.Handler

-- | A type-safe wrapper for attribute names
-- |
-- | The phantom type `value` describes the type of value which this attribute requires.
newtype AttributeName value = AttributeName String

-- Create an attribute name
attributeName :: forall value. String -> AttributeName value
attributeName = AttributeName

-- | Unpack an attribute name
runAttributeName :: forall value. AttributeName value -> String
runAttributeName (AttributeName s) = s

-- | A type-safe wrapper for event names.
-- |
-- | The phantom type `fields` describes the event type which we can expect to exist on events
-- | corresponding to this name.
newtype EventName (fields :: # *) = EventName String

-- Create an event name
eventName :: forall fields. String -> EventName fields
eventName = EventName

-- | Unpack an event name
runEventName :: forall fields. EventName fields -> String
runEventName (EventName s) = s

-- | A type-safe wrapper for a HTML tag name
newtype TagName = TagName String

-- | Create a tag name
tagName :: String -> TagName
tagName = TagName

-- | Unwrap a `TagName` to get the tag name as a `String`.
runTagName :: TagName -> String
runTagName (TagName s) = s

-- | This type class encodes _representations_ of HTML attributes
class (Functor attr) <= AttrRepr attr where
  attr :: forall value i. (Show value) => AttributeName value -> value -> attr i
  handler :: forall fields i. EventName fields -> (Event fields -> EventHandler (Maybe i)) -> attr i

-- | `Attr` represents an abstract attribute
newtype Attr i = Attr (forall attr. (AttrRepr attr) => attr i)

runAttr :: forall i attr. (AttrRepr attr) => Attr i -> attr i
runAttr (Attr f) = f

instance attrRepr :: AttrRepr Attr where
  attr name value = Attr (attr name value)
  handler name f = Attr (handler name f)

instance functorAttr :: Functor Attr where
  (<$>) f (Attr x) = Attr (f <$> x)

-- | This type class encodes _representations_ of HTML nodes
class (Bifunctor node) <= HTMLRepr node where
  text :: forall p i. String -> node p i
  placeholder :: forall p i. p -> node p i
  element :: forall p i. TagName -> [Attr i] -> [node p i] -> node p i

a :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
a xs = element (tagName "a") xs

a_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
a_ = a mempty

abbr :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
abbr xs = element (tagName "abbr") xs

abbr_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
abbr_ = abbr mempty

acronym :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
acronym xs = element (tagName "acronym") xs

acronym_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
acronym_ = acronym mempty

address :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
address xs = element (tagName "address") xs

address_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
address_ = address mempty

applet :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
applet xs = element (tagName "applet") xs

applet_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
applet_ = applet mempty

area :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
area xs = element (tagName "area") xs

area_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
area_ = area mempty

article :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
article xs = element (tagName "article") xs

article_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
article_ = article mempty

aside :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
aside xs = element (tagName "aside") xs

aside_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
aside_ = aside mempty

audio :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
audio xs = element (tagName "audio") xs

audio_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
audio_ = audio mempty

b :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
b xs = element (tagName "b") xs

b_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
b_ = b mempty

base :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
base xs = element (tagName "base") xs

base_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
base_ = base mempty

basefont :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
basefont xs = element (tagName "basefont") xs

basefont_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
basefont_ = basefont mempty

bdi :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
bdi xs = element (tagName "bdi") xs

bdi_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
bdi_ = bdi mempty

bdo :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
bdo xs = element (tagName "bdo") xs

bdo_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
bdo_ = bdo mempty

big :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
big xs = element (tagName "big") xs

big_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
big_ = big mempty

blockquote :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
blockquote xs = element (tagName "blockquote") xs

blockquote_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
blockquote_ = blockquote mempty

body :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
body xs = element (tagName "body") xs

body_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
body_ = body mempty

br :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
br xs = element (tagName "br") xs

br_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
br_ = br mempty

button :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
button xs = element (tagName "button") xs

button_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
button_ = button mempty

canvas :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
canvas xs = element (tagName "canvas") xs

canvas_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
canvas_ = canvas mempty

caption :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
caption xs = element (tagName "caption") xs

caption_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
caption_ = caption mempty

center :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
center xs = element (tagName "center") xs

center_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
center_ = center mempty

cite :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
cite xs = element (tagName "cite") xs

cite_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
cite_ = cite mempty

code :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
code xs = element (tagName "code") xs

code_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
code_ = code mempty

col :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
col xs = element (tagName "col") xs

col_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
col_ = col mempty

colgroup :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
colgroup xs = element (tagName "colgroup") xs

colgroup_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
colgroup_ = colgroup mempty

datalist :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
datalist xs = element (tagName "datalist") xs

datalist_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
datalist_ = datalist mempty

dd :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
dd xs = element (tagName "dd") xs

dd_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
dd_ = dd mempty

del :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
del xs = element (tagName "del") xs

del_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
del_ = del mempty

details :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
details xs = element (tagName "details") xs

details_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
details_ = details mempty

dfn :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
dfn xs = element (tagName "dfn") xs

dfn_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
dfn_ = dfn mempty

dialog :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
dialog xs = element (tagName "dialog") xs

dialog_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
dialog_ = dialog mempty

dir :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
dir xs = element (tagName "dir") xs

dir_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
dir_ = dir mempty

div :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
div xs = element (tagName "div") xs

div_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
div_ = div mempty

dl :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
dl xs = element (tagName "dl") xs

dl_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
dl_ = dl mempty

dt :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
dt xs = element (tagName "dt") xs

dt_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
dt_ = dt mempty

em :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
em = element (tagName "em")

em_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
em_ = em mempty

embed :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
embed xs = element (tagName "embed") xs

embed_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
embed_ = embed mempty

fieldset :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
fieldset xs = element (tagName "fieldset") xs

fieldset_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
fieldset_ = fieldset mempty

figcaption :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
figcaption xs = element (tagName "figcaption") xs

figcaption_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
figcaption_ = figcaption mempty

figure :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
figure xs = element (tagName "figure") xs

figure_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
figure_ = figure mempty

font :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
font xs = element (tagName "font") xs

font_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
font_ = font mempty

footer :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
footer xs = element (tagName "footer") xs

footer_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
footer_ = footer mempty

form :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
form xs = element (tagName "form") xs

form_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
form_ = form mempty

frame :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
frame xs = element (tagName "frame") xs

frame_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
frame_ = frame mempty

frameset :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
frameset xs = element (tagName "frameset") xs

frameset_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
frameset_ = frameset mempty

h1 :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
h1 xs = element (tagName "h1") xs

h1_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
h1_ = h1 mempty

h2 :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
h2 xs = element (tagName "h2") xs

h2_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
h2_ = h2 mempty

h3 :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
h3 xs = element (tagName "h3") xs

h3_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
h3_ = h3 mempty

h4 :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
h4 xs = element (tagName "h4") xs

h4_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
h4_ = h4 mempty

h5 :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
h5 xs = element (tagName "h5") xs

h5_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
h5_ = h5 mempty

h6 :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
h6 xs = element (tagName "h6") xs

h6_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
h6_ = h6 mempty

head :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
head xs = element (tagName "head") xs

head_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
head_ = head mempty

header :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
header xs = element (tagName "header") xs

header_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
header_ = header mempty

hr :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
hr xs = element (tagName "hr") xs

hr_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
hr_ = hr mempty

html :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
html xs = element (tagName "html") xs

html_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
html_ = html mempty

i :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
i xs = element (tagName "i") xs

i_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
i_ = i mempty

iframe :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
iframe xs = element (tagName "iframe") xs

iframe_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
iframe_ = iframe mempty

img :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
img xs = element (tagName "img") xs

img_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
img_ = img mempty

input :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
input xs = element (tagName "input") xs

input_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
input_ = input mempty

ins :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
ins xs = element (tagName "ins") xs

ins_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
ins_ = ins mempty

kbd :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
kbd xs = element (tagName "kbd") xs

kbd_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
kbd_ = kbd mempty

keygen :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
keygen xs = element (tagName "keygen") xs

keygen_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
keygen_ = keygen mempty

label :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
label xs = element (tagName "label") xs

label_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
label_ = label mempty

legend :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
legend xs = element (tagName "legend") xs

legend_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
legend_ = legend mempty

li :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
li xs = element (tagName "li") xs

li_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
li_ = li mempty

link :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
link xs = element (tagName "link") xs

link_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
link_ = link mempty

main :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
main xs = element (tagName "main") xs

main_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
main_ = main mempty

map :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
map xs = element (tagName "map") xs

map_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
map_ = map mempty

mark :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
mark xs = element (tagName "mark") xs

mark_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
mark_ = mark mempty

menu :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
menu xs = element (tagName "menu") xs

menu_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
menu_ = menu mempty

menuitem :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
menuitem xs = element (tagName "menuitem") xs

menuitem_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
menuitem_ = menuitem mempty

meta :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
meta xs = element (tagName "meta") xs

meta_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
meta_ = meta mempty

meter :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
meter xs = element (tagName "meter") xs

meter_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
meter_ = meter mempty

nav :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
nav xs = element (tagName "nav") xs

nav_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
nav_ = nav mempty

noframes :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
noframes xs = element (tagName "noframes") xs

noframes_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
noframes_ = noframes mempty

noscript :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
noscript xs = element (tagName "noscript") xs

noscript_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
noscript_ = noscript mempty

object :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
object xs = element (tagName "object") xs

object_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
object_ = object mempty

ol :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
ol xs = element (tagName "ol") xs

ol_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
ol_ = ol mempty

optgroup :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
optgroup xs = element (tagName "optgroup") xs

optgroup_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
optgroup_ = optgroup mempty

option :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
option xs = element (tagName "option") xs

option_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
option_ = option mempty

output :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
output xs = element (tagName "output") xs

output_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
output_ = output mempty

p :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
p xs = element (tagName "p") xs

p_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
p_ = p mempty

param :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
param xs = element (tagName "param") xs

param_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
param_ = param mempty

pre :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
pre xs = element (tagName "pre") xs

pre_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
pre_ = pre mempty

progress :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
progress xs = element (tagName "progress") xs

progress_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
progress_ = progress mempty

q :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
q xs = element (tagName "q") xs

q_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
q_ = q mempty

rp :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
rp xs = element (tagName "rp") xs

rp_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
rp_ = rp mempty

rt :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
rt xs = element (tagName "rt") xs

rt_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
rt_ = rt mempty

ruby :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
ruby xs = element (tagName "ruby") xs

ruby_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
ruby_ = ruby mempty

s :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
s xs = element (tagName "s") xs

s_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
s_ = s mempty

samp :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
samp xs = element (tagName "samp") xs

samp_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
samp_ = samp mempty

script :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
script xs = element (tagName "script") xs

script_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
script_ = script mempty

section :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
section xs = element (tagName "section") xs

section_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
section_ = section mempty

select :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
select xs = element (tagName "select") xs

select_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
select_ = select mempty

small :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
small xs = element (tagName "small") xs

small_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
small_ = small mempty

source :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
source xs = element (tagName "source") xs

source_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
source_ = source mempty

span :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
span xs = element (tagName "span") xs

span_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
span_ = span mempty

strike :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
strike xs = element (tagName "strike") xs

strike_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
strike_ = strike mempty

strong :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
strong xs = element (tagName "strong") xs

strong_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
strong_ = strong mempty

style :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
style xs = element (tagName "style") xs

style_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
style_ = style mempty

sub :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
sub xs = element (tagName "sub") xs

sub_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
sub_ = sub mempty

summary :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
summary xs = element (tagName "summary") xs

summary_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
summary_ = summary mempty

sup :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
sup xs = element (tagName "sup") xs

sup_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
sup_ = sup mempty

table :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
table xs = element (tagName "table") xs

table_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
table_ = table mempty

tbody :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
tbody xs = element (tagName "tbody") xs

tbody_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
tbody_ = tbody mempty

td :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
td xs = element (tagName "td") xs

td_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
td_ = td mempty

textarea :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
textarea xs = element (tagName "textarea") xs

textarea_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
textarea_ = textarea mempty

tfoot :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
tfoot xs = element (tagName "tfoot") xs

tfoot_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
tfoot_ = tfoot mempty

th :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
th xs = element (tagName "th") xs

th_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
th_ = th mempty

thead :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
thead xs = element (tagName "thead") xs

thead_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
thead_ = thead mempty

time :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
time xs = element (tagName "time") xs

time_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
time_ = time mempty

title :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
title xs = element (tagName "title") xs

title_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
title_ = title mempty

tr :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
tr xs = element (tagName "tr") xs

tr_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
tr_ = tr mempty

track :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
track xs = element (tagName "track") xs

track_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
track_ = track mempty

tt :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
tt xs = element (tagName "tt") xs

tt_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
tt_ = tt mempty

u :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
u xs = element (tagName "u") xs

u_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
u_ = u mempty

ul :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
ul xs = element (tagName "ul") xs

ul_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
ul_ = ul mempty

var :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
var xs = element (tagName "var") xs

var_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
var_ = var mempty

video :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
video xs = element (tagName "video") xs

video_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
video_ = video mempty

wbr :: forall p i node. (HTMLRepr node) => [Attr i] -> [node p i] -> node p i
wbr xs = element (tagName "wbr") xs

wbr_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
wbr_ = wbr mempty
