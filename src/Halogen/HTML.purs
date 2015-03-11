-- | This module defines the HTML types required by the Halogen library, and provides
-- | smart constructors for HTML5 elements.

module Halogen.HTML
  ( HTML(..)
  , Attribute(..)
  , AttributeValue(..)
  
  , TagName()
  , tagName
  , runTagName
  
  , AttributeName()
  , attributeName
  , runAttributeName
  
  , attributesToProps
  
  , graft
  
  , text
  , placeholder
  , hashed
  
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
  
  , renderHtml
  , renderHtml'
  , renderHtmlToString
  ) where

import Data.Void
import Data.Maybe
import Data.Tuple
import Data.Foreign
import Data.Function
import Data.Monoid
import Data.String (joinWith)
import Data.Foldable (for_, foldMap)
import Data.Hashable (Hashcode(), runHashcode)

import qualified Data.Array as A

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Control.Monad.ST

import Halogen.Internal.VirtualDOM
import Halogen.HTML.Events.Handler

-- | A type-safe wrapper for attribute names
newtype AttributeName = AttributeName String

-- Create an attribute name
attributeName :: String -> AttributeName
attributeName = AttributeName

-- | Unpack an attribute name
runAttributeName :: AttributeName -> String
runAttributeName (AttributeName s) = s

-- | The type `AttributeValue i` represents values which can appear inside HTML attributes.
-- | Values are either strings or event handlers. Event handlers are required to produce outputs of type `i`.
data AttributeValue i
  = ValueAttribute String
  | HandlerAttribute (Foreign -> EventHandler (Maybe i))

instance functorAttributeValue :: Functor AttributeValue where
  (<$>) _ (ValueAttribute v) = ValueAttribute v
  (<$>) f (HandlerAttribute k) = HandlerAttribute (((f <$>) <$>) <<< k)

-- | A value of type `Attribute i` represents a collection of HTML attributes, whose
-- | event handlers produce outputs of type `i`.
-- |
-- | The `Semigroup` instance allows attributes to be combined.
data Attribute i = Attribute [Tuple AttributeName (AttributeValue i)]

instance functorAttribute :: Functor Attribute where
  (<$>) f (Attribute xs) = Attribute (A.map ((f <$>) <$>) xs)
  
instance semigroupAttribute :: Semigroup (Attribute i) where
  (<>) (Attribute xs) (Attribute ys) = Attribute (xs <> ys)

instance monoidAttribute :: Monoid (Attribute i) where
  mempty = Attribute []

-- | Convert a collection of attributes to an immutable property collection by providing an event handler.
-- |
-- | This function uses a temporary mutable property collection for efficiency.
attributesToProps :: forall i eff. (i -> Eff eff Unit) -> Attribute i -> Props
attributesToProps k (Attribute xs) = runProps do 
  props <- newProps
  for_ xs (addProp props)
  return props
  where
  addProp :: forall h eff. STProps h -> Tuple AttributeName (AttributeValue i) -> Eff (st :: ST h | eff) Unit
  addProp props (Tuple key (ValueAttribute value)) = runFn3 prop (runAttributeName key) value props
  addProp props (Tuple key (HandlerAttribute f)) = runFn3 handlerProp (runAttributeName key) handler props
    where
    handler :: Foreign -> Eff eff Unit
    handler e = do
      m <- unsafeInterleaveEff $ runEventHandler (unsafeFromForeign e) (f e)
      for_ m k

-- | A type-safe wrapper for a HTML tag name
newtype TagName = TagName String

-- | Create a tag name
tagName :: String -> TagName
tagName = TagName

-- | Unwrap a `TagName` to get the tag name as a `String`.
runTagName :: TagName -> String
runTagName (TagName s) = s

-- | The `HTML` type represents HTML documents before being rendered to the virtual DOM, and ultimately,
-- | the actual DOM.
-- |
-- | This representation is useful because it supports various typed transformations. It also gives a 
-- | strongly-typed representation for the events which can be generated by a document.
-- |
-- | The type parameter `a` corresponds to holes in the document which may be filled with `VTree`s during
-- | rendering. This way, the `HTML` type is kept pure while supporting custom rendering, e.g. embedding
-- | third-party components.
-- |
-- | The type parameter `i` represents the type of events which can be generated by this document.
-- |
-- | The meanings of the constructors of this type are as follows:
-- |
-- | - `Text`, `Element` - regular text and element nodes
-- | - `Placeholder` - A placeholder for a document. This can be replaced during rendering or by using the 
-- |   `graft` operation.
data HTML a i
  = Text String
  | Element TagName (Attribute i) [HTML a i]
  | Hashed Hashcode (Unit -> HTML a i)
  | Placeholder a
    
instance functorHTML :: Functor (HTML a) where
  (<$>) _ (Text s) = Text s
  (<$>) f (Element name attribs children) = Element name (f <$> attribs) (Data.Array.map (f <$>) children)
  (<$>) f (Hashed hash g) = Hashed hash ((f <$>) <<< g)
  (<$>) _ (Placeholder a) = Placeholder a
  
-- | Replace placeholder nodes with HTML documents.
graft :: forall a b i. HTML a i -> (a -> HTML b i) -> HTML b i 
graft (Placeholder a) f = f a
graft (Element name attr els) f = Element name attr (A.map (`graft` f) els)
graft (Hashed hc h) f = Hashed hc \u -> graft (h u) f
graft (Text s) _ = Text s

-- | A more general version of `renderHtml'`.
-- |
-- | The first argument is an event handler.
-- | 
-- | The second argument is used to replace placeholder nodes. If you are not using placeholder nodes, you
-- | might prefer to use `renderHtml` instead.
renderHtml' :: forall i a eff. (i -> Eff eff Unit) -> (a -> VTree) -> HTML a i -> VTree
renderHtml' _ _ (Text s) = vtext s
renderHtml' k f (Element name attribs els) = vnode (runTagName name) (attributesToProps k attribs) (A.map (renderHtml' k f) els)
renderHtml' k f (Hashed h html) = runFn2 hash (mkFn0 \_ -> renderHtml' k f (html unit)) h
renderHtml' _ f (Placeholder a) = f a

-- | Render a `HTML` document to a virtual DOM node
renderHtml :: forall i eff. (i -> Eff eff Unit) -> (forall a. HTML a i) -> VTree
renderHtml f = renderHtml' f absurd
  
-- | Render a HTML document as a `String`, usually for testing purposes.
-- |
-- | The rank-2 type ensures that neither events nor placeholders are allowed.
renderHtmlToString :: (forall a i. HTML a i) -> String
renderHtmlToString = go
  where
  go :: HTML Void Void -> String
  go (Text s) = s
  go (Element name (Attribute attr) els) = "<" <> runTagName name <> " " <> joinWith " " (A.map renderAttr attr) <> ">" <> foldMap go els <> "</" <> runTagName name <> ">"
  go (Hashed _ f) = go (f unit)
  
  renderAttr :: Tuple AttributeName (AttributeValue Void) -> String
  renderAttr (Tuple key (ValueAttribute value)) = runAttributeName key <> "=\"" <> value <> "\""
  
-- | Create a HTML document which represents a text node.
text :: forall a i. String -> HTML a i
text = Text

-- | Create a "hashed" HTML document, which only gets re-rendered when the hash changes
hashed :: forall a i. Hashcode -> (Unit -> HTML a i) -> HTML a i
hashed = Hashed

-- | Create a HTML document which acts as a placeholder for a `VTree` to be rendered later.
-- |
-- | This function is useful when embedding third-party widgets in HTML documents using the `widget` function, and
-- | is considered an advanced feature. Use at your own risk.
placeholder :: forall a i. a -> HTML a i
placeholder = Placeholder

a :: forall a i. Attribute i -> [HTML a i] -> HTML a i
a = Element $ tagName "a"

a_ :: forall a i. [HTML a i] -> HTML a i
a_ = a mempty

abbr :: forall a i. Attribute i -> [HTML a i] -> HTML a i
abbr = Element $ tagName "abbr"

abbr_ :: forall a i. [HTML a i] -> HTML a i
abbr_ = abbr mempty

acronym :: forall a i. Attribute i -> [HTML a i] -> HTML a i
acronym = Element $ tagName "acronym"

acronym_ :: forall a i. [HTML a i] -> HTML a i
acronym_ = acronym mempty

address :: forall a i. Attribute i -> [HTML a i] -> HTML a i
address = Element $ tagName "address"

address_ :: forall a i. [HTML a i] -> HTML a i
address_ = address mempty

applet :: forall a i. Attribute i -> [HTML a i] -> HTML a i
applet = Element $ tagName "applet"

applet_ :: forall a i. [HTML a i] -> HTML a i
applet_ = applet mempty

area :: forall a i. Attribute i -> [HTML a i] -> HTML a i
area = Element $ tagName "area"

area_ :: forall a i. [HTML a i] -> HTML a i
area_ = area mempty

article :: forall a i. Attribute i -> [HTML a i] -> HTML a i
article = Element $ tagName "article"

article_ :: forall a i. [HTML a i] -> HTML a i
article_ = article mempty

aside :: forall a i. Attribute i -> [HTML a i] -> HTML a i
aside = Element $ tagName "aside"

aside_ :: forall a i. [HTML a i] -> HTML a i
aside_ = aside mempty

audio :: forall a i. Attribute i -> [HTML a i] -> HTML a i
audio = Element $ tagName "audio"

audio_ :: forall a i. [HTML a i] -> HTML a i
audio_ = audio mempty

b :: forall a i. Attribute i -> [HTML a i] -> HTML a i
b = Element $ tagName "b"

b_ :: forall a i. [HTML a i] -> HTML a i
b_ = b mempty

base :: forall a i. Attribute i -> [HTML a i] -> HTML a i
base = Element $ tagName "base"

base_ :: forall a i. [HTML a i] -> HTML a i
base_ = base mempty

basefont :: forall a i. Attribute i -> [HTML a i] -> HTML a i
basefont = Element $ tagName "basefont"

basefont_ :: forall a i. [HTML a i] -> HTML a i
basefont_ = basefont mempty

bdi :: forall a i. Attribute i -> [HTML a i] -> HTML a i
bdi = Element $ tagName "bdi"

bdi_ :: forall a i. [HTML a i] -> HTML a i
bdi_ = bdi mempty

bdo :: forall a i. Attribute i -> [HTML a i] -> HTML a i
bdo = Element $ tagName "bdo"

bdo_ :: forall a i. [HTML a i] -> HTML a i
bdo_ = bdo mempty

big :: forall a i. Attribute i -> [HTML a i] -> HTML a i
big = Element $ tagName "big"

big_ :: forall a i. [HTML a i] -> HTML a i
big_ = big mempty

blockquote :: forall a i. Attribute i -> [HTML a i] -> HTML a i
blockquote = Element $ tagName "blockquote"

blockquote_ :: forall a i. [HTML a i] -> HTML a i
blockquote_ = blockquote mempty

body :: forall a i. Attribute i -> [HTML a i] -> HTML a i
body = Element $ tagName "body"

body_ :: forall a i. [HTML a i] -> HTML a i
body_ = body mempty

br :: forall a i. Attribute i -> [HTML a i] -> HTML a i
br = Element $ tagName "br"

br_ :: forall a i. [HTML a i] -> HTML a i
br_ = br mempty

button :: forall a i. Attribute i -> [HTML a i] -> HTML a i
button = Element $ tagName "button"

button_ :: forall a i. [HTML a i] -> HTML a i
button_ = button mempty

canvas :: forall a i. Attribute i -> [HTML a i] -> HTML a i
canvas = Element $ tagName "canvas"

canvas_ :: forall a i. [HTML a i] -> HTML a i
canvas_ = canvas mempty

caption :: forall a i. Attribute i -> [HTML a i] -> HTML a i
caption = Element $ tagName "caption"

caption_ :: forall a i. [HTML a i] -> HTML a i
caption_ = caption mempty

center :: forall a i. Attribute i -> [HTML a i] -> HTML a i
center = Element $ tagName "center"

center_ :: forall a i. [HTML a i] -> HTML a i
center_ = center mempty

cite :: forall a i. Attribute i -> [HTML a i] -> HTML a i
cite = Element $ tagName "cite"

cite_ :: forall a i. [HTML a i] -> HTML a i
cite_ = cite mempty

code :: forall a i. Attribute i -> [HTML a i] -> HTML a i
code = Element $ tagName "code"

code_ :: forall a i. [HTML a i] -> HTML a i
code_ = code mempty

col :: forall a i. Attribute i -> [HTML a i] -> HTML a i
col = Element $ tagName "col"

col_ :: forall a i. [HTML a i] -> HTML a i
col_ = col mempty

colgroup :: forall a i. Attribute i -> [HTML a i] -> HTML a i
colgroup = Element $ tagName "colgroup"

colgroup_ :: forall a i. [HTML a i] -> HTML a i
colgroup_ = colgroup mempty

datalist :: forall a i. Attribute i -> [HTML a i] -> HTML a i
datalist = Element $ tagName "datalist"

datalist_ :: forall a i. [HTML a i] -> HTML a i
datalist_ = datalist mempty

dd :: forall a i. Attribute i -> [HTML a i] -> HTML a i
dd = Element $ tagName "dd"

dd_ :: forall a i. [HTML a i] -> HTML a i
dd_ = dd mempty

del :: forall a i. Attribute i -> [HTML a i] -> HTML a i
del = Element $ tagName "del"

del_ :: forall a i. [HTML a i] -> HTML a i
del_ = del mempty

details :: forall a i. Attribute i -> [HTML a i] -> HTML a i
details = Element $ tagName "details"

details_ :: forall a i. [HTML a i] -> HTML a i
details_ = details mempty

dfn :: forall a i. Attribute i -> [HTML a i] -> HTML a i
dfn = Element $ tagName "dfn"

dfn_ :: forall a i. [HTML a i] -> HTML a i
dfn_ = dfn mempty

dialog :: forall a i. Attribute i -> [HTML a i] -> HTML a i
dialog = Element $ tagName "dialog"

dialog_ :: forall a i. [HTML a i] -> HTML a i
dialog_ = dialog mempty

dir :: forall a i. Attribute i -> [HTML a i] -> HTML a i
dir = Element $ tagName "dir"

dir_ :: forall a i. [HTML a i] -> HTML a i
dir_ = dir mempty

div :: forall a i. Attribute i -> [HTML a i] -> HTML a i
div = Element $ tagName "div"

div_ :: forall a i. [HTML a i] -> HTML a i
div_ = div mempty

dl :: forall a i. Attribute i -> [HTML a i] -> HTML a i
dl = Element $ tagName "dl"

dl_ :: forall a i. [HTML a i] -> HTML a i
dl_ = dl mempty

dt :: forall a i. Attribute i -> [HTML a i] -> HTML a i
dt = Element $ tagName "dt"

dt_ :: forall a i. [HTML a i] -> HTML a i
dt_ = dt mempty

em :: forall a i. Attribute i -> [HTML a i] -> HTML a i
em = Element $ tagName "em"

em_ :: forall a i. [HTML a i] -> HTML a i
em_ = em mempty

embed :: forall a i. Attribute i -> [HTML a i] -> HTML a i
embed = Element $ tagName "embed"

embed_ :: forall a i. [HTML a i] -> HTML a i
embed_ = embed mempty

fieldset :: forall a i. Attribute i -> [HTML a i] -> HTML a i
fieldset = Element $ tagName "fieldset"

fieldset_ :: forall a i. [HTML a i] -> HTML a i
fieldset_ = fieldset mempty

figcaption :: forall a i. Attribute i -> [HTML a i] -> HTML a i
figcaption = Element $ tagName "figcaption"

figcaption_ :: forall a i. [HTML a i] -> HTML a i
figcaption_ = figcaption mempty

figure :: forall a i. Attribute i -> [HTML a i] -> HTML a i
figure = Element $ tagName "figure"

figure_ :: forall a i. [HTML a i] -> HTML a i
figure_ = figure mempty

font :: forall a i. Attribute i -> [HTML a i] -> HTML a i
font = Element $ tagName "font"

font_ :: forall a i. [HTML a i] -> HTML a i
font_ = font mempty

footer :: forall a i. Attribute i -> [HTML a i] -> HTML a i
footer = Element $ tagName "footer"

footer_ :: forall a i. [HTML a i] -> HTML a i
footer_ = footer mempty

form :: forall a i. Attribute i -> [HTML a i] -> HTML a i
form = Element $ tagName "form"

form_ :: forall a i. [HTML a i] -> HTML a i
form_ = form mempty

frame :: forall a i. Attribute i -> [HTML a i] -> HTML a i
frame = Element $ tagName "frame"

frame_ :: forall a i. [HTML a i] -> HTML a i
frame_ = frame mempty

frameset :: forall a i. Attribute i -> [HTML a i] -> HTML a i
frameset = Element $ tagName "frameset"

frameset_ :: forall a i. [HTML a i] -> HTML a i
frameset_ = frameset mempty

h1 :: forall a i. Attribute i -> [HTML a i] -> HTML a i
h1 = Element $ tagName "h1"

h1_ :: forall a i. [HTML a i] -> HTML a i
h1_ = h1 mempty

h2 :: forall a i. Attribute i -> [HTML a i] -> HTML a i
h2 = Element $ tagName "h2"

h2_ :: forall a i. [HTML a i] -> HTML a i
h2_ = h2 mempty

h3 :: forall a i. Attribute i -> [HTML a i] -> HTML a i
h3 = Element $ tagName "h3"

h3_ :: forall a i. [HTML a i] -> HTML a i
h3_ = h3 mempty

h4 :: forall a i. Attribute i -> [HTML a i] -> HTML a i
h4 = Element $ tagName "h4"

h4_ :: forall a i. [HTML a i] -> HTML a i
h4_ = h4 mempty

h5 :: forall a i. Attribute i -> [HTML a i] -> HTML a i
h5 = Element $ tagName "h5"

h5_ :: forall a i. [HTML a i] -> HTML a i
h5_ = h5 mempty

h6 :: forall a i. Attribute i -> [HTML a i] -> HTML a i
h6 = Element $ tagName "h6"

h6_ :: forall a i. [HTML a i] -> HTML a i
h6_ = h6 mempty

head :: forall a i. Attribute i -> [HTML a i] -> HTML a i
head = Element $ tagName "head"

head_ :: forall a i. [HTML a i] -> HTML a i
head_ = head mempty

header :: forall a i. Attribute i -> [HTML a i] -> HTML a i
header = Element $ tagName "header"

header_ :: forall a i. [HTML a i] -> HTML a i
header_ = header mempty

hr :: forall a i. Attribute i -> [HTML a i] -> HTML a i
hr = Element $ tagName "hr"

hr_ :: forall a i. [HTML a i] -> HTML a i
hr_ = hr mempty

html :: forall a i. Attribute i -> [HTML a i] -> HTML a i
html = Element $ tagName "html"

html_ :: forall a i. [HTML a i] -> HTML a i
html_ = html mempty

i :: forall a i. Attribute i -> [HTML a i] -> HTML a i
i = Element $ tagName "i"

i_ :: forall a i. [HTML a i] -> HTML a i
i_ = i mempty

iframe :: forall a i. Attribute i -> [HTML a i] -> HTML a i
iframe = Element $ tagName "iframe"

iframe_ :: forall a i. [HTML a i] -> HTML a i
iframe_ = iframe mempty

img :: forall a i. Attribute i -> [HTML a i] -> HTML a i
img = Element $ tagName "img"

img_ :: forall a i. [HTML a i] -> HTML a i
img_ = img mempty

input :: forall a i. Attribute i -> [HTML a i] -> HTML a i
input = Element $ tagName "input"

input_ :: forall a i. [HTML a i] -> HTML a i
input_ = input mempty

ins :: forall a i. Attribute i -> [HTML a i] -> HTML a i
ins = Element $ tagName "ins"

ins_ :: forall a i. [HTML a i] -> HTML a i
ins_ = ins mempty

kbd :: forall a i. Attribute i -> [HTML a i] -> HTML a i
kbd = Element $ tagName "kbd"

kbd_ :: forall a i. [HTML a i] -> HTML a i
kbd_ = kbd mempty

keygen :: forall a i. Attribute i -> [HTML a i] -> HTML a i
keygen = Element $ tagName "keygen"

keygen_ :: forall a i. [HTML a i] -> HTML a i
keygen_ = keygen mempty

label :: forall a i. Attribute i -> [HTML a i] -> HTML a i
label = Element $ tagName "label"

label_ :: forall a i. [HTML a i] -> HTML a i
label_ = label mempty

legend :: forall a i. Attribute i -> [HTML a i] -> HTML a i
legend = Element $ tagName "legend"

legend_ :: forall a i. [HTML a i] -> HTML a i
legend_ = legend mempty

li :: forall a i. Attribute i -> [HTML a i] -> HTML a i
li = Element $ tagName "li"

li_ :: forall a i. [HTML a i] -> HTML a i
li_ = li mempty

link :: forall a i. Attribute i -> [HTML a i] -> HTML a i
link = Element $ tagName "link"

link_ :: forall a i. [HTML a i] -> HTML a i
link_ = link mempty

main :: forall a i. Attribute i -> [HTML a i] -> HTML a i
main = Element $ tagName "main"

main_ :: forall a i. [HTML a i] -> HTML a i
main_ = main mempty

map :: forall a i. Attribute i -> [HTML a i] -> HTML a i
map = Element $ tagName "map"

map_ :: forall a i. [HTML a i] -> HTML a i
map_ = map mempty

mark :: forall a i. Attribute i -> [HTML a i] -> HTML a i
mark = Element $ tagName "mark"

mark_ :: forall a i. [HTML a i] -> HTML a i
mark_ = mark mempty

menu :: forall a i. Attribute i -> [HTML a i] -> HTML a i
menu = Element $ tagName "menu"

menu_ :: forall a i. [HTML a i] -> HTML a i
menu_ = menu mempty

menuitem :: forall a i. Attribute i -> [HTML a i] -> HTML a i
menuitem = Element $ tagName "menuitem"

menuitem_ :: forall a i. [HTML a i] -> HTML a i
menuitem_ = menuitem mempty

meta :: forall a i. Attribute i -> [HTML a i] -> HTML a i
meta = Element $ tagName "meta"

meta_ :: forall a i. [HTML a i] -> HTML a i
meta_ = meta mempty

meter :: forall a i. Attribute i -> [HTML a i] -> HTML a i
meter = Element $ tagName "meter"

meter_ :: forall a i. [HTML a i] -> HTML a i
meter_ = meter mempty

nav :: forall a i. Attribute i -> [HTML a i] -> HTML a i
nav = Element $ tagName "nav"

nav_ :: forall a i. [HTML a i] -> HTML a i
nav_ = nav mempty

noframes :: forall a i. Attribute i -> [HTML a i] -> HTML a i
noframes = Element $ tagName "noframes"

noframes_ :: forall a i. [HTML a i] -> HTML a i
noframes_ = noframes mempty

noscript :: forall a i. Attribute i -> [HTML a i] -> HTML a i
noscript = Element $ tagName "noscript"

noscript_ :: forall a i. [HTML a i] -> HTML a i
noscript_ = noscript mempty

object :: forall a i. Attribute i -> [HTML a i] -> HTML a i
object = Element $ tagName "object"

object_ :: forall a i. [HTML a i] -> HTML a i
object_ = object mempty

ol :: forall a i. Attribute i -> [HTML a i] -> HTML a i
ol = Element $ tagName "ol"

ol_ :: forall a i. [HTML a i] -> HTML a i
ol_ = ol mempty

optgroup :: forall a i. Attribute i -> [HTML a i] -> HTML a i
optgroup = Element $ tagName "optgroup"

optgroup_ :: forall a i. [HTML a i] -> HTML a i
optgroup_ = optgroup mempty

option :: forall a i. Attribute i -> [HTML a i] -> HTML a i
option = Element $ tagName "option"

option_ :: forall a i. [HTML a i] -> HTML a i
option_ = option mempty

output :: forall a i. Attribute i -> [HTML a i] -> HTML a i
output = Element $ tagName "output"

output_ :: forall a i. [HTML a i] -> HTML a i
output_ = output mempty

p :: forall a i. Attribute i -> [HTML a i] -> HTML a i
p = Element $ tagName "p"

p_ :: forall a i. [HTML a i] -> HTML a i
p_ = p mempty

param :: forall a i. Attribute i -> [HTML a i] -> HTML a i
param = Element $ tagName "param"

param_ :: forall a i. [HTML a i] -> HTML a i
param_ = param mempty

pre :: forall a i. Attribute i -> [HTML a i] -> HTML a i
pre = Element $ tagName "pre"

pre_ :: forall a i. [HTML a i] -> HTML a i
pre_ = pre mempty

progress :: forall a i. Attribute i -> [HTML a i] -> HTML a i
progress = Element $ tagName "progress"

progress_ :: forall a i. [HTML a i] -> HTML a i
progress_ = progress mempty

q :: forall a i. Attribute i -> [HTML a i] -> HTML a i
q = Element $ tagName "q"

q_ :: forall a i. [HTML a i] -> HTML a i
q_ = q mempty

rp :: forall a i. Attribute i -> [HTML a i] -> HTML a i
rp = Element $ tagName "rp"

rp_ :: forall a i. [HTML a i] -> HTML a i
rp_ = rp mempty

rt :: forall a i. Attribute i -> [HTML a i] -> HTML a i
rt = Element $ tagName "rt"

rt_ :: forall a i. [HTML a i] -> HTML a i
rt_ = rt mempty

ruby :: forall a i. Attribute i -> [HTML a i] -> HTML a i
ruby = Element $ tagName "ruby"

ruby_ :: forall a i. [HTML a i] -> HTML a i
ruby_ = ruby mempty

s :: forall a i. Attribute i -> [HTML a i] -> HTML a i
s = Element $ tagName "s"

s_ :: forall a i. [HTML a i] -> HTML a i
s_ = s mempty

samp :: forall a i. Attribute i -> [HTML a i] -> HTML a i
samp = Element $ tagName "samp"

samp_ :: forall a i. [HTML a i] -> HTML a i
samp_ = samp mempty

script :: forall a i. Attribute i -> [HTML a i] -> HTML a i
script = Element $ tagName "script"

script_ :: forall a i. [HTML a i] -> HTML a i
script_ = script mempty

section :: forall a i. Attribute i -> [HTML a i] -> HTML a i
section = Element $ tagName "section"

section_ :: forall a i. [HTML a i] -> HTML a i
section_ = section mempty

select :: forall a i. Attribute i -> [HTML a i] -> HTML a i
select = Element $ tagName "select"

select_ :: forall a i. [HTML a i] -> HTML a i
select_ = select mempty

small :: forall a i. Attribute i -> [HTML a i] -> HTML a i
small = Element $ tagName "small"

small_ :: forall a i. [HTML a i] -> HTML a i
small_ = small mempty

source :: forall a i. Attribute i -> [HTML a i] -> HTML a i
source = Element $ tagName "source"

source_ :: forall a i. [HTML a i] -> HTML a i
source_ = source mempty

span :: forall a i. Attribute i -> [HTML a i] -> HTML a i
span = Element $ tagName "span"

span_ :: forall a i. [HTML a i] -> HTML a i
span_ = span mempty

strike :: forall a i. Attribute i -> [HTML a i] -> HTML a i
strike = Element $ tagName "strike"

strike_ :: forall a i. [HTML a i] -> HTML a i
strike_ = strike mempty

strong :: forall a i. Attribute i -> [HTML a i] -> HTML a i
strong = Element $ tagName "strong"

strong_ :: forall a i. [HTML a i] -> HTML a i
strong_ = strong mempty

style :: forall a i. Attribute i -> [HTML a i] -> HTML a i
style = Element $ tagName "style"

style_ :: forall a i. [HTML a i] -> HTML a i
style_ = style mempty

sub :: forall a i. Attribute i -> [HTML a i] -> HTML a i
sub = Element $ tagName "sub"

sub_ :: forall a i. [HTML a i] -> HTML a i
sub_ = sub mempty

summary :: forall a i. Attribute i -> [HTML a i] -> HTML a i
summary = Element $ tagName "summary"

summary_ :: forall a i. [HTML a i] -> HTML a i
summary_ = summary mempty

sup :: forall a i. Attribute i -> [HTML a i] -> HTML a i
sup = Element $ tagName "sup"

sup_ :: forall a i. [HTML a i] -> HTML a i
sup_ = sup mempty

table :: forall a i. Attribute i -> [HTML a i] -> HTML a i
table = Element $ tagName "table"

table_ :: forall a i. [HTML a i] -> HTML a i
table_ = table mempty

tbody :: forall a i. Attribute i -> [HTML a i] -> HTML a i
tbody = Element $ tagName "tbody"

tbody_ :: forall a i. [HTML a i] -> HTML a i
tbody_ = tbody mempty

td :: forall a i. Attribute i -> [HTML a i] -> HTML a i
td = Element $ tagName "td"

td_ :: forall a i. [HTML a i] -> HTML a i
td_ = td mempty

textarea :: forall a i. Attribute i -> [HTML a i] -> HTML a i
textarea = Element $ tagName "textarea"

textarea_ :: forall a i. [HTML a i] -> HTML a i
textarea_ = textarea mempty

tfoot :: forall a i. Attribute i -> [HTML a i] -> HTML a i
tfoot = Element $ tagName "tfoot"

tfoot_ :: forall a i. [HTML a i] -> HTML a i
tfoot_ = tfoot mempty

th :: forall a i. Attribute i -> [HTML a i] -> HTML a i
th = Element $ tagName "th"

th_ :: forall a i. [HTML a i] -> HTML a i
th_ = th mempty

thead :: forall a i. Attribute i -> [HTML a i] -> HTML a i
thead = Element $ tagName "thead"

thead_ :: forall a i. [HTML a i] -> HTML a i
thead_ = thead mempty

time :: forall a i. Attribute i -> [HTML a i] -> HTML a i
time = Element $ tagName "time"

time_ :: forall a i. [HTML a i] -> HTML a i
time_ = time mempty

title :: forall a i. Attribute i -> [HTML a i] -> HTML a i
title = Element $ tagName "title"

title_ :: forall a i. [HTML a i] -> HTML a i
title_ = title mempty

tr :: forall a i. Attribute i -> [HTML a i] -> HTML a i
tr = Element $ tagName "tr"

tr_ :: forall a i. [HTML a i] -> HTML a i
tr_ = tr mempty

track :: forall a i. Attribute i -> [HTML a i] -> HTML a i
track = Element $ tagName "track"

track_ :: forall a i. [HTML a i] -> HTML a i
track_ = track mempty

tt :: forall a i. Attribute i -> [HTML a i] -> HTML a i
tt = Element $ tagName "tt"

tt_ :: forall a i. [HTML a i] -> HTML a i
tt_ = tt mempty

u :: forall a i. Attribute i -> [HTML a i] -> HTML a i
u = Element $ tagName "u"

u_ :: forall a i. [HTML a i] -> HTML a i
u_ = u mempty

ul :: forall a i. Attribute i -> [HTML a i] -> HTML a i
ul = Element $ tagName "ul"

ul_ :: forall a i. [HTML a i] -> HTML a i
ul_ = ul mempty

var :: forall a i. Attribute i -> [HTML a i] -> HTML a i
var = Element $ tagName "var"

var_ :: forall a i. [HTML a i] -> HTML a i
var_ = var mempty

video :: forall a i. Attribute i -> [HTML a i] -> HTML a i
video = Element $ tagName "video"

video_ :: forall a i. [HTML a i] -> HTML a i
video_ = video mempty

wbr :: forall a i. Attribute i -> [HTML a i] -> HTML a i
wbr = Element $ tagName "wbr"

wbr_ :: forall a i. [HTML a i] -> HTML a i
wbr_ = wbr mempty
