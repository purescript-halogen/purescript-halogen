# Module Documentation

## Module Halogen.HTML


This module defines the HTML types required by the Halogen library, and provides
smart constructors for HTML5 elements.

#### `AttributeName`

``` purescript
newtype AttributeName value
```

A type-safe wrapper for attribute names

The phantom type `value` describes the type of value which this attribute requires.

#### `attributeName`

``` purescript
attributeName :: forall value. String -> AttributeName value
```

#### `runAttributeName`

``` purescript
runAttributeName :: forall value. AttributeName value -> String
```

Unpack an attribute name

#### `EventName`

``` purescript
newtype EventName (fields :: # *)
```

A type-safe wrapper for event names.

The phantom type `fields` describes the event type which we can expect to exist on events
corresponding to this name.

#### `eventName`

``` purescript
eventName :: forall fields. String -> EventName fields
```

#### `runEventName`

``` purescript
runEventName :: forall fields. EventName fields -> String
```

Unpack an event name

#### `TagName`

``` purescript
newtype TagName
```

A type-safe wrapper for a HTML tag name

#### `tagName`

``` purescript
tagName :: String -> TagName
```

Create a tag name

#### `runTagName`

``` purescript
runTagName :: TagName -> String
```

Unwrap a `TagName` to get the tag name as a `String`.

#### `AttrRepr`

``` purescript
class (Plus attr) <= AttrRepr attr where
  attr_ :: forall value i. (Show value) => AttributeName value -> value -> attr i
  handler_ :: forall event i. EventName event -> (Event event -> EventHandler (Maybe i)) -> attr i
```

This type class encodes _representations_ of HTML attributes

#### `Attr`

``` purescript
newtype Attr i
```

`Attr` represents an abstract attribute

#### `runAttr`

``` purescript
runAttr :: forall i attr. (AttrRepr attr) => Attr i -> attr i
```


#### `semigroupAttr`

``` purescript
instance semigroupAttr :: Semigroup (Attr i)
```


#### `monoidAttr`

``` purescript
instance monoidAttr :: Monoid (Attr i)
```


#### `attr`

``` purescript
attr :: forall value i. (Show value) => AttributeName value -> value -> Attr i
```


#### `handler`

``` purescript
handler :: forall event i. EventName event -> (Event event -> EventHandler (Maybe i)) -> Attr i
```


#### `HTMLRepr`

``` purescript
class (Bifunctor node) <= HTMLRepr node where
  text_ :: forall p i. String -> node p i
  placeholder_ :: forall p i. p -> node p i
  element_ :: forall p i. TagName -> Attr i -> [node p i] -> node p i
```

This type class encodes _representations_ of HTML nodes

#### `HTML`

``` purescript
newtype HTML p i
```

`HTML` represents an abstract HTML node

#### `functorHTML`

``` purescript
instance functorHTML :: Functor (HTML p)
```


#### `runHTML`

``` purescript
runHTML :: forall p i node. (HTMLRepr node) => HTML p i -> node p i
```


#### `text`

``` purescript
text :: forall p i. String -> HTML p i
```


#### `placeholder`

``` purescript
placeholder :: forall p i. p -> HTML p i
```


#### `element`

``` purescript
element :: forall p i. TagName -> Attr i -> [HTML p i] -> HTML p i
```


#### `a`

``` purescript
a :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `a_`

``` purescript
a_ :: forall p i. [HTML p i] -> HTML p i
```


#### `abbr`

``` purescript
abbr :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `abbr_`

``` purescript
abbr_ :: forall p i. [HTML p i] -> HTML p i
```


#### `acronym`

``` purescript
acronym :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `acronym_`

``` purescript
acronym_ :: forall p i. [HTML p i] -> HTML p i
```


#### `address`

``` purescript
address :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `address_`

``` purescript
address_ :: forall p i. [HTML p i] -> HTML p i
```


#### `applet`

``` purescript
applet :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `applet_`

``` purescript
applet_ :: forall p i. [HTML p i] -> HTML p i
```


#### `area`

``` purescript
area :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `area_`

``` purescript
area_ :: forall p i. [HTML p i] -> HTML p i
```


#### `article`

``` purescript
article :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `article_`

``` purescript
article_ :: forall p i. [HTML p i] -> HTML p i
```


#### `aside`

``` purescript
aside :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `aside_`

``` purescript
aside_ :: forall p i. [HTML p i] -> HTML p i
```


#### `audio`

``` purescript
audio :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `audio_`

``` purescript
audio_ :: forall p i. [HTML p i] -> HTML p i
```


#### `b`

``` purescript
b :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `b_`

``` purescript
b_ :: forall p i. [HTML p i] -> HTML p i
```


#### `base`

``` purescript
base :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `base_`

``` purescript
base_ :: forall p i. [HTML p i] -> HTML p i
```


#### `basefont`

``` purescript
basefont :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `basefont_`

``` purescript
basefont_ :: forall p i. [HTML p i] -> HTML p i
```


#### `bdi`

``` purescript
bdi :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `bdi_`

``` purescript
bdi_ :: forall p i. [HTML p i] -> HTML p i
```


#### `bdo`

``` purescript
bdo :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `bdo_`

``` purescript
bdo_ :: forall p i. [HTML p i] -> HTML p i
```


#### `big`

``` purescript
big :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `big_`

``` purescript
big_ :: forall p i. [HTML p i] -> HTML p i
```


#### `blockquote`

``` purescript
blockquote :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `blockquote_`

``` purescript
blockquote_ :: forall p i. [HTML p i] -> HTML p i
```


#### `body`

``` purescript
body :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `body_`

``` purescript
body_ :: forall p i. [HTML p i] -> HTML p i
```


#### `br`

``` purescript
br :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `br_`

``` purescript
br_ :: forall p i. [HTML p i] -> HTML p i
```


#### `button`

``` purescript
button :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `button_`

``` purescript
button_ :: forall p i. [HTML p i] -> HTML p i
```


#### `canvas`

``` purescript
canvas :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `canvas_`

``` purescript
canvas_ :: forall p i. [HTML p i] -> HTML p i
```


#### `caption`

``` purescript
caption :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `caption_`

``` purescript
caption_ :: forall p i. [HTML p i] -> HTML p i
```


#### `center`

``` purescript
center :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `center_`

``` purescript
center_ :: forall p i. [HTML p i] -> HTML p i
```


#### `cite`

``` purescript
cite :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `cite_`

``` purescript
cite_ :: forall p i. [HTML p i] -> HTML p i
```


#### `code`

``` purescript
code :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `code_`

``` purescript
code_ :: forall p i. [HTML p i] -> HTML p i
```


#### `col`

``` purescript
col :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `col_`

``` purescript
col_ :: forall p i. [HTML p i] -> HTML p i
```


#### `colgroup`

``` purescript
colgroup :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `colgroup_`

``` purescript
colgroup_ :: forall p i. [HTML p i] -> HTML p i
```


#### `datalist`

``` purescript
datalist :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `datalist_`

``` purescript
datalist_ :: forall p i. [HTML p i] -> HTML p i
```


#### `dd`

``` purescript
dd :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `dd_`

``` purescript
dd_ :: forall p i. [HTML p i] -> HTML p i
```


#### `del`

``` purescript
del :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `del_`

``` purescript
del_ :: forall p i. [HTML p i] -> HTML p i
```


#### `details`

``` purescript
details :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `details_`

``` purescript
details_ :: forall p i. [HTML p i] -> HTML p i
```


#### `dfn`

``` purescript
dfn :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `dfn_`

``` purescript
dfn_ :: forall p i. [HTML p i] -> HTML p i
```


#### `dialog`

``` purescript
dialog :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `dialog_`

``` purescript
dialog_ :: forall p i. [HTML p i] -> HTML p i
```


#### `dir`

``` purescript
dir :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `dir_`

``` purescript
dir_ :: forall p i. [HTML p i] -> HTML p i
```


#### `div`

``` purescript
div :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `div_`

``` purescript
div_ :: forall p i. [HTML p i] -> HTML p i
```


#### `dl`

``` purescript
dl :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `dl_`

``` purescript
dl_ :: forall p i. [HTML p i] -> HTML p i
```


#### `dt`

``` purescript
dt :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `dt_`

``` purescript
dt_ :: forall p i. [HTML p i] -> HTML p i
```


#### `em`

``` purescript
em :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `em_`

``` purescript
em_ :: forall p i. [HTML p i] -> HTML p i
```


#### `embed`

``` purescript
embed :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `embed_`

``` purescript
embed_ :: forall p i. [HTML p i] -> HTML p i
```


#### `fieldset`

``` purescript
fieldset :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `fieldset_`

``` purescript
fieldset_ :: forall p i. [HTML p i] -> HTML p i
```


#### `figcaption`

``` purescript
figcaption :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `figcaption_`

``` purescript
figcaption_ :: forall p i. [HTML p i] -> HTML p i
```


#### `figure`

``` purescript
figure :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `figure_`

``` purescript
figure_ :: forall p i. [HTML p i] -> HTML p i
```


#### `font`

``` purescript
font :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `font_`

``` purescript
font_ :: forall p i. [HTML p i] -> HTML p i
```


#### `footer`

``` purescript
footer :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `footer_`

``` purescript
footer_ :: forall p i. [HTML p i] -> HTML p i
```


#### `form`

``` purescript
form :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `form_`

``` purescript
form_ :: forall p i. [HTML p i] -> HTML p i
```


#### `frame`

``` purescript
frame :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `frame_`

``` purescript
frame_ :: forall p i. [HTML p i] -> HTML p i
```


#### `frameset`

``` purescript
frameset :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `frameset_`

``` purescript
frameset_ :: forall p i. [HTML p i] -> HTML p i
```


#### `h1`

``` purescript
h1 :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `h1_`

``` purescript
h1_ :: forall p i. [HTML p i] -> HTML p i
```


#### `h2`

``` purescript
h2 :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `h2_`

``` purescript
h2_ :: forall p i. [HTML p i] -> HTML p i
```


#### `h3`

``` purescript
h3 :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `h3_`

``` purescript
h3_ :: forall p i. [HTML p i] -> HTML p i
```


#### `h4`

``` purescript
h4 :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `h4_`

``` purescript
h4_ :: forall p i. [HTML p i] -> HTML p i
```


#### `h5`

``` purescript
h5 :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `h5_`

``` purescript
h5_ :: forall p i. [HTML p i] -> HTML p i
```


#### `h6`

``` purescript
h6 :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `h6_`

``` purescript
h6_ :: forall p i. [HTML p i] -> HTML p i
```


#### `head`

``` purescript
head :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `head_`

``` purescript
head_ :: forall p i. [HTML p i] -> HTML p i
```


#### `header`

``` purescript
header :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `header_`

``` purescript
header_ :: forall p i. [HTML p i] -> HTML p i
```


#### `hr`

``` purescript
hr :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `hr_`

``` purescript
hr_ :: forall p i. [HTML p i] -> HTML p i
```


#### `html`

``` purescript
html :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `html_`

``` purescript
html_ :: forall p i. [HTML p i] -> HTML p i
```


#### `i`

``` purescript
i :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `i_`

``` purescript
i_ :: forall p i. [HTML p i] -> HTML p i
```


#### `iframe`

``` purescript
iframe :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `iframe_`

``` purescript
iframe_ :: forall p i. [HTML p i] -> HTML p i
```


#### `img`

``` purescript
img :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `img_`

``` purescript
img_ :: forall p i. [HTML p i] -> HTML p i
```


#### `input`

``` purescript
input :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `input_`

``` purescript
input_ :: forall p i. [HTML p i] -> HTML p i
```


#### `ins`

``` purescript
ins :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `ins_`

``` purescript
ins_ :: forall p i. [HTML p i] -> HTML p i
```


#### `kbd`

``` purescript
kbd :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `kbd_`

``` purescript
kbd_ :: forall p i. [HTML p i] -> HTML p i
```


#### `keygen`

``` purescript
keygen :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `keygen_`

``` purescript
keygen_ :: forall p i. [HTML p i] -> HTML p i
```


#### `label`

``` purescript
label :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `label_`

``` purescript
label_ :: forall p i. [HTML p i] -> HTML p i
```


#### `legend`

``` purescript
legend :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `legend_`

``` purescript
legend_ :: forall p i. [HTML p i] -> HTML p i
```


#### `li`

``` purescript
li :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `li_`

``` purescript
li_ :: forall p i. [HTML p i] -> HTML p i
```


#### `link`

``` purescript
link :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `link_`

``` purescript
link_ :: forall p i. [HTML p i] -> HTML p i
```


#### `main`

``` purescript
main :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `main_`

``` purescript
main_ :: forall p i. [HTML p i] -> HTML p i
```


#### `map`

``` purescript
map :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `map_`

``` purescript
map_ :: forall p i. [HTML p i] -> HTML p i
```


#### `mark`

``` purescript
mark :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `mark_`

``` purescript
mark_ :: forall p i. [HTML p i] -> HTML p i
```


#### `menu`

``` purescript
menu :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `menu_`

``` purescript
menu_ :: forall p i. [HTML p i] -> HTML p i
```


#### `menuitem`

``` purescript
menuitem :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `menuitem_`

``` purescript
menuitem_ :: forall p i. [HTML p i] -> HTML p i
```


#### `meta`

``` purescript
meta :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `meta_`

``` purescript
meta_ :: forall p i. [HTML p i] -> HTML p i
```


#### `meter`

``` purescript
meter :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `meter_`

``` purescript
meter_ :: forall p i. [HTML p i] -> HTML p i
```


#### `nav`

``` purescript
nav :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `nav_`

``` purescript
nav_ :: forall p i. [HTML p i] -> HTML p i
```


#### `noframes`

``` purescript
noframes :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `noframes_`

``` purescript
noframes_ :: forall p i. [HTML p i] -> HTML p i
```


#### `noscript`

``` purescript
noscript :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `noscript_`

``` purescript
noscript_ :: forall p i. [HTML p i] -> HTML p i
```


#### `object`

``` purescript
object :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `object_`

``` purescript
object_ :: forall p i. [HTML p i] -> HTML p i
```


#### `ol`

``` purescript
ol :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `ol_`

``` purescript
ol_ :: forall p i. [HTML p i] -> HTML p i
```


#### `optgroup`

``` purescript
optgroup :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `optgroup_`

``` purescript
optgroup_ :: forall p i. [HTML p i] -> HTML p i
```


#### `option`

``` purescript
option :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `option_`

``` purescript
option_ :: forall p i. [HTML p i] -> HTML p i
```


#### `output`

``` purescript
output :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `output_`

``` purescript
output_ :: forall p i. [HTML p i] -> HTML p i
```


#### `p`

``` purescript
p :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `p_`

``` purescript
p_ :: forall p i. [HTML p i] -> HTML p i
```


#### `param`

``` purescript
param :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `param_`

``` purescript
param_ :: forall p i. [HTML p i] -> HTML p i
```


#### `pre`

``` purescript
pre :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `pre_`

``` purescript
pre_ :: forall p i. [HTML p i] -> HTML p i
```


#### `progress`

``` purescript
progress :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `progress_`

``` purescript
progress_ :: forall p i. [HTML p i] -> HTML p i
```


#### `q`

``` purescript
q :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `q_`

``` purescript
q_ :: forall p i. [HTML p i] -> HTML p i
```


#### `rp`

``` purescript
rp :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `rp_`

``` purescript
rp_ :: forall p i. [HTML p i] -> HTML p i
```


#### `rt`

``` purescript
rt :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `rt_`

``` purescript
rt_ :: forall p i. [HTML p i] -> HTML p i
```


#### `ruby`

``` purescript
ruby :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `ruby_`

``` purescript
ruby_ :: forall p i. [HTML p i] -> HTML p i
```


#### `s`

``` purescript
s :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `s_`

``` purescript
s_ :: forall p i. [HTML p i] -> HTML p i
```


#### `samp`

``` purescript
samp :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `samp_`

``` purescript
samp_ :: forall p i. [HTML p i] -> HTML p i
```


#### `script`

``` purescript
script :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `script_`

``` purescript
script_ :: forall p i. [HTML p i] -> HTML p i
```


#### `section`

``` purescript
section :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `section_`

``` purescript
section_ :: forall p i. [HTML p i] -> HTML p i
```


#### `select`

``` purescript
select :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `select_`

``` purescript
select_ :: forall p i. [HTML p i] -> HTML p i
```


#### `small`

``` purescript
small :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `small_`

``` purescript
small_ :: forall p i. [HTML p i] -> HTML p i
```


#### `source`

``` purescript
source :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `source_`

``` purescript
source_ :: forall p i. [HTML p i] -> HTML p i
```


#### `span`

``` purescript
span :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `span_`

``` purescript
span_ :: forall p i. [HTML p i] -> HTML p i
```


#### `strike`

``` purescript
strike :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `strike_`

``` purescript
strike_ :: forall p i. [HTML p i] -> HTML p i
```


#### `strong`

``` purescript
strong :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `strong_`

``` purescript
strong_ :: forall p i. [HTML p i] -> HTML p i
```


#### `style`

``` purescript
style :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `style_`

``` purescript
style_ :: forall p i. [HTML p i] -> HTML p i
```


#### `sub`

``` purescript
sub :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `sub_`

``` purescript
sub_ :: forall p i. [HTML p i] -> HTML p i
```


#### `summary`

``` purescript
summary :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `summary_`

``` purescript
summary_ :: forall p i. [HTML p i] -> HTML p i
```


#### `sup`

``` purescript
sup :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `sup_`

``` purescript
sup_ :: forall p i. [HTML p i] -> HTML p i
```


#### `table`

``` purescript
table :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `table_`

``` purescript
table_ :: forall p i. [HTML p i] -> HTML p i
```


#### `tbody`

``` purescript
tbody :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `tbody_`

``` purescript
tbody_ :: forall p i. [HTML p i] -> HTML p i
```


#### `td`

``` purescript
td :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `td_`

``` purescript
td_ :: forall p i. [HTML p i] -> HTML p i
```


#### `textarea`

``` purescript
textarea :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `textarea_`

``` purescript
textarea_ :: forall p i. [HTML p i] -> HTML p i
```


#### `tfoot`

``` purescript
tfoot :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `tfoot_`

``` purescript
tfoot_ :: forall p i. [HTML p i] -> HTML p i
```


#### `th`

``` purescript
th :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `th_`

``` purescript
th_ :: forall p i. [HTML p i] -> HTML p i
```


#### `thead`

``` purescript
thead :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `thead_`

``` purescript
thead_ :: forall p i. [HTML p i] -> HTML p i
```


#### `time`

``` purescript
time :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `time_`

``` purescript
time_ :: forall p i. [HTML p i] -> HTML p i
```


#### `title`

``` purescript
title :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `title_`

``` purescript
title_ :: forall p i. [HTML p i] -> HTML p i
```


#### `tr`

``` purescript
tr :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `tr_`

``` purescript
tr_ :: forall p i. [HTML p i] -> HTML p i
```


#### `track`

``` purescript
track :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `track_`

``` purescript
track_ :: forall p i. [HTML p i] -> HTML p i
```


#### `tt`

``` purescript
tt :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `tt_`

``` purescript
tt_ :: forall p i. [HTML p i] -> HTML p i
```


#### `u`

``` purescript
u :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `u_`

``` purescript
u_ :: forall p i. [HTML p i] -> HTML p i
```


#### `ul`

``` purescript
ul :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `ul_`

``` purescript
ul_ :: forall p i. [HTML p i] -> HTML p i
```


#### `var`

``` purescript
var :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `var_`

``` purescript
var_ :: forall p i. [HTML p i] -> HTML p i
```


#### `video`

``` purescript
video :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `video_`

``` purescript
video_ :: forall p i. [HTML p i] -> HTML p i
```


#### `wbr`

``` purescript
wbr :: forall p i. Attr i -> [HTML p i] -> HTML p i
```


#### `wbr_`

``` purescript
wbr_ :: forall p i. [HTML p i] -> HTML p i
```



## Module Halogen.HTML.Attributes


This module enumerates some common HTML attributes, and provides additional
helper functions for working with CSS classes.

#### `ClassName`

``` purescript
newtype ClassName
```

A wrapper for strings which are used as CSS classes

#### `className`

``` purescript
className :: String -> ClassName
```

#### `runClassName`

``` purescript
runClassName :: ClassName -> String
```

Unpack a class name

#### `alt`

``` purescript
alt :: forall i. String -> H.Attr i
```


#### `charset`

``` purescript
charset :: forall i. String -> H.Attr i
```


#### `class_`

``` purescript
class_ :: forall i. ClassName -> H.Attr i
```


#### `classes`

``` purescript
classes :: forall i. [ClassName] -> H.Attr i
```


#### `content`

``` purescript
content :: forall i. String -> H.Attr i
```


#### `for`

``` purescript
for :: forall i. String -> H.Attr i
```


#### `height`

``` purescript
height :: forall i. Number -> H.Attr i
```


#### `href`

``` purescript
href :: forall i. String -> H.Attr i
```


#### `httpEquiv`

``` purescript
httpEquiv :: forall i. String -> H.Attr i
```


#### `id_`

``` purescript
id_ :: forall i. String -> H.Attr i
```


#### `name`

``` purescript
name :: forall i. String -> H.Attr i
```


#### `rel`

``` purescript
rel :: forall i. String -> H.Attr i
```


#### `src`

``` purescript
src :: forall i. String -> H.Attr i
```


#### `target`

``` purescript
target :: forall i. String -> H.Attr i
```


#### `title`

``` purescript
title :: forall i. String -> H.Attr i
```


#### `type_`

``` purescript
type_ :: forall i. String -> H.Attr i
```


#### `value`

``` purescript
value :: forall i. String -> H.Attr i
```


#### `width`

``` purescript
width :: forall i. Number -> H.Attr i
```


#### `disabled`

``` purescript
disabled :: forall i. Boolean -> H.Attr i
```


#### `enabled`

``` purescript
enabled :: forall i. Boolean -> H.Attr i
```


#### `checked`

``` purescript
checked :: forall i. Boolean -> H.Attr i
```


#### `placeholder`

``` purescript
placeholder :: forall i. String -> H.Attr i
```


#### `style`

``` purescript
style :: forall i. StrMap String -> H.Attr i
```




