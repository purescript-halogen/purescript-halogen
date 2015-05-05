# Module Documentation

## Module Halogen.HTML


This module defines the HTML types required by the Halogen library, and provides
smart constructors for HTML5 elements.

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

#### `HTML`

``` purescript
data HTML i
  = Text String
  | Element TagName [A.Attr i] [HTML i]
```

An initial encoding of HTML nodes.

#### `functorHTML`

``` purescript
instance functorHTML :: Functor HTML
```


#### `text`

``` purescript
text :: forall i. String -> HTML i
```


#### `element`

``` purescript
element :: forall i. TagName -> [A.Attr i] -> [HTML i] -> HTML i
```


#### `a`

``` purescript
a :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `a_`

``` purescript
a_ :: forall i. [HTML i] -> HTML i
```


#### `abbr`

``` purescript
abbr :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `abbr_`

``` purescript
abbr_ :: forall i. [HTML i] -> HTML i
```


#### `acronym`

``` purescript
acronym :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `acronym_`

``` purescript
acronym_ :: forall i. [HTML i] -> HTML i
```


#### `address`

``` purescript
address :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `address_`

``` purescript
address_ :: forall i. [HTML i] -> HTML i
```


#### `applet`

``` purescript
applet :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `applet_`

``` purescript
applet_ :: forall i. [HTML i] -> HTML i
```


#### `area`

``` purescript
area :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `area_`

``` purescript
area_ :: forall i. [HTML i] -> HTML i
```


#### `article`

``` purescript
article :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `article_`

``` purescript
article_ :: forall i. [HTML i] -> HTML i
```


#### `aside`

``` purescript
aside :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `aside_`

``` purescript
aside_ :: forall i. [HTML i] -> HTML i
```


#### `audio`

``` purescript
audio :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `audio_`

``` purescript
audio_ :: forall i. [HTML i] -> HTML i
```


#### `b`

``` purescript
b :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `b_`

``` purescript
b_ :: forall i. [HTML i] -> HTML i
```


#### `base`

``` purescript
base :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `base_`

``` purescript
base_ :: forall i. [HTML i] -> HTML i
```


#### `basefont`

``` purescript
basefont :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `basefont_`

``` purescript
basefont_ :: forall i. [HTML i] -> HTML i
```


#### `bdi`

``` purescript
bdi :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `bdi_`

``` purescript
bdi_ :: forall i. [HTML i] -> HTML i
```


#### `bdo`

``` purescript
bdo :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `bdo_`

``` purescript
bdo_ :: forall i. [HTML i] -> HTML i
```


#### `big`

``` purescript
big :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `big_`

``` purescript
big_ :: forall i. [HTML i] -> HTML i
```


#### `blockquote`

``` purescript
blockquote :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `blockquote_`

``` purescript
blockquote_ :: forall i. [HTML i] -> HTML i
```


#### `body`

``` purescript
body :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `body_`

``` purescript
body_ :: forall i. [HTML i] -> HTML i
```


#### `br`

``` purescript
br :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `br_`

``` purescript
br_ :: forall i. [HTML i] -> HTML i
```


#### `button`

``` purescript
button :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `button_`

``` purescript
button_ :: forall i. [HTML i] -> HTML i
```


#### `canvas`

``` purescript
canvas :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `canvas_`

``` purescript
canvas_ :: forall i. [HTML i] -> HTML i
```


#### `caption`

``` purescript
caption :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `caption_`

``` purescript
caption_ :: forall i. [HTML i] -> HTML i
```


#### `center`

``` purescript
center :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `center_`

``` purescript
center_ :: forall i. [HTML i] -> HTML i
```


#### `cite`

``` purescript
cite :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `cite_`

``` purescript
cite_ :: forall i. [HTML i] -> HTML i
```


#### `code`

``` purescript
code :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `code_`

``` purescript
code_ :: forall i. [HTML i] -> HTML i
```


#### `col`

``` purescript
col :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `col_`

``` purescript
col_ :: forall i. [HTML i] -> HTML i
```


#### `colgroup`

``` purescript
colgroup :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `colgroup_`

``` purescript
colgroup_ :: forall i. [HTML i] -> HTML i
```


#### `datalist`

``` purescript
datalist :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `datalist_`

``` purescript
datalist_ :: forall i. [HTML i] -> HTML i
```


#### `dd`

``` purescript
dd :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `dd_`

``` purescript
dd_ :: forall i. [HTML i] -> HTML i
```


#### `del`

``` purescript
del :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `del_`

``` purescript
del_ :: forall i. [HTML i] -> HTML i
```


#### `details`

``` purescript
details :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `details_`

``` purescript
details_ :: forall i. [HTML i] -> HTML i
```


#### `dfn`

``` purescript
dfn :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `dfn_`

``` purescript
dfn_ :: forall i. [HTML i] -> HTML i
```


#### `dialog`

``` purescript
dialog :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `dialog_`

``` purescript
dialog_ :: forall i. [HTML i] -> HTML i
```


#### `dir`

``` purescript
dir :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `dir_`

``` purescript
dir_ :: forall i. [HTML i] -> HTML i
```


#### `div`

``` purescript
div :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `div_`

``` purescript
div_ :: forall i. [HTML i] -> HTML i
```


#### `dl`

``` purescript
dl :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `dl_`

``` purescript
dl_ :: forall i. [HTML i] -> HTML i
```


#### `dt`

``` purescript
dt :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `dt_`

``` purescript
dt_ :: forall i. [HTML i] -> HTML i
```


#### `em`

``` purescript
em :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `em_`

``` purescript
em_ :: forall i. [HTML i] -> HTML i
```


#### `embed`

``` purescript
embed :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `embed_`

``` purescript
embed_ :: forall i. [HTML i] -> HTML i
```


#### `fieldset`

``` purescript
fieldset :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `fieldset_`

``` purescript
fieldset_ :: forall i. [HTML i] -> HTML i
```


#### `figcaption`

``` purescript
figcaption :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `figcaption_`

``` purescript
figcaption_ :: forall i. [HTML i] -> HTML i
```


#### `figure`

``` purescript
figure :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `figure_`

``` purescript
figure_ :: forall i. [HTML i] -> HTML i
```


#### `font`

``` purescript
font :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `font_`

``` purescript
font_ :: forall i. [HTML i] -> HTML i
```


#### `footer`

``` purescript
footer :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `footer_`

``` purescript
footer_ :: forall i. [HTML i] -> HTML i
```


#### `form`

``` purescript
form :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `form_`

``` purescript
form_ :: forall i. [HTML i] -> HTML i
```


#### `frame`

``` purescript
frame :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `frame_`

``` purescript
frame_ :: forall i. [HTML i] -> HTML i
```


#### `frameset`

``` purescript
frameset :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `frameset_`

``` purescript
frameset_ :: forall i. [HTML i] -> HTML i
```


#### `h1`

``` purescript
h1 :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `h1_`

``` purescript
h1_ :: forall i. [HTML i] -> HTML i
```


#### `h2`

``` purescript
h2 :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `h2_`

``` purescript
h2_ :: forall i. [HTML i] -> HTML i
```


#### `h3`

``` purescript
h3 :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `h3_`

``` purescript
h3_ :: forall i. [HTML i] -> HTML i
```


#### `h4`

``` purescript
h4 :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `h4_`

``` purescript
h4_ :: forall i. [HTML i] -> HTML i
```


#### `h5`

``` purescript
h5 :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `h5_`

``` purescript
h5_ :: forall i. [HTML i] -> HTML i
```


#### `h6`

``` purescript
h6 :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `h6_`

``` purescript
h6_ :: forall i. [HTML i] -> HTML i
```


#### `head`

``` purescript
head :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `head_`

``` purescript
head_ :: forall i. [HTML i] -> HTML i
```


#### `header`

``` purescript
header :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `header_`

``` purescript
header_ :: forall i. [HTML i] -> HTML i
```


#### `hr`

``` purescript
hr :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `hr_`

``` purescript
hr_ :: forall i. [HTML i] -> HTML i
```


#### `html`

``` purescript
html :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `html_`

``` purescript
html_ :: forall i. [HTML i] -> HTML i
```


#### `i`

``` purescript
i :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `i_`

``` purescript
i_ :: forall i. [HTML i] -> HTML i
```


#### `iframe`

``` purescript
iframe :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `iframe_`

``` purescript
iframe_ :: forall i. [HTML i] -> HTML i
```


#### `img`

``` purescript
img :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `img_`

``` purescript
img_ :: forall i. [HTML i] -> HTML i
```


#### `input`

``` purescript
input :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `input_`

``` purescript
input_ :: forall i. [HTML i] -> HTML i
```


#### `ins`

``` purescript
ins :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `ins_`

``` purescript
ins_ :: forall i. [HTML i] -> HTML i
```


#### `kbd`

``` purescript
kbd :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `kbd_`

``` purescript
kbd_ :: forall i. [HTML i] -> HTML i
```


#### `keygen`

``` purescript
keygen :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `keygen_`

``` purescript
keygen_ :: forall i. [HTML i] -> HTML i
```


#### `label`

``` purescript
label :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `label_`

``` purescript
label_ :: forall i. [HTML i] -> HTML i
```


#### `legend`

``` purescript
legend :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `legend_`

``` purescript
legend_ :: forall i. [HTML i] -> HTML i
```


#### `li`

``` purescript
li :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `li_`

``` purescript
li_ :: forall i. [HTML i] -> HTML i
```


#### `link`

``` purescript
link :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `link_`

``` purescript
link_ :: forall i. [HTML i] -> HTML i
```


#### `main`

``` purescript
main :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `main_`

``` purescript
main_ :: forall i. [HTML i] -> HTML i
```


#### `map`

``` purescript
map :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `map_`

``` purescript
map_ :: forall i. [HTML i] -> HTML i
```


#### `mark`

``` purescript
mark :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `mark_`

``` purescript
mark_ :: forall i. [HTML i] -> HTML i
```


#### `menu`

``` purescript
menu :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `menu_`

``` purescript
menu_ :: forall i. [HTML i] -> HTML i
```


#### `menuitem`

``` purescript
menuitem :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `menuitem_`

``` purescript
menuitem_ :: forall i. [HTML i] -> HTML i
```


#### `meta`

``` purescript
meta :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `meta_`

``` purescript
meta_ :: forall i. [HTML i] -> HTML i
```


#### `meter`

``` purescript
meter :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `meter_`

``` purescript
meter_ :: forall i. [HTML i] -> HTML i
```


#### `nav`

``` purescript
nav :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `nav_`

``` purescript
nav_ :: forall i. [HTML i] -> HTML i
```


#### `noframes`

``` purescript
noframes :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `noframes_`

``` purescript
noframes_ :: forall i. [HTML i] -> HTML i
```


#### `noscript`

``` purescript
noscript :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `noscript_`

``` purescript
noscript_ :: forall i. [HTML i] -> HTML i
```


#### `object`

``` purescript
object :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `object_`

``` purescript
object_ :: forall i. [HTML i] -> HTML i
```


#### `ol`

``` purescript
ol :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `ol_`

``` purescript
ol_ :: forall i. [HTML i] -> HTML i
```


#### `optgroup`

``` purescript
optgroup :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `optgroup_`

``` purescript
optgroup_ :: forall i. [HTML i] -> HTML i
```


#### `option`

``` purescript
option :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `option_`

``` purescript
option_ :: forall i. [HTML i] -> HTML i
```


#### `output`

``` purescript
output :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `output_`

``` purescript
output_ :: forall i. [HTML i] -> HTML i
```


#### `p`

``` purescript
p :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `p_`

``` purescript
p_ :: forall i. [HTML i] -> HTML i
```


#### `param`

``` purescript
param :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `param_`

``` purescript
param_ :: forall i. [HTML i] -> HTML i
```


#### `pre`

``` purescript
pre :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `pre_`

``` purescript
pre_ :: forall i. [HTML i] -> HTML i
```


#### `progress`

``` purescript
progress :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `progress_`

``` purescript
progress_ :: forall i. [HTML i] -> HTML i
```


#### `q`

``` purescript
q :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `q_`

``` purescript
q_ :: forall i. [HTML i] -> HTML i
```


#### `rp`

``` purescript
rp :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `rp_`

``` purescript
rp_ :: forall i. [HTML i] -> HTML i
```


#### `rt`

``` purescript
rt :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `rt_`

``` purescript
rt_ :: forall i. [HTML i] -> HTML i
```


#### `ruby`

``` purescript
ruby :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `ruby_`

``` purescript
ruby_ :: forall i. [HTML i] -> HTML i
```


#### `s`

``` purescript
s :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `s_`

``` purescript
s_ :: forall i. [HTML i] -> HTML i
```


#### `samp`

``` purescript
samp :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `samp_`

``` purescript
samp_ :: forall i. [HTML i] -> HTML i
```


#### `script`

``` purescript
script :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `script_`

``` purescript
script_ :: forall i. [HTML i] -> HTML i
```


#### `section`

``` purescript
section :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `section_`

``` purescript
section_ :: forall i. [HTML i] -> HTML i
```


#### `select`

``` purescript
select :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `select_`

``` purescript
select_ :: forall i. [HTML i] -> HTML i
```


#### `small`

``` purescript
small :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `small_`

``` purescript
small_ :: forall i. [HTML i] -> HTML i
```


#### `source`

``` purescript
source :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `source_`

``` purescript
source_ :: forall i. [HTML i] -> HTML i
```


#### `span`

``` purescript
span :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `span_`

``` purescript
span_ :: forall i. [HTML i] -> HTML i
```


#### `strike`

``` purescript
strike :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `strike_`

``` purescript
strike_ :: forall i. [HTML i] -> HTML i
```


#### `strong`

``` purescript
strong :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `strong_`

``` purescript
strong_ :: forall i. [HTML i] -> HTML i
```


#### `style`

``` purescript
style :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `style_`

``` purescript
style_ :: forall i. [HTML i] -> HTML i
```


#### `sub`

``` purescript
sub :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `sub_`

``` purescript
sub_ :: forall i. [HTML i] -> HTML i
```


#### `summary`

``` purescript
summary :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `summary_`

``` purescript
summary_ :: forall i. [HTML i] -> HTML i
```


#### `sup`

``` purescript
sup :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `sup_`

``` purescript
sup_ :: forall i. [HTML i] -> HTML i
```


#### `table`

``` purescript
table :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `table_`

``` purescript
table_ :: forall i. [HTML i] -> HTML i
```


#### `tbody`

``` purescript
tbody :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `tbody_`

``` purescript
tbody_ :: forall i. [HTML i] -> HTML i
```


#### `td`

``` purescript
td :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `td_`

``` purescript
td_ :: forall i. [HTML i] -> HTML i
```


#### `textarea`

``` purescript
textarea :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `textarea_`

``` purescript
textarea_ :: forall i. [HTML i] -> HTML i
```


#### `tfoot`

``` purescript
tfoot :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `tfoot_`

``` purescript
tfoot_ :: forall i. [HTML i] -> HTML i
```


#### `th`

``` purescript
th :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `th_`

``` purescript
th_ :: forall i. [HTML i] -> HTML i
```


#### `thead`

``` purescript
thead :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `thead_`

``` purescript
thead_ :: forall i. [HTML i] -> HTML i
```


#### `time`

``` purescript
time :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `time_`

``` purescript
time_ :: forall i. [HTML i] -> HTML i
```


#### `title`

``` purescript
title :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `title_`

``` purescript
title_ :: forall i. [HTML i] -> HTML i
```


#### `tr`

``` purescript
tr :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `tr_`

``` purescript
tr_ :: forall i. [HTML i] -> HTML i
```


#### `track`

``` purescript
track :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `track_`

``` purescript
track_ :: forall i. [HTML i] -> HTML i
```


#### `tt`

``` purescript
tt :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `tt_`

``` purescript
tt_ :: forall i. [HTML i] -> HTML i
```


#### `u`

``` purescript
u :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `u_`

``` purescript
u_ :: forall i. [HTML i] -> HTML i
```


#### `ul`

``` purescript
ul :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `ul_`

``` purescript
ul_ :: forall i. [HTML i] -> HTML i
```


#### `var`

``` purescript
var :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `var_`

``` purescript
var_ :: forall i. [HTML i] -> HTML i
```


#### `video`

``` purescript
video :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `video_`

``` purescript
video_ :: forall i. [HTML i] -> HTML i
```


#### `wbr`

``` purescript
wbr :: forall i. [A.Attr i] -> [HTML i] -> HTML i
```


#### `wbr_`

``` purescript
wbr_ :: forall i. [HTML i] -> HTML i
```



## Module Halogen.HTML.Attributes


This module enumerates some common HTML attributes, and provides additional
helper functions for working with CSS classes.

#### `ExistsR`

``` purescript
data ExistsR (f :: # * -> *)
```

We need a variant of `Exists` which works for type constructors which accept a _row_ of types.

#### `runExistsR`

``` purescript
runExistsR :: forall f r. (forall a. f a -> r) -> ExistsR f -> r
```


#### `mkExistsR`

``` purescript
mkExistsR :: forall f a. f a -> ExistsR f
```


#### `AttrF`

``` purescript
data AttrF value
  = AttrF (AttributeName value -> value -> String) (AttributeName value) value
```

The data which represents a typed attribute, hidden inside an existential package in
the `Attr` type.

#### `HandlerF`

``` purescript
data HandlerF i fields
  = HandlerF (EventName fields) (Event fields -> EventHandler i)
```

The data which represents a typed event handler, hidden inside an existential package in
the `Attr` type.

#### `Attr`

``` purescript
data Attr i
  = Attr (Exists AttrF)
  | Handler (ExistsR (HandlerF i))
  | Initializer i
  | Finalizer i
```

A single attribute is either

- An attribute
- An event handler

#### `functorAttr`

``` purescript
instance functorAttr :: Functor Attr
```


#### `attr`

``` purescript
attr :: forall value i. (IsAttribute value) => AttributeName value -> value -> Attr i
```

Create an attribute

#### `handler`

``` purescript
handler :: forall fields i. EventName fields -> (Event fields -> EventHandler i) -> Attr i
```

Create an event handler

#### `initializer`

``` purescript
initializer :: forall i. i -> Attr i
```

Attach an initializer.

#### `finalizer`

``` purescript
finalizer :: forall i. i -> Attr i
```

Attach a finalizer.

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

Create an attribute name

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

#### `Styles`

``` purescript
newtype Styles
```

A newtype for CSS styles

#### `styles`

``` purescript
styles :: StrMap String -> Styles
```

#### `runStyles`

``` purescript
runStyles :: Styles -> StrMap String
```

Unpack CSS styles

#### `IsAttribute`

``` purescript
class IsAttribute a where
  toAttrString :: AttributeName a -> a -> String
```

This type class captures those types which can be used as attribute values.

`toAttrString` is an alternative to `show`, and is needed by `attr` in the string renderer.

#### `stringIsAttribute`

``` purescript
instance stringIsAttribute :: IsAttribute String
```


#### `numberIsAttribute`

``` purescript
instance numberIsAttribute :: IsAttribute Number
```


#### `booleanIsAttribute`

``` purescript
instance booleanIsAttribute :: IsAttribute Boolean
```


#### `stylesIsAttribute`

``` purescript
instance stylesIsAttribute :: IsAttribute Styles
```


#### `key`

``` purescript
key :: forall i. String -> Attr i
```

#### `alt`

``` purescript
alt :: forall i. String -> Attr i
```


#### `charset`

``` purescript
charset :: forall i. String -> Attr i
```


#### `class_`

``` purescript
class_ :: forall i. ClassName -> Attr i
```


#### `classes`

``` purescript
classes :: forall i. [ClassName] -> Attr i
```


#### `colSpan`

``` purescript
colSpan :: forall i. Number -> Attr i
```


#### `rowSpan`

``` purescript
rowSpan :: forall i. Number -> Attr i
```


#### `content`

``` purescript
content :: forall i. String -> Attr i
```


#### `for`

``` purescript
for :: forall i. String -> Attr i
```


#### `height`

``` purescript
height :: forall i. Number -> Attr i
```


#### `href`

``` purescript
href :: forall i. String -> Attr i
```


#### `httpEquiv`

``` purescript
httpEquiv :: forall i. String -> Attr i
```


#### `id_`

``` purescript
id_ :: forall i. String -> Attr i
```


#### `name`

``` purescript
name :: forall i. String -> Attr i
```


#### `rel`

``` purescript
rel :: forall i. String -> Attr i
```


#### `src`

``` purescript
src :: forall i. String -> Attr i
```


#### `target`

``` purescript
target :: forall i. String -> Attr i
```


#### `title`

``` purescript
title :: forall i. String -> Attr i
```


#### `type_`

``` purescript
type_ :: forall i. String -> Attr i
```


#### `value`

``` purescript
value :: forall i. String -> Attr i
```


#### `width`

``` purescript
width :: forall i. Number -> Attr i
```


#### `disabled`

``` purescript
disabled :: forall i. Boolean -> Attr i
```


#### `required`

``` purescript
required :: forall i. Boolean -> Attr i
```


#### `readonly`

``` purescript
readonly :: forall i. Boolean -> Attr i
```


#### `spellcheck`

``` purescript
spellcheck :: forall i. Boolean -> Attr i
```


#### `enabled`

``` purescript
enabled :: forall i. Boolean -> Attr i
```


#### `checked`

``` purescript
checked :: forall i. Boolean -> Attr i
```


#### `selected`

``` purescript
selected :: forall i. Boolean -> Attr i
```


#### `placeholder`

``` purescript
placeholder :: forall i. String -> Attr i
```


#### `style`

``` purescript
style :: forall i. Styles -> Attr i
```




