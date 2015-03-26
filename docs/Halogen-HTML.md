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

#### `HTMLRepr`

``` purescript
class (Bifunctor node) <= HTMLRepr node where
  text :: forall p i. String -> node p i
  placeholder :: forall p i. p -> node p i
  element :: forall p i. TagName -> [A.Attr i] -> [node p i] -> node p i
```

This type class encodes _representations_ of HTML nodes

#### `a`

``` purescript
a :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `a_`

``` purescript
a_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `abbr`

``` purescript
abbr :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `abbr_`

``` purescript
abbr_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `acronym`

``` purescript
acronym :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `acronym_`

``` purescript
acronym_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `address`

``` purescript
address :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `address_`

``` purescript
address_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `applet`

``` purescript
applet :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `applet_`

``` purescript
applet_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `area`

``` purescript
area :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `area_`

``` purescript
area_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `article`

``` purescript
article :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `article_`

``` purescript
article_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `aside`

``` purescript
aside :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `aside_`

``` purescript
aside_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `audio`

``` purescript
audio :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `audio_`

``` purescript
audio_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `b`

``` purescript
b :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `b_`

``` purescript
b_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `base`

``` purescript
base :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `base_`

``` purescript
base_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `basefont`

``` purescript
basefont :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `basefont_`

``` purescript
basefont_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `bdi`

``` purescript
bdi :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `bdi_`

``` purescript
bdi_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `bdo`

``` purescript
bdo :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `bdo_`

``` purescript
bdo_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `big`

``` purescript
big :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `big_`

``` purescript
big_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `blockquote`

``` purescript
blockquote :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `blockquote_`

``` purescript
blockquote_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `body`

``` purescript
body :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `body_`

``` purescript
body_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `br`

``` purescript
br :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `br_`

``` purescript
br_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `button`

``` purescript
button :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `button_`

``` purescript
button_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `canvas`

``` purescript
canvas :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `canvas_`

``` purescript
canvas_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `caption`

``` purescript
caption :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `caption_`

``` purescript
caption_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `center`

``` purescript
center :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `center_`

``` purescript
center_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `cite`

``` purescript
cite :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `cite_`

``` purescript
cite_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `code`

``` purescript
code :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `code_`

``` purescript
code_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `col`

``` purescript
col :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `col_`

``` purescript
col_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `colgroup`

``` purescript
colgroup :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `colgroup_`

``` purescript
colgroup_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `datalist`

``` purescript
datalist :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `datalist_`

``` purescript
datalist_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `dd`

``` purescript
dd :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `dd_`

``` purescript
dd_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `del`

``` purescript
del :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `del_`

``` purescript
del_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `details`

``` purescript
details :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `details_`

``` purescript
details_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `dfn`

``` purescript
dfn :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `dfn_`

``` purescript
dfn_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `dialog`

``` purescript
dialog :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `dialog_`

``` purescript
dialog_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `dir`

``` purescript
dir :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `dir_`

``` purescript
dir_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `div`

``` purescript
div :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `div_`

``` purescript
div_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `dl`

``` purescript
dl :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `dl_`

``` purescript
dl_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `dt`

``` purescript
dt :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `dt_`

``` purescript
dt_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `em`

``` purescript
em :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `em_`

``` purescript
em_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `embed`

``` purescript
embed :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `embed_`

``` purescript
embed_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `fieldset`

``` purescript
fieldset :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `fieldset_`

``` purescript
fieldset_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `figcaption`

``` purescript
figcaption :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `figcaption_`

``` purescript
figcaption_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `figure`

``` purescript
figure :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `figure_`

``` purescript
figure_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `font`

``` purescript
font :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `font_`

``` purescript
font_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `footer`

``` purescript
footer :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `footer_`

``` purescript
footer_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `form`

``` purescript
form :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `form_`

``` purescript
form_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `frame`

``` purescript
frame :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `frame_`

``` purescript
frame_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `frameset`

``` purescript
frameset :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `frameset_`

``` purescript
frameset_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `h1`

``` purescript
h1 :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `h1_`

``` purescript
h1_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `h2`

``` purescript
h2 :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `h2_`

``` purescript
h2_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `h3`

``` purescript
h3 :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `h3_`

``` purescript
h3_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `h4`

``` purescript
h4 :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `h4_`

``` purescript
h4_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `h5`

``` purescript
h5 :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `h5_`

``` purescript
h5_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `h6`

``` purescript
h6 :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `h6_`

``` purescript
h6_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `head`

``` purescript
head :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `head_`

``` purescript
head_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `header`

``` purescript
header :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `header_`

``` purescript
header_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `hr`

``` purescript
hr :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `hr_`

``` purescript
hr_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `html`

``` purescript
html :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `html_`

``` purescript
html_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `i`

``` purescript
i :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `i_`

``` purescript
i_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `iframe`

``` purescript
iframe :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `iframe_`

``` purescript
iframe_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `img`

``` purescript
img :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `img_`

``` purescript
img_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `input`

``` purescript
input :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `input_`

``` purescript
input_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `ins`

``` purescript
ins :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `ins_`

``` purescript
ins_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `kbd`

``` purescript
kbd :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `kbd_`

``` purescript
kbd_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `keygen`

``` purescript
keygen :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `keygen_`

``` purescript
keygen_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `label`

``` purescript
label :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `label_`

``` purescript
label_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `legend`

``` purescript
legend :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `legend_`

``` purescript
legend_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `li`

``` purescript
li :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `li_`

``` purescript
li_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `link`

``` purescript
link :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `link_`

``` purescript
link_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `main`

``` purescript
main :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `main_`

``` purescript
main_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `map`

``` purescript
map :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `map_`

``` purescript
map_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `mark`

``` purescript
mark :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `mark_`

``` purescript
mark_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `menu`

``` purescript
menu :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `menu_`

``` purescript
menu_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `menuitem`

``` purescript
menuitem :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `menuitem_`

``` purescript
menuitem_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `meta`

``` purescript
meta :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `meta_`

``` purescript
meta_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `meter`

``` purescript
meter :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `meter_`

``` purescript
meter_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `nav`

``` purescript
nav :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `nav_`

``` purescript
nav_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `noframes`

``` purescript
noframes :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `noframes_`

``` purescript
noframes_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `noscript`

``` purescript
noscript :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `noscript_`

``` purescript
noscript_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `object`

``` purescript
object :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `object_`

``` purescript
object_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `ol`

``` purescript
ol :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `ol_`

``` purescript
ol_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `optgroup`

``` purescript
optgroup :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `optgroup_`

``` purescript
optgroup_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `option`

``` purescript
option :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `option_`

``` purescript
option_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `output`

``` purescript
output :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `output_`

``` purescript
output_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `p`

``` purescript
p :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `p_`

``` purescript
p_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `param`

``` purescript
param :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `param_`

``` purescript
param_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `pre`

``` purescript
pre :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `pre_`

``` purescript
pre_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `progress`

``` purescript
progress :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `progress_`

``` purescript
progress_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `q`

``` purescript
q :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `q_`

``` purescript
q_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `rp`

``` purescript
rp :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `rp_`

``` purescript
rp_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `rt`

``` purescript
rt :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `rt_`

``` purescript
rt_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `ruby`

``` purescript
ruby :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `ruby_`

``` purescript
ruby_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `s`

``` purescript
s :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `s_`

``` purescript
s_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `samp`

``` purescript
samp :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `samp_`

``` purescript
samp_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `script`

``` purescript
script :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `script_`

``` purescript
script_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `section`

``` purescript
section :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `section_`

``` purescript
section_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `select`

``` purescript
select :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `select_`

``` purescript
select_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `small`

``` purescript
small :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `small_`

``` purescript
small_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `source`

``` purescript
source :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `source_`

``` purescript
source_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `span`

``` purescript
span :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `span_`

``` purescript
span_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `strike`

``` purescript
strike :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `strike_`

``` purescript
strike_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `strong`

``` purescript
strong :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `strong_`

``` purescript
strong_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `style`

``` purescript
style :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `style_`

``` purescript
style_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `sub`

``` purescript
sub :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `sub_`

``` purescript
sub_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `summary`

``` purescript
summary :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `summary_`

``` purescript
summary_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `sup`

``` purescript
sup :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `sup_`

``` purescript
sup_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `table`

``` purescript
table :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `table_`

``` purescript
table_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `tbody`

``` purescript
tbody :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `tbody_`

``` purescript
tbody_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `td`

``` purescript
td :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `td_`

``` purescript
td_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `textarea`

``` purescript
textarea :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `textarea_`

``` purescript
textarea_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `tfoot`

``` purescript
tfoot :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `tfoot_`

``` purescript
tfoot_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `th`

``` purescript
th :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `th_`

``` purescript
th_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `thead`

``` purescript
thead :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `thead_`

``` purescript
thead_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `time`

``` purescript
time :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `time_`

``` purescript
time_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `title`

``` purescript
title :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `title_`

``` purescript
title_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `tr`

``` purescript
tr :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `tr_`

``` purescript
tr_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `track`

``` purescript
track :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `track_`

``` purescript
track_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `tt`

``` purescript
tt :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `tt_`

``` purescript
tt_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `u`

``` purescript
u :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `u_`

``` purescript
u_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `ul`

``` purescript
ul :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `ul_`

``` purescript
ul_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `var`

``` purescript
var :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `var_`

``` purescript
var_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `video`

``` purescript
video :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `video_`

``` purescript
video_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
```


#### `wbr`

``` purescript
wbr :: forall p i node. (HTMLRepr node) => [A.Attr i] -> [node p i] -> node p i
```


#### `wbr_`

``` purescript
wbr_ :: forall p i node. (HTMLRepr node) => [node p i] -> node p i
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


#### `AttrRepr`

``` purescript
class (Functor attr) <= AttrRepr attr where
  attr :: forall value i. (IsAttribute value) => AttributeName value -> value -> attr i
  handler :: forall fields i. EventName fields -> (Event fields -> EventHandler (Maybe i)) -> attr i
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


#### `attrRepr`

``` purescript
instance attrRepr :: AttrRepr Attr
```


#### `functorAttr`

``` purescript
instance functorAttr :: Functor Attr
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


#### `enabled`

``` purescript
enabled :: forall i. Boolean -> Attr i
```


#### `checked`

``` purescript
checked :: forall i. Boolean -> Attr i
```


#### `placeholder`

``` purescript
placeholder :: forall i. String -> Attr i
```


#### `style`

``` purescript
style :: forall i. Styles -> Attr i
```




