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
  | Element TagName (Array (Attr i)) (Array (HTML i))
```

An initial encoding of HTML nodes.

##### Instances
``` purescript
instance functorHTML :: Functor HTML
```

#### `text`

``` purescript
text :: forall i. String -> HTML i
```

#### `element`

``` purescript
element :: forall i. TagName -> Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `a`

``` purescript
a :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `a_`

``` purescript
a_ :: forall i. Array (HTML i) -> HTML i
```

#### `abbr`

``` purescript
abbr :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `abbr_`

``` purescript
abbr_ :: forall i. Array (HTML i) -> HTML i
```

#### `acronym`

``` purescript
acronym :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `acronym_`

``` purescript
acronym_ :: forall i. Array (HTML i) -> HTML i
```

#### `address`

``` purescript
address :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `address_`

``` purescript
address_ :: forall i. Array (HTML i) -> HTML i
```

#### `applet`

``` purescript
applet :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `applet_`

``` purescript
applet_ :: forall i. Array (HTML i) -> HTML i
```

#### `area`

``` purescript
area :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `area_`

``` purescript
area_ :: forall i. Array (HTML i) -> HTML i
```

#### `article`

``` purescript
article :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `article_`

``` purescript
article_ :: forall i. Array (HTML i) -> HTML i
```

#### `aside`

``` purescript
aside :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `aside_`

``` purescript
aside_ :: forall i. Array (HTML i) -> HTML i
```

#### `audio`

``` purescript
audio :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `audio_`

``` purescript
audio_ :: forall i. Array (HTML i) -> HTML i
```

#### `b`

``` purescript
b :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `b_`

``` purescript
b_ :: forall i. Array (HTML i) -> HTML i
```

#### `base`

``` purescript
base :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `base_`

``` purescript
base_ :: forall i. Array (HTML i) -> HTML i
```

#### `basefont`

``` purescript
basefont :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `basefont_`

``` purescript
basefont_ :: forall i. Array (HTML i) -> HTML i
```

#### `bdi`

``` purescript
bdi :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `bdi_`

``` purescript
bdi_ :: forall i. Array (HTML i) -> HTML i
```

#### `bdo`

``` purescript
bdo :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `bdo_`

``` purescript
bdo_ :: forall i. Array (HTML i) -> HTML i
```

#### `big`

``` purescript
big :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `big_`

``` purescript
big_ :: forall i. Array (HTML i) -> HTML i
```

#### `blockquote`

``` purescript
blockquote :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `blockquote_`

``` purescript
blockquote_ :: forall i. Array (HTML i) -> HTML i
```

#### `body`

``` purescript
body :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `body_`

``` purescript
body_ :: forall i. Array (HTML i) -> HTML i
```

#### `br`

``` purescript
br :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `br_`

``` purescript
br_ :: forall i. Array (HTML i) -> HTML i
```

#### `button`

``` purescript
button :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `button_`

``` purescript
button_ :: forall i. Array (HTML i) -> HTML i
```

#### `canvas`

``` purescript
canvas :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `canvas_`

``` purescript
canvas_ :: forall i. Array (HTML i) -> HTML i
```

#### `caption`

``` purescript
caption :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `caption_`

``` purescript
caption_ :: forall i. Array (HTML i) -> HTML i
```

#### `center`

``` purescript
center :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `center_`

``` purescript
center_ :: forall i. Array (HTML i) -> HTML i
```

#### `cite`

``` purescript
cite :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `cite_`

``` purescript
cite_ :: forall i. Array (HTML i) -> HTML i
```

#### `code`

``` purescript
code :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `code_`

``` purescript
code_ :: forall i. Array (HTML i) -> HTML i
```

#### `col`

``` purescript
col :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `col_`

``` purescript
col_ :: forall i. Array (HTML i) -> HTML i
```

#### `colgroup`

``` purescript
colgroup :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `colgroup_`

``` purescript
colgroup_ :: forall i. Array (HTML i) -> HTML i
```

#### `datalist`

``` purescript
datalist :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `datalist_`

``` purescript
datalist_ :: forall i. Array (HTML i) -> HTML i
```

#### `dd`

``` purescript
dd :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `dd_`

``` purescript
dd_ :: forall i. Array (HTML i) -> HTML i
```

#### `del`

``` purescript
del :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `del_`

``` purescript
del_ :: forall i. Array (HTML i) -> HTML i
```

#### `details`

``` purescript
details :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `details_`

``` purescript
details_ :: forall i. Array (HTML i) -> HTML i
```

#### `dfn`

``` purescript
dfn :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `dfn_`

``` purescript
dfn_ :: forall i. Array (HTML i) -> HTML i
```

#### `dialog`

``` purescript
dialog :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `dialog_`

``` purescript
dialog_ :: forall i. Array (HTML i) -> HTML i
```

#### `dir`

``` purescript
dir :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `dir_`

``` purescript
dir_ :: forall i. Array (HTML i) -> HTML i
```

#### `div`

``` purescript
div :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `div_`

``` purescript
div_ :: forall i. Array (HTML i) -> HTML i
```

#### `dl`

``` purescript
dl :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `dl_`

``` purescript
dl_ :: forall i. Array (HTML i) -> HTML i
```

#### `dt`

``` purescript
dt :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `dt_`

``` purescript
dt_ :: forall i. Array (HTML i) -> HTML i
```

#### `em`

``` purescript
em :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `em_`

``` purescript
em_ :: forall i. Array (HTML i) -> HTML i
```

#### `embed`

``` purescript
embed :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `embed_`

``` purescript
embed_ :: forall i. Array (HTML i) -> HTML i
```

#### `fieldset`

``` purescript
fieldset :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `fieldset_`

``` purescript
fieldset_ :: forall i. Array (HTML i) -> HTML i
```

#### `figcaption`

``` purescript
figcaption :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `figcaption_`

``` purescript
figcaption_ :: forall i. Array (HTML i) -> HTML i
```

#### `figure`

``` purescript
figure :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `figure_`

``` purescript
figure_ :: forall i. Array (HTML i) -> HTML i
```

#### `font`

``` purescript
font :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `font_`

``` purescript
font_ :: forall i. Array (HTML i) -> HTML i
```

#### `footer`

``` purescript
footer :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `footer_`

``` purescript
footer_ :: forall i. Array (HTML i) -> HTML i
```

#### `form`

``` purescript
form :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `form_`

``` purescript
form_ :: forall i. Array (HTML i) -> HTML i
```

#### `frame`

``` purescript
frame :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `frame_`

``` purescript
frame_ :: forall i. Array (HTML i) -> HTML i
```

#### `frameset`

``` purescript
frameset :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `frameset_`

``` purescript
frameset_ :: forall i. Array (HTML i) -> HTML i
```

#### `h1`

``` purescript
h1 :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `h1_`

``` purescript
h1_ :: forall i. Array (HTML i) -> HTML i
```

#### `h2`

``` purescript
h2 :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `h2_`

``` purescript
h2_ :: forall i. Array (HTML i) -> HTML i
```

#### `h3`

``` purescript
h3 :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `h3_`

``` purescript
h3_ :: forall i. Array (HTML i) -> HTML i
```

#### `h4`

``` purescript
h4 :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `h4_`

``` purescript
h4_ :: forall i. Array (HTML i) -> HTML i
```

#### `h5`

``` purescript
h5 :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `h5_`

``` purescript
h5_ :: forall i. Array (HTML i) -> HTML i
```

#### `h6`

``` purescript
h6 :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `h6_`

``` purescript
h6_ :: forall i. Array (HTML i) -> HTML i
```

#### `head`

``` purescript
head :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `head_`

``` purescript
head_ :: forall i. Array (HTML i) -> HTML i
```

#### `header`

``` purescript
header :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `header_`

``` purescript
header_ :: forall i. Array (HTML i) -> HTML i
```

#### `hr`

``` purescript
hr :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `hr_`

``` purescript
hr_ :: forall i. Array (HTML i) -> HTML i
```

#### `html`

``` purescript
html :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `html_`

``` purescript
html_ :: forall i. Array (HTML i) -> HTML i
```

#### `i`

``` purescript
i :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `i_`

``` purescript
i_ :: forall i. Array (HTML i) -> HTML i
```

#### `iframe`

``` purescript
iframe :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `iframe_`

``` purescript
iframe_ :: forall i. Array (HTML i) -> HTML i
```

#### `img`

``` purescript
img :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `img_`

``` purescript
img_ :: forall i. Array (HTML i) -> HTML i
```

#### `input`

``` purescript
input :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `input_`

``` purescript
input_ :: forall i. Array (HTML i) -> HTML i
```

#### `ins`

``` purescript
ins :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `ins_`

``` purescript
ins_ :: forall i. Array (HTML i) -> HTML i
```

#### `kbd`

``` purescript
kbd :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `kbd_`

``` purescript
kbd_ :: forall i. Array (HTML i) -> HTML i
```

#### `keygen`

``` purescript
keygen :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `keygen_`

``` purescript
keygen_ :: forall i. Array (HTML i) -> HTML i
```

#### `label`

``` purescript
label :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `label_`

``` purescript
label_ :: forall i. Array (HTML i) -> HTML i
```

#### `legend`

``` purescript
legend :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `legend_`

``` purescript
legend_ :: forall i. Array (HTML i) -> HTML i
```

#### `li`

``` purescript
li :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `li_`

``` purescript
li_ :: forall i. Array (HTML i) -> HTML i
```

#### `link`

``` purescript
link :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `link_`

``` purescript
link_ :: forall i. Array (HTML i) -> HTML i
```

#### `main`

``` purescript
main :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `main_`

``` purescript
main_ :: forall i. Array (HTML i) -> HTML i
```

#### `map`

``` purescript
map :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `map_`

``` purescript
map_ :: forall i. Array (HTML i) -> HTML i
```

#### `mark`

``` purescript
mark :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `mark_`

``` purescript
mark_ :: forall i. Array (HTML i) -> HTML i
```

#### `menu`

``` purescript
menu :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `menu_`

``` purescript
menu_ :: forall i. Array (HTML i) -> HTML i
```

#### `menuitem`

``` purescript
menuitem :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `menuitem_`

``` purescript
menuitem_ :: forall i. Array (HTML i) -> HTML i
```

#### `meta`

``` purescript
meta :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `meta_`

``` purescript
meta_ :: forall i. Array (HTML i) -> HTML i
```

#### `meter`

``` purescript
meter :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `meter_`

``` purescript
meter_ :: forall i. Array (HTML i) -> HTML i
```

#### `nav`

``` purescript
nav :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `nav_`

``` purescript
nav_ :: forall i. Array (HTML i) -> HTML i
```

#### `noframes`

``` purescript
noframes :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `noframes_`

``` purescript
noframes_ :: forall i. Array (HTML i) -> HTML i
```

#### `noscript`

``` purescript
noscript :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `noscript_`

``` purescript
noscript_ :: forall i. Array (HTML i) -> HTML i
```

#### `object`

``` purescript
object :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `object_`

``` purescript
object_ :: forall i. Array (HTML i) -> HTML i
```

#### `ol`

``` purescript
ol :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `ol_`

``` purescript
ol_ :: forall i. Array (HTML i) -> HTML i
```

#### `optgroup`

``` purescript
optgroup :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `optgroup_`

``` purescript
optgroup_ :: forall i. Array (HTML i) -> HTML i
```

#### `option`

``` purescript
option :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `option_`

``` purescript
option_ :: forall i. Array (HTML i) -> HTML i
```

#### `output`

``` purescript
output :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `output_`

``` purescript
output_ :: forall i. Array (HTML i) -> HTML i
```

#### `p`

``` purescript
p :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `p_`

``` purescript
p_ :: forall i. Array (HTML i) -> HTML i
```

#### `param`

``` purescript
param :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `param_`

``` purescript
param_ :: forall i. Array (HTML i) -> HTML i
```

#### `pre`

``` purescript
pre :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `pre_`

``` purescript
pre_ :: forall i. Array (HTML i) -> HTML i
```

#### `progress`

``` purescript
progress :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `progress_`

``` purescript
progress_ :: forall i. Array (HTML i) -> HTML i
```

#### `q`

``` purescript
q :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `q_`

``` purescript
q_ :: forall i. Array (HTML i) -> HTML i
```

#### `rp`

``` purescript
rp :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `rp_`

``` purescript
rp_ :: forall i. Array (HTML i) -> HTML i
```

#### `rt`

``` purescript
rt :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `rt_`

``` purescript
rt_ :: forall i. Array (HTML i) -> HTML i
```

#### `ruby`

``` purescript
ruby :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `ruby_`

``` purescript
ruby_ :: forall i. Array (HTML i) -> HTML i
```

#### `s`

``` purescript
s :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `s_`

``` purescript
s_ :: forall i. Array (HTML i) -> HTML i
```

#### `samp`

``` purescript
samp :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `samp_`

``` purescript
samp_ :: forall i. Array (HTML i) -> HTML i
```

#### `script`

``` purescript
script :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `script_`

``` purescript
script_ :: forall i. Array (HTML i) -> HTML i
```

#### `section`

``` purescript
section :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `section_`

``` purescript
section_ :: forall i. Array (HTML i) -> HTML i
```

#### `select`

``` purescript
select :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `select_`

``` purescript
select_ :: forall i. Array (HTML i) -> HTML i
```

#### `small`

``` purescript
small :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `small_`

``` purescript
small_ :: forall i. Array (HTML i) -> HTML i
```

#### `source`

``` purescript
source :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `source_`

``` purescript
source_ :: forall i. Array (HTML i) -> HTML i
```

#### `span`

``` purescript
span :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `span_`

``` purescript
span_ :: forall i. Array (HTML i) -> HTML i
```

#### `strike`

``` purescript
strike :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `strike_`

``` purescript
strike_ :: forall i. Array (HTML i) -> HTML i
```

#### `strong`

``` purescript
strong :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `strong_`

``` purescript
strong_ :: forall i. Array (HTML i) -> HTML i
```

#### `style`

``` purescript
style :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `style_`

``` purescript
style_ :: forall i. Array (HTML i) -> HTML i
```

#### `sub`

``` purescript
sub :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `sub_`

``` purescript
sub_ :: forall i. Array (HTML i) -> HTML i
```

#### `summary`

``` purescript
summary :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `summary_`

``` purescript
summary_ :: forall i. Array (HTML i) -> HTML i
```

#### `sup`

``` purescript
sup :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `sup_`

``` purescript
sup_ :: forall i. Array (HTML i) -> HTML i
```

#### `table`

``` purescript
table :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `table_`

``` purescript
table_ :: forall i. Array (HTML i) -> HTML i
```

#### `tbody`

``` purescript
tbody :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `tbody_`

``` purescript
tbody_ :: forall i. Array (HTML i) -> HTML i
```

#### `td`

``` purescript
td :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `td_`

``` purescript
td_ :: forall i. Array (HTML i) -> HTML i
```

#### `textarea`

``` purescript
textarea :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `textarea_`

``` purescript
textarea_ :: forall i. Array (HTML i) -> HTML i
```

#### `tfoot`

``` purescript
tfoot :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `tfoot_`

``` purescript
tfoot_ :: forall i. Array (HTML i) -> HTML i
```

#### `th`

``` purescript
th :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `th_`

``` purescript
th_ :: forall i. Array (HTML i) -> HTML i
```

#### `thead`

``` purescript
thead :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `thead_`

``` purescript
thead_ :: forall i. Array (HTML i) -> HTML i
```

#### `time`

``` purescript
time :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `time_`

``` purescript
time_ :: forall i. Array (HTML i) -> HTML i
```

#### `title`

``` purescript
title :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `title_`

``` purescript
title_ :: forall i. Array (HTML i) -> HTML i
```

#### `tr`

``` purescript
tr :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `tr_`

``` purescript
tr_ :: forall i. Array (HTML i) -> HTML i
```

#### `track`

``` purescript
track :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `track_`

``` purescript
track_ :: forall i. Array (HTML i) -> HTML i
```

#### `tt`

``` purescript
tt :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `tt_`

``` purescript
tt_ :: forall i. Array (HTML i) -> HTML i
```

#### `u`

``` purescript
u :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `u_`

``` purescript
u_ :: forall i. Array (HTML i) -> HTML i
```

#### `ul`

``` purescript
ul :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `ul_`

``` purescript
ul_ :: forall i. Array (HTML i) -> HTML i
```

#### `var`

``` purescript
var :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `var_`

``` purescript
var_ :: forall i. Array (HTML i) -> HTML i
```

#### `video`

``` purescript
video :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `video_`

``` purescript
video_ :: forall i. Array (HTML i) -> HTML i
```

#### `wbr`

``` purescript
wbr :: forall i. Array (Attr i) -> Array (HTML i) -> HTML i
```

#### `wbr_`

``` purescript
wbr_ :: forall i. Array (HTML i) -> HTML i
```


