## Module Halogen.HTML

This module defines the HTML types required by the Halogen library, and provides
smart constructors for HTML5 elements.

#### `HTML`

``` purescript
data HTML p i
  = Text String
  | Element TagName (Array (Prop i)) (Array (HTML p i))
  | Placeholder p
```

An initial encoding of HTML nodes.

##### Instances
``` purescript
instance bifunctorHTML :: Bifunctor HTML
instance functorHTML :: Functor (HTML p)
```

#### `text`

``` purescript
text :: forall p i. String -> HTML p i
```

#### `element`

``` purescript
element :: forall p i. TagName -> Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `install`

``` purescript
install :: forall p p' i i' m. (Applicative m) => (p -> m (HTML p' i')) -> (i -> i') -> HTML p i -> m (HTML p' i')
```

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

#### `a`

``` purescript
a :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `a_`

``` purescript
a_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `abbr`

``` purescript
abbr :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `abbr_`

``` purescript
abbr_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `acronym`

``` purescript
acronym :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `acronym_`

``` purescript
acronym_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `address`

``` purescript
address :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `address_`

``` purescript
address_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `applet`

``` purescript
applet :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `applet_`

``` purescript
applet_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `area`

``` purescript
area :: forall p i. Array (Prop i) -> HTML p i
```

#### `article`

``` purescript
article :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `article_`

``` purescript
article_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `aside`

``` purescript
aside :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `aside_`

``` purescript
aside_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `audio`

``` purescript
audio :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `audio_`

``` purescript
audio_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `b`

``` purescript
b :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `b_`

``` purescript
b_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `base`

``` purescript
base :: forall p i. Array (Prop i) -> HTML p i
```

#### `basefont`

``` purescript
basefont :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `basefont_`

``` purescript
basefont_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `bdi`

``` purescript
bdi :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `bdi_`

``` purescript
bdi_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `bdo`

``` purescript
bdo :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `bdo_`

``` purescript
bdo_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `big`

``` purescript
big :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `big_`

``` purescript
big_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `blockquote`

``` purescript
blockquote :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `blockquote_`

``` purescript
blockquote_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `body`

``` purescript
body :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `body_`

``` purescript
body_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `br`

``` purescript
br :: forall p i. Array (Prop i) -> HTML p i
```

#### `button`

``` purescript
button :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `button_`

``` purescript
button_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `canvas`

``` purescript
canvas :: forall p i. Array (Prop i) -> HTML p i
```

#### `caption`

``` purescript
caption :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `caption_`

``` purescript
caption_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `center`

``` purescript
center :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `center_`

``` purescript
center_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `cite`

``` purescript
cite :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `cite_`

``` purescript
cite_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `code`

``` purescript
code :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `code_`

``` purescript
code_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `col`

``` purescript
col :: forall p i. Array (Prop i) -> HTML p i
```

#### `colgroup`

``` purescript
colgroup :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `colgroup_`

``` purescript
colgroup_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `command`

``` purescript
command :: forall p i. Array (Prop i) -> HTML p i
```

#### `datalist`

``` purescript
datalist :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `datalist_`

``` purescript
datalist_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `dd`

``` purescript
dd :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `dd_`

``` purescript
dd_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `del`

``` purescript
del :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `del_`

``` purescript
del_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `details`

``` purescript
details :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `details_`

``` purescript
details_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `dfn`

``` purescript
dfn :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `dfn_`

``` purescript
dfn_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `dialog`

``` purescript
dialog :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `dialog_`

``` purescript
dialog_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `dir`

``` purescript
dir :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `dir_`

``` purescript
dir_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `div`

``` purescript
div :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `div_`

``` purescript
div_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `dl`

``` purescript
dl :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `dl_`

``` purescript
dl_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `dt`

``` purescript
dt :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `dt_`

``` purescript
dt_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `em`

``` purescript
em :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `em_`

``` purescript
em_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `embed`

``` purescript
embed :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `embed_`

``` purescript
embed_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `fieldset`

``` purescript
fieldset :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `fieldset_`

``` purescript
fieldset_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `figcaption`

``` purescript
figcaption :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `figcaption_`

``` purescript
figcaption_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `figure`

``` purescript
figure :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `figure_`

``` purescript
figure_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `font`

``` purescript
font :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `font_`

``` purescript
font_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `footer`

``` purescript
footer :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `footer_`

``` purescript
footer_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `form`

``` purescript
form :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `form_`

``` purescript
form_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `frame`

``` purescript
frame :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `frame_`

``` purescript
frame_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `frameset`

``` purescript
frameset :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `frameset_`

``` purescript
frameset_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `h1`

``` purescript
h1 :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `h1_`

``` purescript
h1_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `h2`

``` purescript
h2 :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `h2_`

``` purescript
h2_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `h3`

``` purescript
h3 :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `h3_`

``` purescript
h3_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `h4`

``` purescript
h4 :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `h4_`

``` purescript
h4_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `h5`

``` purescript
h5 :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `h5_`

``` purescript
h5_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `h6`

``` purescript
h6 :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `h6_`

``` purescript
h6_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `head`

``` purescript
head :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `head_`

``` purescript
head_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `header`

``` purescript
header :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `header_`

``` purescript
header_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `hr`

``` purescript
hr :: forall p i. Array (Prop i) -> HTML p i
```

#### `html`

``` purescript
html :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `html_`

``` purescript
html_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `i`

``` purescript
i :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `i_`

``` purescript
i_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `iframe`

``` purescript
iframe :: forall p i. Array (Prop i) -> HTML p i
```

#### `img`

``` purescript
img :: forall p i. Array (Prop i) -> HTML p i
```

#### `input`

``` purescript
input :: forall p i. Array (Prop i) -> HTML p i
```

#### `ins`

``` purescript
ins :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `ins_`

``` purescript
ins_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `kbd`

``` purescript
kbd :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `kbd_`

``` purescript
kbd_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `keygen`

``` purescript
keygen :: forall p i. Array (Prop i) -> HTML p i
```

#### `label`

``` purescript
label :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `label_`

``` purescript
label_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `legend`

``` purescript
legend :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `legend_`

``` purescript
legend_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `li`

``` purescript
li :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `li_`

``` purescript
li_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `link`

``` purescript
link :: forall p i. Array (Prop i) -> HTML p i
```

#### `main`

``` purescript
main :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `main_`

``` purescript
main_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `map`

``` purescript
map :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `map_`

``` purescript
map_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `mark`

``` purescript
mark :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `mark_`

``` purescript
mark_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `menu`

``` purescript
menu :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `menu_`

``` purescript
menu_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `menuitem`

``` purescript
menuitem :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `menuitem_`

``` purescript
menuitem_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `meta`

``` purescript
meta :: forall p i. Array (Prop i) -> HTML p i
```

#### `meter`

``` purescript
meter :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `meter_`

``` purescript
meter_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `nav`

``` purescript
nav :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `nav_`

``` purescript
nav_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `noframes`

``` purescript
noframes :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `noframes_`

``` purescript
noframes_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `noscript`

``` purescript
noscript :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `noscript_`

``` purescript
noscript_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `object`

``` purescript
object :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `object_`

``` purescript
object_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `ol`

``` purescript
ol :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `ol_`

``` purescript
ol_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `optgroup`

``` purescript
optgroup :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `optgroup_`

``` purescript
optgroup_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `option`

``` purescript
option :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `option_`

``` purescript
option_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `output`

``` purescript
output :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `output_`

``` purescript
output_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `p`

``` purescript
p :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `p_`

``` purescript
p_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `param`

``` purescript
param :: forall p i. Array (Prop i) -> HTML p i
```

#### `pre`

``` purescript
pre :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `pre_`

``` purescript
pre_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `progress`

``` purescript
progress :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `progress_`

``` purescript
progress_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `q`

``` purescript
q :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `q_`

``` purescript
q_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `rp`

``` purescript
rp :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `rp_`

``` purescript
rp_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `rt`

``` purescript
rt :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `rt_`

``` purescript
rt_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `ruby`

``` purescript
ruby :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `ruby_`

``` purescript
ruby_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `s`

``` purescript
s :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `s_`

``` purescript
s_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `samp`

``` purescript
samp :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `samp_`

``` purescript
samp_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `script`

``` purescript
script :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `script_`

``` purescript
script_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `section`

``` purescript
section :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `section_`

``` purescript
section_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `select`

``` purescript
select :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `select_`

``` purescript
select_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `small`

``` purescript
small :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `small_`

``` purescript
small_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `source`

``` purescript
source :: forall p i. Array (Prop i) -> HTML p i
```

#### `span`

``` purescript
span :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `span_`

``` purescript
span_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `strike`

``` purescript
strike :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `strike_`

``` purescript
strike_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `strong`

``` purescript
strong :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `strong_`

``` purescript
strong_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `style`

``` purescript
style :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `style_`

``` purescript
style_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `sub`

``` purescript
sub :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `sub_`

``` purescript
sub_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `summary`

``` purescript
summary :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `summary_`

``` purescript
summary_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `sup`

``` purescript
sup :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `sup_`

``` purescript
sup_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `table`

``` purescript
table :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `table_`

``` purescript
table_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `tbody`

``` purescript
tbody :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `tbody_`

``` purescript
tbody_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `td`

``` purescript
td :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `td_`

``` purescript
td_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `textarea`

``` purescript
textarea :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `textarea_`

``` purescript
textarea_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `tfoot`

``` purescript
tfoot :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `tfoot_`

``` purescript
tfoot_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `th`

``` purescript
th :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `th_`

``` purescript
th_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `thead`

``` purescript
thead :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `thead_`

``` purescript
thead_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `time`

``` purescript
time :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `time_`

``` purescript
time_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `title`

``` purescript
title :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `title_`

``` purescript
title_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `tr`

``` purescript
tr :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `tr_`

``` purescript
tr_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `track`

``` purescript
track :: forall p i. Array (Prop i) -> HTML p i
```

#### `tt`

``` purescript
tt :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `tt_`

``` purescript
tt_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `u`

``` purescript
u :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `u_`

``` purescript
u_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `ul`

``` purescript
ul :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `ul_`

``` purescript
ul_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `var`

``` purescript
var :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `var_`

``` purescript
var_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `video`

``` purescript
video :: forall p i. Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `video_`

``` purescript
video_ :: forall p i. Array (HTML p i) -> HTML p i
```

#### `wbr`

``` purescript
wbr :: forall p i. Array (Prop i) -> HTML p i
```


