module Halogen.HTML.Elements.Indexed
  ( Node()
  , Leaf()
  , NoninteractiveNode()
  , NoninteractiveLeaf()
  , a
  , abbr
  , acronym
  , address
  , applet
  , area
  , article
  , aside
  , audio
  , b
  , base
  , basefont
  , bdi
  , bdo
  , big
  , blockquote
  , body
  , br
  , button
  , canvas
  , caption
  , center
  , cite
  , code
  , col
  , colgroup
  , command
  , datalist
  , dd
  , del
  , details
  , dfn
  , dialog
  , dir
  , div
  , dl
  , dt
  , em
  , embed
  , fieldset
  , figcaption
  , figure
  , footer
  , form
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , head
  , header
  , hr
  , html
  , i
  , iframe
  , img
  , input
  , ins
  , kbd
  , keygen
  , label
  , legend
  , li
  , link
  , main
  , map
  , mark
  , menu
  , menuitem
  , meta
  , meter
  , nav
  , noframes
  , noscript
  , object
  , ol
  , optgroup
  , option
  , output
  , p
  , param
  , pre
  , progress
  , q
  , rp
  , rt
  , ruby
  , s
  , samp
  , script
  , section
  , select
  , small
  , source
  , span
  , strong
  , style
  , sub
  , summary
  , sup
  , table
  , tbody
  , td
  , textarea
  , tfoot
  , th
  , thead
  , time
  , title
  , tr
  , track
  , tt
  , u
  , ul
  , var
  , video
  , wbr
  , module Unrefined
  ) where

import Halogen.HTML.Core (HTML())
import Halogen.HTML.Properties.Indexed hiding (title)
import qualified Halogen.HTML.Elements as E
import qualified Halogen.HTML.Elements
  ( a_
  , abbr_
  , acronym_
  , address_
  , applet_
  , article_
  , aside_
  , audio_
  , b_
  , basefont_
  , bdi_
  , bdo_
  , big_
  , blockquote_
  , body_
  , br_
  , button_
  , caption_
  , center_
  , cite_
  , code_
  , colgroup_
  , datalist_
  , dd_
  , del_
  , details_
  , dfn_
  , dialog_
  , dir_
  , div_
  , dl_
  , dt_
  , em_
  , embed_
  , fieldset_
  , figcaption_
  , figure_
  , font_
  , footer_
  , form_
  , frame_
  , frameset_
  , h1_
  , h2_
  , h3_
  , h4_
  , h5_
  , h6_
  , head_
  , header_
  , hr_
  , html_
  , i_
  , ins_
  , kbd_
  , label_
  , legend_
  , li_
  , main_
  , map_
  , mark_
  , menu_
  , menuitem_
  , meter_
  , nav_
  , noframes_
  , noscript_
  , object_
  , ol_
  , optgroup_
  , option_
  , output_
  , p_
  , pre_
  , progress_
  , q_
  , rp_
  , rt_
  , ruby_
  , s_
  , samp_
  , script_
  , section_
  , select_
  , small_
  , span_
  , strike_
  , strong_
  , style_
  , sub_
  , summary_
  , sup_
  , table_
  , tbody_
  , td_
  , tfoot_
  , th_
  , thead_
  , time_
  , title_
  , tr_
  , tt_
  , u_
  , ul_
  , var_
  , video_
  ) as Unrefined

import Unsafe.Coerce

-- | An HTML element that admits children.
type Node r p i
   = Array (IProp (InteractiveEvents (GlobalProperties r)) i)
  -> Array (HTML p i)
  -> HTML p i

-- | A `Node` that doesn't support mouse events.
type NoninteractiveNode r p i
   = Array (IProp (GlobalProperties r) i)
  -> Array (HTML p i)
  -> HTML p i

-- | An HTML element that does not admit children.
type Leaf r p i
   = Array (IProp (InteractiveEvents (GlobalProperties r)) i)
  -> HTML p i

-- | An `Leaf` that doesn't support mouse events.
type NoninteractiveLeaf r p i
   = Array (IProp (GlobalProperties r) i)
  -> HTML p i

a :: forall p i. Node (download :: I, href :: I, hreflang :: I, mediate :: I, rel :: I, target :: I, mediaType :: I) p i
a = unsafeCoerce E.a

abbr :: forall p i. Node () p i
abbr = unsafeCoerce E.a

acronym :: forall p i. Node () p i
acronym = unsafeCoerce E.acronym

address :: forall p i. Node (onScroll :: I) p i
address = unsafeCoerce E.address

applet :: forall p i. Node (code :: I, object :: I) p i
applet = unsafeCoerce E.applet

area :: forall p i. Leaf (coords :: I, download :: I, href :: I, hreflang :: I, media :: I, rel :: I, shape :: I, target :: I, mediaType :: I) p i
area = unsafeCoerce E.area

article :: forall p i. Node () p i
article = unsafeCoerce E.article

aside :: forall p i. Node () p i
aside = unsafeCoerce E.aside

audio :: forall p i. Node (autoplay :: I, controls :: I, loop :: I, muted :: I, preload :: I, src :: I) p i
audio = unsafeCoerce E.audio

b :: forall p i. Node () p i
b = unsafeCoerce E.b

base :: forall p i. NoninteractiveLeaf (href :: I, target :: I) p i
base = unsafeCoerce E.base

basefont :: forall p i. Node (color :: I, face :: I, size :: I) p i
basefont = unsafeCoerce E.basefont

bdi :: forall p i. Node () p i
bdi = unsafeCoerce E.bdi

bdo :: forall p i. NoninteractiveNode (dir :: I) p i
bdo = unsafeCoerce E.bdo

big :: forall p i. Node () p i
big = unsafeCoerce E.big

blockquote :: forall p i. Node (cite :: I, onScroll :: I) p i
blockquote = unsafeCoerce E.blockquote

body :: forall p i. Node (onBeforeUnload :: I, onHashChange :: I, onLoad :: I, onPageShow :: I, onPageHide :: I, onResize :: I, onScroll :: I, onUnload :: I) p i
body = unsafeCoerce E.body

br :: forall p i. NoninteractiveLeaf () p i
br = unsafeCoerce E.br

button :: forall p i. Node (autofocus :: I, disabled :: I, form :: I, formaction :: I, formenctyp :: I, formmethod :: I, formnovalidate :: I, formtaget :: I, buttonType :: I, value :: I) p i
button = unsafeCoerce E.button

canvas :: forall p i. Leaf (width :: I, height :: I) p i
canvas = unsafeCoerce E.canvas

caption :: forall p i. Node (align :: I, onScroll :: I) p i
caption = unsafeCoerce E.caption

center :: forall p i. Node (onScroll :: I) p i
center = unsafeCoerce E.center

cite :: forall p i. Node () p i
cite = unsafeCoerce E.cite

code :: forall p i. Node () p i
code = unsafeCoerce E.code

col :: forall p i. Leaf () p i
col = unsafeCoerce E.col

colgroup :: forall p i. Node (span :: I) p i
colgroup = unsafeCoerce E.colgroup

command :: forall p i. Leaf () p i
command = unsafeCoerce E.command

datalist :: forall p i. Node () p i
datalist = unsafeCoerce E.datalist

dd :: forall p i. Node (onScroll :: I) p i
dd = unsafeCoerce E.dd

del :: forall p i. Node (cite :: I, datetime :: I) p i
del = unsafeCoerce E.del

details :: forall p i. Node (open :: I) p i
details = unsafeCoerce E.details

dfn :: forall p i. Node () p i
dfn = unsafeCoerce E.dfn

dialog :: forall p i. Node (open :: I) p i
dialog = unsafeCoerce E.dialog

dir :: forall p i. Node (onScroll :: I) p i
dir = unsafeCoerce E.dir

div :: forall p i. Node (onScroll :: I) p i
div = unsafeCoerce E.div

dl :: forall p i. Node (onScroll :: I) p i
dl = unsafeCoerce E.dl

dt :: forall p i. Node (onScroll :: I) p i
dt = unsafeCoerce E.dt

em :: forall p i. Node () p i
em = unsafeCoerce E.em

embed :: forall p i. Node (height :: I, src :: I, mediaType :: I, width :: I) p i
embed = unsafeCoerce E.embed

fieldset :: forall p i. Node (disabled :: I, form :: I, onScroll :: I) p i
fieldset = unsafeCoerce E.fieldset

figcaption :: forall p i. Node () p i
figcaption = unsafeCoerce E.figcaption

figure :: forall p i. Node () p i
figure = unsafeCoerce E.figure

footer :: forall p i. Node () p i
footer = unsafeCoerce E.footer

form :: forall p i. Node (acceptCharset :: I, action :: I, autocomplete :: I, enctype :: I, method :: I, onReset :: I, novalidate :: I, onScroll :: I, onSubmit :: I, target :: I) p i
form = unsafeCoerce E.form

h1 :: forall p i. Node (onScroll :: I) p i
h1 = unsafeCoerce E.h1

h2 :: forall p i. Node (onScroll :: I) p i
h2 = unsafeCoerce E.h2

h3 :: forall p i. Node (onScroll :: I) p i
h3 = unsafeCoerce E.h3

h4 :: forall p i. Node (onScroll :: I) p i
h4 = unsafeCoerce E.h4

h5 :: forall p i. Node (onScroll :: I) p i
h5 = unsafeCoerce E.h5

h6 :: forall p i. Node (onScroll :: I) p i
h6 = unsafeCoerce E.h6

head :: forall p i. NoninteractiveNode () p i
head = unsafeCoerce E.head

header :: forall p i. Node () p i
header = unsafeCoerce E.header

hr :: forall p i. Leaf () p i
hr = unsafeCoerce E.hr

html :: forall p i. NoninteractiveNode (manifest :: I, xmlns :: I, onScroll :: I) p i
html = unsafeCoerce E.html

i :: forall p i. Node () p i
i = unsafeCoerce E.i

iframe :: forall p i. NoninteractiveLeaf (onLoad :: I, sandbox :: I, scrolling :: I, src :: I, srcdoc :: I, width :: I, height :: I) p i
iframe = unsafeCoerce E.iframe

img :: forall p i. Leaf (alt :: I, crossorigin :: I, height :: I, ismap :: I, longdesc :: I, onAbort :: I, onError :: I, onLoad :: I, src :: I, usemap :: I, width :: I) p i
img = unsafeCoerce E.img

input :: forall p i. Leaf (accept :: I, autocomplete :: I, autofocus :: I, checked :: I, disabled :: I, form :: I, formaction :: I, formenctype :: I, formmethod :: I, formnovalidate :: I, formtarget :: I, height :: I, list :: I, max :: I, min :: I, multiple :: I, onAbort :: I, onChange :: I, onError :: I, onInput :: I, onInvalid :: I, onLoad :: I, onSearch :: I, onSelect :: I, pattern :: I, placeholder :: I, readonly :: I, required :: I, size :: I, src :: I, step :: I, inputType :: I, value :: I, width :: I) p i
input = unsafeCoerce E.input

ins :: forall p i. Node (cite :: I, datetime :: I) p i
ins = unsafeCoerce E.ins

kbd :: forall p i. Node () p i
kbd = unsafeCoerce E.kbd

keygen :: forall p i. Leaf (autofocus :: I, challenge :: I, disabled :: I, form :: I, keytype :: I, onChange :: I, onReset :: I, onSelect :: I, onSubmit :: I) p i
keygen = unsafeCoerce E.keygen

label :: forall p i. Node (for :: I, form :: I) p i
label = unsafeCoerce E.label

legend :: forall p i. Node () p i
legend = unsafeCoerce E.legend

li :: forall p i. Node (value :: I, onScroll :: I) p i
li = unsafeCoerce E.li

link :: forall p i. Leaf (crossorigin :: I, href :: I, hreflang :: I, media :: I, onLoad :: I, rel :: I, sizes :: I, mediaType :: I) p i
link = unsafeCoerce E.link

main :: forall p i. Node () p i
main = unsafeCoerce E.main

map :: forall p i. Node () p i
map = unsafeCoerce E.map

mark :: forall p i. Node () p i
mark = unsafeCoerce E.mark

menu :: forall p i. Node (label :: I, onScroll :: I, menuType :: I) p i
menu = unsafeCoerce E.menu

menuitem :: forall p i. Node (checked :: I, command :: I, default :: I, disabled :: I, icon :: I, label :: I, radiogroup :: I, menuitemType :: I) p i
menuitem = unsafeCoerce E.menuitem

meta :: forall p i. NoninteractiveLeaf (charset :: I, content :: I, httpEquiv :: I) p i
meta = unsafeCoerce E.meta

meter :: forall p i. Node (form :: I, high :: I, low :: I, max :: I, min :: I, optimum :: I, value :: I) p i
meter = unsafeCoerce E.meter

nav :: forall p i. Node () p i
nav = unsafeCoerce E.nav

noframes :: forall p i. Node () p i
noframes = unsafeCoerce E.noframes

noscript :: forall p i. Node () p i
noscript = unsafeCoerce E.noscript

object :: forall p i. Node (data :: I, form :: I, height :: I, onError :: I, onScroll :: I, mediaType :: I, usemap :: I, width :: I) p i
object = unsafeCoerce E.object

ol :: forall p i. Node (onScroll :: I, reversed :: I, start :: I, olType :: I) p i
ol = unsafeCoerce E.ol

optgroup :: forall p i. Node (disabled :: I, label :: I) p i
optgroup = unsafeCoerce E.optgroup

option :: forall p i. Node (disabled :: I, label :: I, selected :: I, value :: I) p i
option = unsafeCoerce E.option

output :: forall p i. Node (for :: I, form :: I) p i
output = unsafeCoerce E.output

p :: forall p i. Node (onScroll :: I) p i
p = unsafeCoerce E.p

param :: forall p i. NoninteractiveLeaf (value :: I) p i
param = unsafeCoerce E.param

pre :: forall p i. Node (onScroll :: I) p i
pre = unsafeCoerce E.pre

progress :: forall p i. Node (max :: I, value :: I) p i
progress = unsafeCoerce E.progress

q :: forall p i. Node (cite :: I) p i
q = unsafeCoerce E.q

rp :: forall p i. Node () p i
rp = unsafeCoerce E.rp

rt :: forall p i. Node () p i
rt = unsafeCoerce E.rt

ruby :: forall p i. Node () p i
ruby = unsafeCoerce E.ruby

s :: forall p i. Node () p i
s = unsafeCoerce E.s

samp :: forall p i. Node () p i
samp = unsafeCoerce E.samp

script :: forall p i. NoninteractiveNode (async :: I, charset :: I, defer :: I, onError :: I, onLoad :: I, src :: I, mediaType :: I) p i
script = unsafeCoerce E.script

section :: forall p i. Node () p i
section = unsafeCoerce E.section

select :: forall p i. Node (autofocus :: I, disabled :: I, form :: I, multiple :: I, onChange :: I, onScroll :: I, required :: I, size :: I, value :: I) p i
select = unsafeCoerce E.select

small :: forall p i. Node () p i
small = unsafeCoerce E.small

source :: forall p i. Leaf (media :: I, src :: I, mediaType :: I) p i
source = unsafeCoerce E.source

span :: forall p i. Node () p i
span = unsafeCoerce E.span

strong :: forall p i. Node () p i
strong = unsafeCoerce E.strong

style :: forall p i. NoninteractiveNode (media :: I, onError :: I, onLoad :: I, scoped :: I, mediaType :: I) p i
style = unsafeCoerce E.style

sub :: forall p i. Node () p i
sub = unsafeCoerce E.sub

summary :: forall p i. Node () p i
summary = unsafeCoerce E.summary

sup :: forall p i. Node () p i
sup = unsafeCoerce E.sup

table :: forall p i. Node (sortable :: I) p i
table = unsafeCoerce E.table

tbody :: forall p i. Node (onScroll :: I) p i
tbody = unsafeCoerce E.tbody

td :: forall p i. Node (colSpan :: I, headers :: I, rowSpan :: I) p i
td = unsafeCoerce E.td

textarea :: forall p i. Leaf (autofocus :: I, cols :: I, disabled :: I, form :: I, maxlength :: I, onChange :: I, onInput :: I, onScroll :: I, onSelect :: I, placeholder :: I, readonly :: I, required :: I, rows :: I, value :: I, wrap :: I) p i
textarea = unsafeCoerce E.textarea

tfoot :: forall p i. Node (onScroll :: I) p i
tfoot = unsafeCoerce E.tfoot

th :: forall p i. Node (abbr :: I, colSpan :: I, headers :: I, rowSpan :: I, scope :: I, sorted :: I) p i
th = unsafeCoerce E.th

thead :: forall p i. Node () p i
thead = unsafeCoerce E.thead

time :: forall p i. Node (datetime :: I) p i
time = unsafeCoerce E.time

title :: forall p i. NoninteractiveNode () p i
title = unsafeCoerce E.title

tr :: forall p i. Node () p i
tr = unsafeCoerce E.tr

track :: forall p i. Leaf (default :: I, kind :: I, label :: I, src :: I, srclang :: I) p i
track = unsafeCoerce E.track

tt :: forall p i. Node () p i
tt = unsafeCoerce E.tt

u :: forall p i. Node () p i
u = unsafeCoerce E.u

ul :: forall p i. Node (onScroll :: I) p i
ul = unsafeCoerce E.ul

var :: forall p i. Node () p i
var = unsafeCoerce E.var

video :: forall p i. Node (autoplay :: I, controls :: I, height :: I, loop :: I, muted :: I, poster :: I, preload :: I, src :: I, width :: I) p i
video = unsafeCoerce E.video

wbr :: forall p i. Leaf () p i
wbr = unsafeCoerce E.wbr

