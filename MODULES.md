# Module Documentation

## Module Halogen


The main module of the Halogen library. It defines functions for running applications
assembled from the parts defined in the various submodules:

- `Halogen.Signal` for responding to inputs and maintaining state
- `Halogen.HTML.*` for templating HTML documents
- `Halogen.Themes.*` for rendering using common front-end libraries
- `Halogen.Mixin.*` for common additional application features

The type signature and documentation for the [`runUI`](#runUI) function provides a good introduction 
to this library. For more advanced use-cases, you might like to look at the `runUIEff` function instead.


#### `HalogenEffects`

``` purescript
type HalogenEffects eff = (dom :: DOM, ref :: Ref | eff)
```

Wraps the effects required by the `runUI` and `runUIEff` functions.

#### `changes`

``` purescript
changes :: VTree -> SF VTree Patch
```

A signal which emits patches corresponding to successive `VTree`s.

This function can be used to create alternative top-level handlers which use `virtual-dom`.

#### `runUI`

``` purescript
runUI :: forall i eff. (forall a. SF1 i (HTML a i)) -> Eff (HalogenEffects eff) Node
```

`runUI` takes a UI represented as a signal function, and renders it to the DOM
using `virtual-dom`.

The signal function is responsible for rendering the HTML for the UI, and the 
HTML can generate inputs which will be fed back into the signal function,
resulting in DOM updates.

This function returns a `Node`, and the caller is responsible for adding the node
to the DOM.

As a simple example, we can create a signal which responds to button clicks:

```purescript
ui :: SF1 Unit (HTML Unit)
ui = view <$> stateful 0 (\n _ -> n + 1)
  where
  view :: Number -> HTML Unit
  view n = button [ onclick (const unit) ] [ text (show n) ]
```

#### `Handler`

``` purescript
type Handler r i eff = r -> Driver i eff -> Eff (HalogenEffects eff) Unit
```

This type synonym is provided to tidy up the type signature of `runUIEff`.

The _handler function_ is responsible for receiving requests from the UI, integrating with external
components, and providing inputs back to the system based on the results.

For example:

```purescript
data Input = SetDateAndTime DateAndTime | ...

data Request = GetDateAndTimeRequest | ...

appHandler :: forall eff. Handler Request Input eff 
appHandler GetDateAndTimeRequest k =
  get "/date" \response -> k (readDateAndTime response)
```

#### `Driver`

``` purescript
type Driver i eff = i -> Eff (HalogenEffects eff) Unit
```

This type synonym is provided to tidy up the type signature of `runUIEff`.

The _driver function_ can be used by the caller to inject additional inputs into the system at the top-level.

This is useful for supporting applications which respond to external events which originate
outside the UI, such as timers or hash-change events.

For example, to drive the UI with a `Tick` input every second, we might write something like the following:

```purescript
main = do
  Tuple node driver <- runUIEff ui absurd handler
  appendToBody node
  setInterval 1000 $ driver Tick
```

#### `runUIEff`

``` purescript
runUIEff :: forall i a r eff. SF1 i (HTML a (Either i r)) -> (a -> VTree) -> Handler r i eff -> Eff (HalogenEffects eff) (Tuple Node (Driver i eff))
```

`runUIEff` is a more general version of `runUI` which can be used to construct other
top-level handlers for applications.

`runUIEff` takes a signal function which creates HTML documents containing _requests_, 
and a handler function which accepts requests and provides new inputs to a continuation as they
become available.

For example, the handler function might be responsible for issuing AJAX requests on behalf of the
application.

In this way, all effects are pushed to the handler function at the boundary of the application.



## Module Data.Void

#### `Void`

``` purescript
data Void
```

An empty data type

#### `absurd`

``` purescript
absurd :: forall a. Void -> a
```

Since the `Void` type has no inhabitants, we can eliminate it producing any type whatsoever.


## Module Halogen.HTML


This module defines the HTML types required by the Halogen library, and provides
smart constructors for HTML5 elements.

#### `AttributeName`

``` purescript
newtype AttributeName
```

A type-safe wrapper for attribute names

#### `attributeName`

``` purescript
attributeName :: String -> AttributeName
```

#### `runAttributeName`

``` purescript
runAttributeName :: AttributeName -> String
```

Unpack an attribute name

#### `AttributeValue`

``` purescript
data AttributeValue i
  = StringAttribute String
  | BooleanAttribute Boolean
  | MapAttribute (StrMap String)
  | HandlerAttribute (Foreign -> EventHandler (Maybe i))
```

The type `AttributeValue i` represents values which can appear inside HTML attributes.
Values are either strings, booleans, maps or event handlers. Event handlers are required to produce outputs of type `i`.

#### `functorAttributeValue`

``` purescript
instance functorAttributeValue :: Functor AttributeValue
```


#### `Attribute`

``` purescript
data Attribute i
  = Attribute [Tuple AttributeName (AttributeValue i)]
```

A value of type `Attribute i` represents a collection of HTML attributes, whose
event handlers produce outputs of type `i`.

The `Semigroup` instance allows attributes to be combined.

#### `functorAttribute`

``` purescript
instance functorAttribute :: Functor Attribute
```


#### `semigroupAttribute`

``` purescript
instance semigroupAttribute :: Semigroup (Attribute i)
```


#### `monoidAttribute`

``` purescript
instance monoidAttribute :: Monoid (Attribute i)
```


#### `attributesToProps`

``` purescript
attributesToProps :: forall i eff. (i -> Eff eff Unit) -> Attribute i -> Props
```

Convert a collection of attributes to an immutable property collection by providing an event handler.

This function uses a temporary mutable property collection for efficiency.

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
data HTML a i
  = Text String
  | Element TagName (Attribute i) [HTML a i]
  | Placeholder a
```

The `HTML` type represents HTML documents before being rendered to the virtual DOM, and ultimately,
the actual DOM.

This representation is useful because it supports various typed transformations. It also gives a 
strongly-typed representation for the events which can be generated by a document.

The type parameter `a` corresponds to holes in the document which may be filled with `VTree`s during
rendering. This way, the `HTML` type is kept pure while supporting custom rendering, e.g. embedding
third-party components.

The type parameter `i` represents the type of events which can be generated by this document.

The meanings of the constructors of this type are as follows:

- `Text`, `Element` - regular text and element nodes
- `Placeholder` - A placeholder for a document. This can be replaced during rendering or by using the 
  `graft` operation.

#### `functorHTML`

``` purescript
instance functorHTML :: Functor (HTML a)
```


#### `graft`

``` purescript
graft :: forall a b i. HTML a i -> (a -> HTML b i) -> HTML b i
```

Replace placeholder nodes with HTML documents.

#### `renderHtml'`

``` purescript
renderHtml' :: forall i a eff. (i -> Eff eff Unit) -> (a -> VTree) -> HTML a i -> VTree
```

A more general version of `renderHtml'`.

The first argument is an event handler.

The second argument is used to replace placeholder nodes. If you are not using placeholder nodes, you
might prefer to use `renderHtml` instead.

#### `renderHtml`

``` purescript
renderHtml :: forall i eff. (i -> Eff eff Unit) -> (forall a. HTML a i) -> VTree
```

Render a `HTML` document to a virtual DOM node

#### `renderHtmlToString`

``` purescript
renderHtmlToString :: (forall a i. HTML a i) -> String
```

Render a HTML document as a `String`, usually for testing purposes.

The rank-2 type ensures that neither events nor placeholders are allowed.

#### `text`

``` purescript
text :: forall a i. String -> HTML a i
```

Create a HTML document which represents a text node.

#### `placeholder`

``` purescript
placeholder :: forall a i. a -> HTML a i
```

Create a HTML document which acts as a placeholder for a `VTree` to be rendered later.

This function is useful when embedding third-party widgets in HTML documents using the `widget` function, and
is considered an advanced feature. Use at your own risk.

#### `a`

``` purescript
a :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `a_`

``` purescript
a_ :: forall a i. [HTML a i] -> HTML a i
```


#### `abbr`

``` purescript
abbr :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `abbr_`

``` purescript
abbr_ :: forall a i. [HTML a i] -> HTML a i
```


#### `acronym`

``` purescript
acronym :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `acronym_`

``` purescript
acronym_ :: forall a i. [HTML a i] -> HTML a i
```


#### `address`

``` purescript
address :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `address_`

``` purescript
address_ :: forall a i. [HTML a i] -> HTML a i
```


#### `applet`

``` purescript
applet :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `applet_`

``` purescript
applet_ :: forall a i. [HTML a i] -> HTML a i
```


#### `area`

``` purescript
area :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `area_`

``` purescript
area_ :: forall a i. [HTML a i] -> HTML a i
```


#### `article`

``` purescript
article :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `article_`

``` purescript
article_ :: forall a i. [HTML a i] -> HTML a i
```


#### `aside`

``` purescript
aside :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `aside_`

``` purescript
aside_ :: forall a i. [HTML a i] -> HTML a i
```


#### `audio`

``` purescript
audio :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `audio_`

``` purescript
audio_ :: forall a i. [HTML a i] -> HTML a i
```


#### `b`

``` purescript
b :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `b_`

``` purescript
b_ :: forall a i. [HTML a i] -> HTML a i
```


#### `base`

``` purescript
base :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `base_`

``` purescript
base_ :: forall a i. [HTML a i] -> HTML a i
```


#### `basefont`

``` purescript
basefont :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `basefont_`

``` purescript
basefont_ :: forall a i. [HTML a i] -> HTML a i
```


#### `bdi`

``` purescript
bdi :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `bdi_`

``` purescript
bdi_ :: forall a i. [HTML a i] -> HTML a i
```


#### `bdo`

``` purescript
bdo :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `bdo_`

``` purescript
bdo_ :: forall a i. [HTML a i] -> HTML a i
```


#### `big`

``` purescript
big :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `big_`

``` purescript
big_ :: forall a i. [HTML a i] -> HTML a i
```


#### `blockquote`

``` purescript
blockquote :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `blockquote_`

``` purescript
blockquote_ :: forall a i. [HTML a i] -> HTML a i
```


#### `body`

``` purescript
body :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `body_`

``` purescript
body_ :: forall a i. [HTML a i] -> HTML a i
```


#### `br`

``` purescript
br :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `br_`

``` purescript
br_ :: forall a i. [HTML a i] -> HTML a i
```


#### `button`

``` purescript
button :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `button_`

``` purescript
button_ :: forall a i. [HTML a i] -> HTML a i
```


#### `canvas`

``` purescript
canvas :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `canvas_`

``` purescript
canvas_ :: forall a i. [HTML a i] -> HTML a i
```


#### `caption`

``` purescript
caption :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `caption_`

``` purescript
caption_ :: forall a i. [HTML a i] -> HTML a i
```


#### `center`

``` purescript
center :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `center_`

``` purescript
center_ :: forall a i. [HTML a i] -> HTML a i
```


#### `cite`

``` purescript
cite :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `cite_`

``` purescript
cite_ :: forall a i. [HTML a i] -> HTML a i
```


#### `code`

``` purescript
code :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `code_`

``` purescript
code_ :: forall a i. [HTML a i] -> HTML a i
```


#### `col`

``` purescript
col :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `col_`

``` purescript
col_ :: forall a i. [HTML a i] -> HTML a i
```


#### `colgroup`

``` purescript
colgroup :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `colgroup_`

``` purescript
colgroup_ :: forall a i. [HTML a i] -> HTML a i
```


#### `datalist`

``` purescript
datalist :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `datalist_`

``` purescript
datalist_ :: forall a i. [HTML a i] -> HTML a i
```


#### `dd`

``` purescript
dd :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `dd_`

``` purescript
dd_ :: forall a i. [HTML a i] -> HTML a i
```


#### `del`

``` purescript
del :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `del_`

``` purescript
del_ :: forall a i. [HTML a i] -> HTML a i
```


#### `details`

``` purescript
details :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `details_`

``` purescript
details_ :: forall a i. [HTML a i] -> HTML a i
```


#### `dfn`

``` purescript
dfn :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `dfn_`

``` purescript
dfn_ :: forall a i. [HTML a i] -> HTML a i
```


#### `dialog`

``` purescript
dialog :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `dialog_`

``` purescript
dialog_ :: forall a i. [HTML a i] -> HTML a i
```


#### `dir`

``` purescript
dir :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `dir_`

``` purescript
dir_ :: forall a i. [HTML a i] -> HTML a i
```


#### `div`

``` purescript
div :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `div_`

``` purescript
div_ :: forall a i. [HTML a i] -> HTML a i
```


#### `dl`

``` purescript
dl :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `dl_`

``` purescript
dl_ :: forall a i. [HTML a i] -> HTML a i
```


#### `dt`

``` purescript
dt :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `dt_`

``` purescript
dt_ :: forall a i. [HTML a i] -> HTML a i
```


#### `em`

``` purescript
em :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `em_`

``` purescript
em_ :: forall a i. [HTML a i] -> HTML a i
```


#### `embed`

``` purescript
embed :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `embed_`

``` purescript
embed_ :: forall a i. [HTML a i] -> HTML a i
```


#### `fieldset`

``` purescript
fieldset :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `fieldset_`

``` purescript
fieldset_ :: forall a i. [HTML a i] -> HTML a i
```


#### `figcaption`

``` purescript
figcaption :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `figcaption_`

``` purescript
figcaption_ :: forall a i. [HTML a i] -> HTML a i
```


#### `figure`

``` purescript
figure :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `figure_`

``` purescript
figure_ :: forall a i. [HTML a i] -> HTML a i
```


#### `font`

``` purescript
font :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `font_`

``` purescript
font_ :: forall a i. [HTML a i] -> HTML a i
```


#### `footer`

``` purescript
footer :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `footer_`

``` purescript
footer_ :: forall a i. [HTML a i] -> HTML a i
```


#### `form`

``` purescript
form :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `form_`

``` purescript
form_ :: forall a i. [HTML a i] -> HTML a i
```


#### `frame`

``` purescript
frame :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `frame_`

``` purescript
frame_ :: forall a i. [HTML a i] -> HTML a i
```


#### `frameset`

``` purescript
frameset :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `frameset_`

``` purescript
frameset_ :: forall a i. [HTML a i] -> HTML a i
```


#### `h1`

``` purescript
h1 :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `h1_`

``` purescript
h1_ :: forall a i. [HTML a i] -> HTML a i
```


#### `h2`

``` purescript
h2 :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `h2_`

``` purescript
h2_ :: forall a i. [HTML a i] -> HTML a i
```


#### `h3`

``` purescript
h3 :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `h3_`

``` purescript
h3_ :: forall a i. [HTML a i] -> HTML a i
```


#### `h4`

``` purescript
h4 :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `h4_`

``` purescript
h4_ :: forall a i. [HTML a i] -> HTML a i
```


#### `h5`

``` purescript
h5 :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `h5_`

``` purescript
h5_ :: forall a i. [HTML a i] -> HTML a i
```


#### `h6`

``` purescript
h6 :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `h6_`

``` purescript
h6_ :: forall a i. [HTML a i] -> HTML a i
```


#### `head`

``` purescript
head :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `head_`

``` purescript
head_ :: forall a i. [HTML a i] -> HTML a i
```


#### `header`

``` purescript
header :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `header_`

``` purescript
header_ :: forall a i. [HTML a i] -> HTML a i
```


#### `hr`

``` purescript
hr :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `hr_`

``` purescript
hr_ :: forall a i. [HTML a i] -> HTML a i
```


#### `html`

``` purescript
html :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `html_`

``` purescript
html_ :: forall a i. [HTML a i] -> HTML a i
```


#### `i`

``` purescript
i :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `i_`

``` purescript
i_ :: forall a i. [HTML a i] -> HTML a i
```


#### `iframe`

``` purescript
iframe :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `iframe_`

``` purescript
iframe_ :: forall a i. [HTML a i] -> HTML a i
```


#### `img`

``` purescript
img :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `img_`

``` purescript
img_ :: forall a i. [HTML a i] -> HTML a i
```


#### `input`

``` purescript
input :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `input_`

``` purescript
input_ :: forall a i. [HTML a i] -> HTML a i
```


#### `ins`

``` purescript
ins :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `ins_`

``` purescript
ins_ :: forall a i. [HTML a i] -> HTML a i
```


#### `kbd`

``` purescript
kbd :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `kbd_`

``` purescript
kbd_ :: forall a i. [HTML a i] -> HTML a i
```


#### `keygen`

``` purescript
keygen :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `keygen_`

``` purescript
keygen_ :: forall a i. [HTML a i] -> HTML a i
```


#### `label`

``` purescript
label :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `label_`

``` purescript
label_ :: forall a i. [HTML a i] -> HTML a i
```


#### `legend`

``` purescript
legend :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `legend_`

``` purescript
legend_ :: forall a i. [HTML a i] -> HTML a i
```


#### `li`

``` purescript
li :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `li_`

``` purescript
li_ :: forall a i. [HTML a i] -> HTML a i
```


#### `link`

``` purescript
link :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `link_`

``` purescript
link_ :: forall a i. [HTML a i] -> HTML a i
```


#### `main`

``` purescript
main :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `main_`

``` purescript
main_ :: forall a i. [HTML a i] -> HTML a i
```


#### `map`

``` purescript
map :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `map_`

``` purescript
map_ :: forall a i. [HTML a i] -> HTML a i
```


#### `mark`

``` purescript
mark :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `mark_`

``` purescript
mark_ :: forall a i. [HTML a i] -> HTML a i
```


#### `menu`

``` purescript
menu :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `menu_`

``` purescript
menu_ :: forall a i. [HTML a i] -> HTML a i
```


#### `menuitem`

``` purescript
menuitem :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `menuitem_`

``` purescript
menuitem_ :: forall a i. [HTML a i] -> HTML a i
```


#### `meta`

``` purescript
meta :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `meta_`

``` purescript
meta_ :: forall a i. [HTML a i] -> HTML a i
```


#### `meter`

``` purescript
meter :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `meter_`

``` purescript
meter_ :: forall a i. [HTML a i] -> HTML a i
```


#### `nav`

``` purescript
nav :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `nav_`

``` purescript
nav_ :: forall a i. [HTML a i] -> HTML a i
```


#### `noframes`

``` purescript
noframes :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `noframes_`

``` purescript
noframes_ :: forall a i. [HTML a i] -> HTML a i
```


#### `noscript`

``` purescript
noscript :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `noscript_`

``` purescript
noscript_ :: forall a i. [HTML a i] -> HTML a i
```


#### `object`

``` purescript
object :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `object_`

``` purescript
object_ :: forall a i. [HTML a i] -> HTML a i
```


#### `ol`

``` purescript
ol :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `ol_`

``` purescript
ol_ :: forall a i. [HTML a i] -> HTML a i
```


#### `optgroup`

``` purescript
optgroup :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `optgroup_`

``` purescript
optgroup_ :: forall a i. [HTML a i] -> HTML a i
```


#### `option`

``` purescript
option :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `option_`

``` purescript
option_ :: forall a i. [HTML a i] -> HTML a i
```


#### `output`

``` purescript
output :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `output_`

``` purescript
output_ :: forall a i. [HTML a i] -> HTML a i
```


#### `p`

``` purescript
p :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `p_`

``` purescript
p_ :: forall a i. [HTML a i] -> HTML a i
```


#### `param`

``` purescript
param :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `param_`

``` purescript
param_ :: forall a i. [HTML a i] -> HTML a i
```


#### `pre`

``` purescript
pre :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `pre_`

``` purescript
pre_ :: forall a i. [HTML a i] -> HTML a i
```


#### `progress`

``` purescript
progress :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `progress_`

``` purescript
progress_ :: forall a i. [HTML a i] -> HTML a i
```


#### `q`

``` purescript
q :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `q_`

``` purescript
q_ :: forall a i. [HTML a i] -> HTML a i
```


#### `rp`

``` purescript
rp :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `rp_`

``` purescript
rp_ :: forall a i. [HTML a i] -> HTML a i
```


#### `rt`

``` purescript
rt :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `rt_`

``` purescript
rt_ :: forall a i. [HTML a i] -> HTML a i
```


#### `ruby`

``` purescript
ruby :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `ruby_`

``` purescript
ruby_ :: forall a i. [HTML a i] -> HTML a i
```


#### `s`

``` purescript
s :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `s_`

``` purescript
s_ :: forall a i. [HTML a i] -> HTML a i
```


#### `samp`

``` purescript
samp :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `samp_`

``` purescript
samp_ :: forall a i. [HTML a i] -> HTML a i
```


#### `script`

``` purescript
script :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `script_`

``` purescript
script_ :: forall a i. [HTML a i] -> HTML a i
```


#### `section`

``` purescript
section :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `section_`

``` purescript
section_ :: forall a i. [HTML a i] -> HTML a i
```


#### `select`

``` purescript
select :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `select_`

``` purescript
select_ :: forall a i. [HTML a i] -> HTML a i
```


#### `small`

``` purescript
small :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `small_`

``` purescript
small_ :: forall a i. [HTML a i] -> HTML a i
```


#### `source`

``` purescript
source :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `source_`

``` purescript
source_ :: forall a i. [HTML a i] -> HTML a i
```


#### `span`

``` purescript
span :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `span_`

``` purescript
span_ :: forall a i. [HTML a i] -> HTML a i
```


#### `strike`

``` purescript
strike :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `strike_`

``` purescript
strike_ :: forall a i. [HTML a i] -> HTML a i
```


#### `strong`

``` purescript
strong :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `strong_`

``` purescript
strong_ :: forall a i. [HTML a i] -> HTML a i
```


#### `style`

``` purescript
style :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `style_`

``` purescript
style_ :: forall a i. [HTML a i] -> HTML a i
```


#### `sub`

``` purescript
sub :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `sub_`

``` purescript
sub_ :: forall a i. [HTML a i] -> HTML a i
```


#### `summary`

``` purescript
summary :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `summary_`

``` purescript
summary_ :: forall a i. [HTML a i] -> HTML a i
```


#### `sup`

``` purescript
sup :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `sup_`

``` purescript
sup_ :: forall a i. [HTML a i] -> HTML a i
```


#### `table`

``` purescript
table :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `table_`

``` purescript
table_ :: forall a i. [HTML a i] -> HTML a i
```


#### `tbody`

``` purescript
tbody :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `tbody_`

``` purescript
tbody_ :: forall a i. [HTML a i] -> HTML a i
```


#### `td`

``` purescript
td :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `td_`

``` purescript
td_ :: forall a i. [HTML a i] -> HTML a i
```


#### `textarea`

``` purescript
textarea :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `textarea_`

``` purescript
textarea_ :: forall a i. [HTML a i] -> HTML a i
```


#### `tfoot`

``` purescript
tfoot :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `tfoot_`

``` purescript
tfoot_ :: forall a i. [HTML a i] -> HTML a i
```


#### `th`

``` purescript
th :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `th_`

``` purescript
th_ :: forall a i. [HTML a i] -> HTML a i
```


#### `thead`

``` purescript
thead :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `thead_`

``` purescript
thead_ :: forall a i. [HTML a i] -> HTML a i
```


#### `time`

``` purescript
time :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `time_`

``` purescript
time_ :: forall a i. [HTML a i] -> HTML a i
```


#### `title`

``` purescript
title :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `title_`

``` purescript
title_ :: forall a i. [HTML a i] -> HTML a i
```


#### `tr`

``` purescript
tr :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `tr_`

``` purescript
tr_ :: forall a i. [HTML a i] -> HTML a i
```


#### `track`

``` purescript
track :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `track_`

``` purescript
track_ :: forall a i. [HTML a i] -> HTML a i
```


#### `tt`

``` purescript
tt :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `tt_`

``` purescript
tt_ :: forall a i. [HTML a i] -> HTML a i
```


#### `u`

``` purescript
u :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `u_`

``` purescript
u_ :: forall a i. [HTML a i] -> HTML a i
```


#### `ul`

``` purescript
ul :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `ul_`

``` purescript
ul_ :: forall a i. [HTML a i] -> HTML a i
```


#### `var`

``` purescript
var :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `var_`

``` purescript
var_ :: forall a i. [HTML a i] -> HTML a i
```


#### `video`

``` purescript
video :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `video_`

``` purescript
video_ :: forall a i. [HTML a i] -> HTML a i
```


#### `wbr`

``` purescript
wbr :: forall a i. Attribute i -> [HTML a i] -> HTML a i
```


#### `wbr_`

``` purescript
wbr_ :: forall a i. [HTML a i] -> HTML a i
```



## Module Halogen.Signal


This module defines signal functions (`SF`) and non-empty signal functions (`SF1`) and combinators
for working with them.

#### `SF`

``` purescript
newtype SF i o
```

A `SF` represents a state machine which responds to inputs of type `i`, producing outputs of type `o`.

#### `runSF`

``` purescript
runSF :: forall i o. SF i o -> i -> SF1 i o
```

Run a `SF` by providing an input

#### `SF1`

``` purescript
newtype SF1 i o
```

`SF1` represents non-empty signals, i.e. signals with an initial output value.

#### `runSF1`

``` purescript
runSF1 :: forall i o. SF1 i o -> { next :: SF i o, result :: o }
```

Run a `SF1` to obtain the initial value and remaining signal

#### `arr`

``` purescript
arr :: forall i o. (i -> o) -> SF i o
```

Create a `SF` from a function  

#### `input`

``` purescript
input :: forall i. SF i i
```

A `SF` which returns the latest input

#### `startingAt`

``` purescript
startingAt :: forall i o. SF i o -> o -> SF1 i o
```

Convert a `SF` to a `SF1` by providing an initial value

#### `head`

``` purescript
head :: forall i o. SF1 i o -> o
```

Get the current value of a `SF1`

#### `tail`

``` purescript
tail :: forall i o. SF1 i o -> SF i o
```

Convert a `SF1` to a `SF` by ignoring its initial value

#### `stateful`

``` purescript
stateful :: forall s i o. s -> (s -> i -> s) -> SF1 i s
```

Creates a stateful `SF1`

#### `stateful'`

``` purescript
stateful' :: forall s i o. s -> (s -> i -> Tuple o s) -> SF i o
```

Creates a stateful `SF` based on a function which returns an output value

#### `differencesWith`

``` purescript
differencesWith :: forall i d. (i -> i -> d) -> i -> SF i d
```

A `SF` which compares consecutive inputs using a helper function

#### `loop`

``` purescript
loop :: forall s i o. s -> SF (Tuple s i) (Tuple s o) -> SF i o
```

Create a `SF` which hides a piece of internal state of type `s`.

#### `functorSF`

``` purescript
instance functorSF :: Functor (SF i)
```


#### `functorSF1`

``` purescript
instance functorSF1 :: Functor (SF1 i)
```


#### `applySF`

``` purescript
instance applySF :: Apply (SF i)
```


#### `applySF1`

``` purescript
instance applySF1 :: Apply (SF1 i)
```


#### `applicativeSF`

``` purescript
instance applicativeSF :: Applicative (SF i)
```


#### `applicativeSF1`

``` purescript
instance applicativeSF1 :: Applicative (SF1 i)
```


#### `profunctorSF`

``` purescript
instance profunctorSF :: Profunctor SF
```


#### `profunctorSF1`

``` purescript
instance profunctorSF1 :: Profunctor SF1
```


#### `strongSF`

``` purescript
instance strongSF :: Strong SF
```


#### `choiceSF`

``` purescript
instance choiceSF :: Choice SF
```


#### `semigroupoidSF`

``` purescript
instance semigroupoidSF :: Semigroupoid SF
```


#### `semigroupoidSF1`

``` purescript
instance semigroupoidSF1 :: Semigroupoid SF1
```


#### `categorySF`

``` purescript
instance categorySF :: Category SF
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

#### `addClass`

``` purescript
addClass :: forall i. ClassName -> H.Attribute i -> H.Attribute i
```

#### `attribute`

``` purescript
attribute :: forall i value. H.AttributeName -> String -> H.Attribute i
```

This function can be used to define custom attributes.

#### `alt`

``` purescript
alt :: forall i. String -> H.Attribute i
```


#### `charset`

``` purescript
charset :: forall i. String -> H.Attribute i
```


#### `class_`

``` purescript
class_ :: forall i. ClassName -> H.Attribute i
```


#### `classes`

``` purescript
classes :: forall i. [ClassName] -> H.Attribute i
```


#### `content`

``` purescript
content :: forall i. String -> H.Attribute i
```


#### `for`

``` purescript
for :: forall i. String -> H.Attribute i
```


#### `height`

``` purescript
height :: forall i. Number -> H.Attribute i
```


#### `href`

``` purescript
href :: forall i. String -> H.Attribute i
```


#### `httpEquiv`

``` purescript
httpEquiv :: forall i. String -> H.Attribute i
```


#### `id_`

``` purescript
id_ :: forall i. String -> H.Attribute i
```


#### `name`

``` purescript
name :: forall i. String -> H.Attribute i
```


#### `rel`

``` purescript
rel :: forall i. String -> H.Attribute i
```


#### `src`

``` purescript
src :: forall i. String -> H.Attribute i
```


#### `target`

``` purescript
target :: forall i. String -> H.Attribute i
```


#### `title`

``` purescript
title :: forall i. String -> H.Attribute i
```


#### `type_`

``` purescript
type_ :: forall i. String -> H.Attribute i
```


#### `value`

``` purescript
value :: forall i. String -> H.Attribute i
```


#### `width`

``` purescript
width :: forall i. Number -> H.Attribute i
```


#### `disabled`

``` purescript
disabled :: forall i. Boolean -> H.Attribute i
```


#### `enabled`

``` purescript
enabled :: forall i. Boolean -> H.Attribute i
```


#### `checked`

``` purescript
checked :: forall i. Boolean -> H.Attribute i
```


#### `placeholder`

``` purescript
placeholder :: forall i. String -> H.Attribute i
```


#### `style`

``` purescript
style :: forall i. StrMap String -> H.Attribute i
```



## Module Halogen.HTML.Events


This module defines well-typed wrappers for common DOM events, so that
they may be safely embedded in HTML documents.

#### `onabort`

``` purescript
onabort :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onbeforeunload`

``` purescript
onbeforeunload :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onerror`

``` purescript
onerror :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onhashchange`

``` purescript
onhashchange :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onload`

``` purescript
onload :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onpageshow`

``` purescript
onpageshow :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onpagehide`

``` purescript
onpagehide :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onresize`

``` purescript
onresize :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onscroll`

``` purescript
onscroll :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onunload`

``` purescript
onunload :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onchange`

``` purescript
onchange :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `oninput`

``` purescript
oninput :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `oninvalid`

``` purescript
oninvalid :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onreset`

``` purescript
onreset :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onsearch`

``` purescript
onsearch :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onselect`

``` purescript
onselect :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onsubmit`

``` purescript
onsubmit :: forall i. (Event () -> EventHandler i) -> H.Attribute i
```


#### `onclick`

``` purescript
onclick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
```


#### `oncontextmenu`

``` purescript
oncontextmenu :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
```


#### `ondblclick`

``` purescript
ondblclick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
```


#### `onmousedown`

``` purescript
onmousedown :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
```


#### `onmouseenter`

``` purescript
onmouseenter :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
```


#### `onmouseleave`

``` purescript
onmouseleave :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
```


#### `onmousemove`

``` purescript
onmousemove :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
```


#### `onmouseover`

``` purescript
onmouseover :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
```


#### `onmouseout`

``` purescript
onmouseout :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
```


#### `onmouseup`

``` purescript
onmouseup :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
```


#### `onkeydown`

``` purescript
onkeydown :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attribute i
```


#### `onkeypress`

``` purescript
onkeypress :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attribute i
```


#### `onkeyup`

``` purescript
onkeyup :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attribute i
```


#### `onblur`

``` purescript
onblur :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attribute i
```


#### `onfocus`

``` purescript
onfocus :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attribute i
```


#### `onfocusin`

``` purescript
onfocusin :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attribute i
```


#### `onfocusout`

``` purescript
onfocusout :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attribute i
```



## Module Halogen.Mixin.Aff


Helper functions for working with the `Aff` monad.

#### `SupportsErrors`

``` purescript
class SupportsErrors input where
  liftError :: Error -> input
```

This type class identifies those input types which support errors

#### `HandlerAff`

``` purescript
type HandlerAff r i eff = r -> Aff (HalogenEffects eff) i
```

This type synonym is provided to tidy up the signature of `runUIAff`.

#### `runUIAff`

``` purescript
runUIAff :: forall i a r eff. (SupportsErrors i) => SF1 i (HTML a (Either i r)) -> (a -> VTree) -> HandlerAff r i eff -> EffA (HalogenEffects eff) (Tuple Node (Driver i eff))
```

A convenience function which uses the `Aff` monad to represent the handler function.


## Module Halogen.Mixin.Router


This module provides helper functions for working with URL hashes.

#### `Hash`

``` purescript
newtype Hash
```

A type-safe wrapper for the hash component of a URL

#### `runHash`

``` purescript
runHash :: Hash -> String
```

Unwrap a `Hash` to get a `String`.

#### `onHashChange`

``` purescript
onHashChange :: forall i eff. (Hash -> i) -> Driver i eff -> Eff (HalogenEffects eff) Unit
```

Listen for hash change events, and provide an input to the driver function when one occurs.


## Module Halogen.Mixin.UndoRedo


This module provides a generic undo/redo capability.

#### `UndoRedoInput`

``` purescript
data UndoRedoInput
  = Undo 
  | Redo 
```

Adds two new input types:

- `Undo` - move to the previous state
- `Redo` - move to the next state

#### `SupportsUndoRedo`

``` purescript
class SupportsUndoRedo input where
  fromUndoRedo :: UndoRedoInput -> input
  toUndoRedo :: input -> Maybe UndoRedoInput
```

This type class identifies those input types which support the Undo and Redo actions

#### `undo`

``` purescript
undo :: forall i. (SupportsUndoRedo i) => i
```

The undo action

#### `redo`

``` purescript
redo :: forall i. (SupportsUndoRedo i) => i
```

The redo action

#### `UndoRedoState`

``` purescript
data UndoRedoState s
```

Modifies the state type to include its _past_ and _future_.

#### `canUndo`

``` purescript
canUndo :: forall s. UndoRedoState s -> Boolean
```

`true` if the state supports the undo operation. 

#### `canRedo`

``` purescript
canRedo :: forall s. UndoRedoState s -> Boolean
```

`true` if the state supports the redo operation.

#### `getState`

``` purescript
getState :: forall s. UndoRedoState s -> s
```

Get the state at the current time

#### `undoRedoState`

``` purescript
undoRedoState :: forall s. s -> UndoRedoState s
```

Create a state with no past and no future

#### `withUndoRedo`

``` purescript
withUndoRedo :: forall s i. (SupportsUndoRedo i) => (s -> i -> s) -> UndoRedoState s -> i -> UndoRedoState s
```

Lift a step function to support the undo and redo operations.

The view should use the `canUndo` and `canRedo` functions to determine whether or not
to enable the corresponding controls.


## Module Halogen.Themes.Bootstrap3


This module provides CSS class names for common Bootstrap 3 classes.

#### `active`

``` purescript
active :: ClassName
```


#### `affix`

``` purescript
affix :: ClassName
```


#### `alert`

``` purescript
alert :: ClassName
```


#### `alertDanger`

``` purescript
alertDanger :: ClassName
```


#### `alertDismissable`

``` purescript
alertDismissable :: ClassName
```


#### `alertDismissible`

``` purescript
alertDismissible :: ClassName
```


#### `alertInfo`

``` purescript
alertInfo :: ClassName
```


#### `alertLink`

``` purescript
alertLink :: ClassName
```


#### `alertSuccess`

``` purescript
alertSuccess :: ClassName
```


#### `alertWarning`

``` purescript
alertWarning :: ClassName
```


#### `arrow`

``` purescript
arrow :: ClassName
```


#### `badge`

``` purescript
badge :: ClassName
```


#### `bgDanger`

``` purescript
bgDanger :: ClassName
```


#### `bgInfo`

``` purescript
bgInfo :: ClassName
```


#### `bgPrimary`

``` purescript
bgPrimary :: ClassName
```


#### `bgSuccess`

``` purescript
bgSuccess :: ClassName
```


#### `bgWarning`

``` purescript
bgWarning :: ClassName
```


#### `blockquoteReverse`

``` purescript
blockquoteReverse :: ClassName
```


#### `bottom`

``` purescript
bottom :: ClassName
```


#### `bottomLeft`

``` purescript
bottomLeft :: ClassName
```


#### `bottomRight`

``` purescript
bottomRight :: ClassName
```


#### `breadcrumb`

``` purescript
breadcrumb :: ClassName
```


#### `btn`

``` purescript
btn :: ClassName
```


#### `btnBlock`

``` purescript
btnBlock :: ClassName
```


#### `btnDanger`

``` purescript
btnDanger :: ClassName
```


#### `btnDefault`

``` purescript
btnDefault :: ClassName
```


#### `btnGroup`

``` purescript
btnGroup :: ClassName
```


#### `btnGroupJustified`

``` purescript
btnGroupJustified :: ClassName
```


#### `btnGroupLg`

``` purescript
btnGroupLg :: ClassName
```


#### `btnGroupSm`

``` purescript
btnGroupSm :: ClassName
```


#### `btnGroupVertical`

``` purescript
btnGroupVertical :: ClassName
```


#### `btnGroupXs`

``` purescript
btnGroupXs :: ClassName
```


#### `btnInfo`

``` purescript
btnInfo :: ClassName
```


#### `btnLg`

``` purescript
btnLg :: ClassName
```


#### `btnLink`

``` purescript
btnLink :: ClassName
```


#### `btnPrimary`

``` purescript
btnPrimary :: ClassName
```


#### `btnSm`

``` purescript
btnSm :: ClassName
```


#### `btnSuccess`

``` purescript
btnSuccess :: ClassName
```


#### `btnToolbar`

``` purescript
btnToolbar :: ClassName
```


#### `btnWarning`

``` purescript
btnWarning :: ClassName
```


#### `btnXs`

``` purescript
btnXs :: ClassName
```


#### `caption`

``` purescript
caption :: ClassName
```


#### `caret`

``` purescript
caret :: ClassName
```


#### `carousel`

``` purescript
carousel :: ClassName
```


#### `carouselCaption`

``` purescript
carouselCaption :: ClassName
```


#### `carouselControl`

``` purescript
carouselControl :: ClassName
```


#### `carouselIndicators`

``` purescript
carouselIndicators :: ClassName
```


#### `carouselInner`

``` purescript
carouselInner :: ClassName
```


#### `centerBlock`

``` purescript
centerBlock :: ClassName
```


#### `checkbox`

``` purescript
checkbox :: ClassName
```


#### `checkboxInline`

``` purescript
checkboxInline :: ClassName
```


#### `clearfix`

``` purescript
clearfix :: ClassName
```


#### `close`

``` purescript
close :: ClassName
```


#### `colLg1`

``` purescript
colLg1 :: ClassName
```


#### `colLg10`

``` purescript
colLg10 :: ClassName
```


#### `colLg11`

``` purescript
colLg11 :: ClassName
```


#### `colLg12`

``` purescript
colLg12 :: ClassName
```


#### `colLg2`

``` purescript
colLg2 :: ClassName
```


#### `colLg3`

``` purescript
colLg3 :: ClassName
```


#### `colLg4`

``` purescript
colLg4 :: ClassName
```


#### `colLg5`

``` purescript
colLg5 :: ClassName
```


#### `colLg6`

``` purescript
colLg6 :: ClassName
```


#### `colLg7`

``` purescript
colLg7 :: ClassName
```


#### `colLg8`

``` purescript
colLg8 :: ClassName
```


#### `colLg9`

``` purescript
colLg9 :: ClassName
```


#### `colLgOffset0`

``` purescript
colLgOffset0 :: ClassName
```


#### `colLgOffset1`

``` purescript
colLgOffset1 :: ClassName
```


#### `colLgOffset10`

``` purescript
colLgOffset10 :: ClassName
```


#### `colLgOffset11`

``` purescript
colLgOffset11 :: ClassName
```


#### `colLgOffset12`

``` purescript
colLgOffset12 :: ClassName
```


#### `colLgOffset2`

``` purescript
colLgOffset2 :: ClassName
```


#### `colLgOffset3`

``` purescript
colLgOffset3 :: ClassName
```


#### `colLgOffset4`

``` purescript
colLgOffset4 :: ClassName
```


#### `colLgOffset5`

``` purescript
colLgOffset5 :: ClassName
```


#### `colLgOffset6`

``` purescript
colLgOffset6 :: ClassName
```


#### `colLgOffset7`

``` purescript
colLgOffset7 :: ClassName
```


#### `colLgOffset8`

``` purescript
colLgOffset8 :: ClassName
```


#### `colLgOffset9`

``` purescript
colLgOffset9 :: ClassName
```


#### `colLgPull0`

``` purescript
colLgPull0 :: ClassName
```


#### `colLgPull1`

``` purescript
colLgPull1 :: ClassName
```


#### `colLgPull10`

``` purescript
colLgPull10 :: ClassName
```


#### `colLgPull11`

``` purescript
colLgPull11 :: ClassName
```


#### `colLgPull12`

``` purescript
colLgPull12 :: ClassName
```


#### `colLgPull2`

``` purescript
colLgPull2 :: ClassName
```


#### `colLgPull3`

``` purescript
colLgPull3 :: ClassName
```


#### `colLgPull4`

``` purescript
colLgPull4 :: ClassName
```


#### `colLgPull5`

``` purescript
colLgPull5 :: ClassName
```


#### `colLgPull6`

``` purescript
colLgPull6 :: ClassName
```


#### `colLgPull7`

``` purescript
colLgPull7 :: ClassName
```


#### `colLgPull8`

``` purescript
colLgPull8 :: ClassName
```


#### `colLgPull9`

``` purescript
colLgPull9 :: ClassName
```


#### `colLgPush0`

``` purescript
colLgPush0 :: ClassName
```


#### `colLgPush1`

``` purescript
colLgPush1 :: ClassName
```


#### `colLgPush10`

``` purescript
colLgPush10 :: ClassName
```


#### `colLgPush11`

``` purescript
colLgPush11 :: ClassName
```


#### `colLgPush12`

``` purescript
colLgPush12 :: ClassName
```


#### `colLgPush2`

``` purescript
colLgPush2 :: ClassName
```


#### `colLgPush3`

``` purescript
colLgPush3 :: ClassName
```


#### `colLgPush4`

``` purescript
colLgPush4 :: ClassName
```


#### `colLgPush5`

``` purescript
colLgPush5 :: ClassName
```


#### `colLgPush6`

``` purescript
colLgPush6 :: ClassName
```


#### `colLgPush7`

``` purescript
colLgPush7 :: ClassName
```


#### `colLgPush8`

``` purescript
colLgPush8 :: ClassName
```


#### `colLgPush9`

``` purescript
colLgPush9 :: ClassName
```


#### `colMd1`

``` purescript
colMd1 :: ClassName
```


#### `colMd10`

``` purescript
colMd10 :: ClassName
```


#### `colMd11`

``` purescript
colMd11 :: ClassName
```


#### `colMd12`

``` purescript
colMd12 :: ClassName
```


#### `colMd2`

``` purescript
colMd2 :: ClassName
```


#### `colMd3`

``` purescript
colMd3 :: ClassName
```


#### `colMd4`

``` purescript
colMd4 :: ClassName
```


#### `colMd5`

``` purescript
colMd5 :: ClassName
```


#### `colMd6`

``` purescript
colMd6 :: ClassName
```


#### `colMd7`

``` purescript
colMd7 :: ClassName
```


#### `colMd8`

``` purescript
colMd8 :: ClassName
```


#### `colMd9`

``` purescript
colMd9 :: ClassName
```


#### `colMdOffset0`

``` purescript
colMdOffset0 :: ClassName
```


#### `colMdOffset1`

``` purescript
colMdOffset1 :: ClassName
```


#### `colMdOffset10`

``` purescript
colMdOffset10 :: ClassName
```


#### `colMdOffset11`

``` purescript
colMdOffset11 :: ClassName
```


#### `colMdOffset12`

``` purescript
colMdOffset12 :: ClassName
```


#### `colMdOffset2`

``` purescript
colMdOffset2 :: ClassName
```


#### `colMdOffset3`

``` purescript
colMdOffset3 :: ClassName
```


#### `colMdOffset4`

``` purescript
colMdOffset4 :: ClassName
```


#### `colMdOffset5`

``` purescript
colMdOffset5 :: ClassName
```


#### `colMdOffset6`

``` purescript
colMdOffset6 :: ClassName
```


#### `colMdOffset7`

``` purescript
colMdOffset7 :: ClassName
```


#### `colMdOffset8`

``` purescript
colMdOffset8 :: ClassName
```


#### `colMdOffset9`

``` purescript
colMdOffset9 :: ClassName
```


#### `colMdPull0`

``` purescript
colMdPull0 :: ClassName
```


#### `colMdPull1`

``` purescript
colMdPull1 :: ClassName
```


#### `colMdPull10`

``` purescript
colMdPull10 :: ClassName
```


#### `colMdPull11`

``` purescript
colMdPull11 :: ClassName
```


#### `colMdPull12`

``` purescript
colMdPull12 :: ClassName
```


#### `colMdPull2`

``` purescript
colMdPull2 :: ClassName
```


#### `colMdPull3`

``` purescript
colMdPull3 :: ClassName
```


#### `colMdPull4`

``` purescript
colMdPull4 :: ClassName
```


#### `colMdPull5`

``` purescript
colMdPull5 :: ClassName
```


#### `colMdPull6`

``` purescript
colMdPull6 :: ClassName
```


#### `colMdPull7`

``` purescript
colMdPull7 :: ClassName
```


#### `colMdPull8`

``` purescript
colMdPull8 :: ClassName
```


#### `colMdPull9`

``` purescript
colMdPull9 :: ClassName
```


#### `colMdPush0`

``` purescript
colMdPush0 :: ClassName
```


#### `colMdPush1`

``` purescript
colMdPush1 :: ClassName
```


#### `colMdPush10`

``` purescript
colMdPush10 :: ClassName
```


#### `colMdPush11`

``` purescript
colMdPush11 :: ClassName
```


#### `colMdPush12`

``` purescript
colMdPush12 :: ClassName
```


#### `colMdPush2`

``` purescript
colMdPush2 :: ClassName
```


#### `colMdPush3`

``` purescript
colMdPush3 :: ClassName
```


#### `colMdPush4`

``` purescript
colMdPush4 :: ClassName
```


#### `colMdPush5`

``` purescript
colMdPush5 :: ClassName
```


#### `colMdPush6`

``` purescript
colMdPush6 :: ClassName
```


#### `colMdPush7`

``` purescript
colMdPush7 :: ClassName
```


#### `colMdPush8`

``` purescript
colMdPush8 :: ClassName
```


#### `colMdPush9`

``` purescript
colMdPush9 :: ClassName
```


#### `colSm1`

``` purescript
colSm1 :: ClassName
```


#### `colSm10`

``` purescript
colSm10 :: ClassName
```


#### `colSm11`

``` purescript
colSm11 :: ClassName
```


#### `colSm12`

``` purescript
colSm12 :: ClassName
```


#### `colSm2`

``` purescript
colSm2 :: ClassName
```


#### `colSm3`

``` purescript
colSm3 :: ClassName
```


#### `colSm4`

``` purescript
colSm4 :: ClassName
```


#### `colSm5`

``` purescript
colSm5 :: ClassName
```


#### `colSm6`

``` purescript
colSm6 :: ClassName
```


#### `colSm7`

``` purescript
colSm7 :: ClassName
```


#### `colSm8`

``` purescript
colSm8 :: ClassName
```


#### `colSm9`

``` purescript
colSm9 :: ClassName
```


#### `colSmOffset0`

``` purescript
colSmOffset0 :: ClassName
```


#### `colSmOffset1`

``` purescript
colSmOffset1 :: ClassName
```


#### `colSmOffset10`

``` purescript
colSmOffset10 :: ClassName
```


#### `colSmOffset11`

``` purescript
colSmOffset11 :: ClassName
```


#### `colSmOffset12`

``` purescript
colSmOffset12 :: ClassName
```


#### `colSmOffset2`

``` purescript
colSmOffset2 :: ClassName
```


#### `colSmOffset3`

``` purescript
colSmOffset3 :: ClassName
```


#### `colSmOffset4`

``` purescript
colSmOffset4 :: ClassName
```


#### `colSmOffset5`

``` purescript
colSmOffset5 :: ClassName
```


#### `colSmOffset6`

``` purescript
colSmOffset6 :: ClassName
```


#### `colSmOffset7`

``` purescript
colSmOffset7 :: ClassName
```


#### `colSmOffset8`

``` purescript
colSmOffset8 :: ClassName
```


#### `colSmOffset9`

``` purescript
colSmOffset9 :: ClassName
```


#### `colSmPull0`

``` purescript
colSmPull0 :: ClassName
```


#### `colSmPull1`

``` purescript
colSmPull1 :: ClassName
```


#### `colSmPull10`

``` purescript
colSmPull10 :: ClassName
```


#### `colSmPull11`

``` purescript
colSmPull11 :: ClassName
```


#### `colSmPull12`

``` purescript
colSmPull12 :: ClassName
```


#### `colSmPull2`

``` purescript
colSmPull2 :: ClassName
```


#### `colSmPull3`

``` purescript
colSmPull3 :: ClassName
```


#### `colSmPull4`

``` purescript
colSmPull4 :: ClassName
```


#### `colSmPull5`

``` purescript
colSmPull5 :: ClassName
```


#### `colSmPull6`

``` purescript
colSmPull6 :: ClassName
```


#### `colSmPull7`

``` purescript
colSmPull7 :: ClassName
```


#### `colSmPull8`

``` purescript
colSmPull8 :: ClassName
```


#### `colSmPull9`

``` purescript
colSmPull9 :: ClassName
```


#### `colSmPush0`

``` purescript
colSmPush0 :: ClassName
```


#### `colSmPush1`

``` purescript
colSmPush1 :: ClassName
```


#### `colSmPush10`

``` purescript
colSmPush10 :: ClassName
```


#### `colSmPush11`

``` purescript
colSmPush11 :: ClassName
```


#### `colSmPush12`

``` purescript
colSmPush12 :: ClassName
```


#### `colSmPush2`

``` purescript
colSmPush2 :: ClassName
```


#### `colSmPush3`

``` purescript
colSmPush3 :: ClassName
```


#### `colSmPush4`

``` purescript
colSmPush4 :: ClassName
```


#### `colSmPush5`

``` purescript
colSmPush5 :: ClassName
```


#### `colSmPush6`

``` purescript
colSmPush6 :: ClassName
```


#### `colSmPush7`

``` purescript
colSmPush7 :: ClassName
```


#### `colSmPush8`

``` purescript
colSmPush8 :: ClassName
```


#### `colSmPush9`

``` purescript
colSmPush9 :: ClassName
```


#### `colXs1`

``` purescript
colXs1 :: ClassName
```


#### `colXs10`

``` purescript
colXs10 :: ClassName
```


#### `colXs11`

``` purescript
colXs11 :: ClassName
```


#### `colXs12`

``` purescript
colXs12 :: ClassName
```


#### `colXs2`

``` purescript
colXs2 :: ClassName
```


#### `colXs3`

``` purescript
colXs3 :: ClassName
```


#### `colXs4`

``` purescript
colXs4 :: ClassName
```


#### `colXs5`

``` purescript
colXs5 :: ClassName
```


#### `colXs6`

``` purescript
colXs6 :: ClassName
```


#### `colXs7`

``` purescript
colXs7 :: ClassName
```


#### `colXs8`

``` purescript
colXs8 :: ClassName
```


#### `colXs9`

``` purescript
colXs9 :: ClassName
```


#### `colXsOffset0`

``` purescript
colXsOffset0 :: ClassName
```


#### `colXsOffset1`

``` purescript
colXsOffset1 :: ClassName
```


#### `colXsOffset10`

``` purescript
colXsOffset10 :: ClassName
```


#### `colXsOffset11`

``` purescript
colXsOffset11 :: ClassName
```


#### `colXsOffset12`

``` purescript
colXsOffset12 :: ClassName
```


#### `colXsOffset2`

``` purescript
colXsOffset2 :: ClassName
```


#### `colXsOffset3`

``` purescript
colXsOffset3 :: ClassName
```


#### `colXsOffset4`

``` purescript
colXsOffset4 :: ClassName
```


#### `colXsOffset5`

``` purescript
colXsOffset5 :: ClassName
```


#### `colXsOffset6`

``` purescript
colXsOffset6 :: ClassName
```


#### `colXsOffset7`

``` purescript
colXsOffset7 :: ClassName
```


#### `colXsOffset8`

``` purescript
colXsOffset8 :: ClassName
```


#### `colXsOffset9`

``` purescript
colXsOffset9 :: ClassName
```


#### `colXsPull0`

``` purescript
colXsPull0 :: ClassName
```


#### `colXsPull1`

``` purescript
colXsPull1 :: ClassName
```


#### `colXsPull10`

``` purescript
colXsPull10 :: ClassName
```


#### `colXsPull11`

``` purescript
colXsPull11 :: ClassName
```


#### `colXsPull12`

``` purescript
colXsPull12 :: ClassName
```


#### `colXsPull2`

``` purescript
colXsPull2 :: ClassName
```


#### `colXsPull3`

``` purescript
colXsPull3 :: ClassName
```


#### `colXsPull4`

``` purescript
colXsPull4 :: ClassName
```


#### `colXsPull5`

``` purescript
colXsPull5 :: ClassName
```


#### `colXsPull6`

``` purescript
colXsPull6 :: ClassName
```


#### `colXsPull7`

``` purescript
colXsPull7 :: ClassName
```


#### `colXsPull8`

``` purescript
colXsPull8 :: ClassName
```


#### `colXsPull9`

``` purescript
colXsPull9 :: ClassName
```


#### `colXsPush0`

``` purescript
colXsPush0 :: ClassName
```


#### `colXsPush1`

``` purescript
colXsPush1 :: ClassName
```


#### `colXsPush10`

``` purescript
colXsPush10 :: ClassName
```


#### `colXsPush11`

``` purescript
colXsPush11 :: ClassName
```


#### `colXsPush12`

``` purescript
colXsPush12 :: ClassName
```


#### `colXsPush2`

``` purescript
colXsPush2 :: ClassName
```


#### `colXsPush3`

``` purescript
colXsPush3 :: ClassName
```


#### `colXsPush4`

``` purescript
colXsPush4 :: ClassName
```


#### `colXsPush5`

``` purescript
colXsPush5 :: ClassName
```


#### `colXsPush6`

``` purescript
colXsPush6 :: ClassName
```


#### `colXsPush7`

``` purescript
colXsPush7 :: ClassName
```


#### `colXsPush8`

``` purescript
colXsPush8 :: ClassName
```


#### `colXsPush9`

``` purescript
colXsPush9 :: ClassName
```


#### `collapse`

``` purescript
collapse :: ClassName
```


#### `collapsing`

``` purescript
collapsing :: ClassName
```


#### `container`

``` purescript
container :: ClassName
```


#### `containerFluid`

``` purescript
containerFluid :: ClassName
```


#### `controlLabel`

``` purescript
controlLabel :: ClassName
```


#### `danger`

``` purescript
danger :: ClassName
```


#### `disabled`

``` purescript
disabled :: ClassName
```


#### `divider`

``` purescript
divider :: ClassName
```


#### `dlHorizontal`

``` purescript
dlHorizontal :: ClassName
```


#### `dropdown`

``` purescript
dropdown :: ClassName
```


#### `dropdownBackdrop`

``` purescript
dropdownBackdrop :: ClassName
```


#### `dropdownHeader`

``` purescript
dropdownHeader :: ClassName
```


#### `dropdownMenu`

``` purescript
dropdownMenu :: ClassName
```


#### `dropdownMenuLeft`

``` purescript
dropdownMenuLeft :: ClassName
```


#### `dropdownMenuRight`

``` purescript
dropdownMenuRight :: ClassName
```


#### `dropdownToggle`

``` purescript
dropdownToggle :: ClassName
```


#### `dropup`

``` purescript
dropup :: ClassName
```


#### `embedResponsive`

``` purescript
embedResponsive :: ClassName
```


#### `embedResponsive16By9`

``` purescript
embedResponsive16By9 :: ClassName
```


#### `embedResponsive4By3`

``` purescript
embedResponsive4By3 :: ClassName
```


#### `embedResponsiveItem`

``` purescript
embedResponsiveItem :: ClassName
```


#### `eot`

``` purescript
eot :: ClassName
```


#### `fade`

``` purescript
fade :: ClassName
```


#### `focus`

``` purescript
focus :: ClassName
```


#### `formControl`

``` purescript
formControl :: ClassName
```


#### `formControlFeedback`

``` purescript
formControlFeedback :: ClassName
```


#### `formControlStatic`

``` purescript
formControlStatic :: ClassName
```


#### `formGroup`

``` purescript
formGroup :: ClassName
```


#### `formGroupLg`

``` purescript
formGroupLg :: ClassName
```


#### `formGroupSm`

``` purescript
formGroupSm :: ClassName
```


#### `formHorizontal`

``` purescript
formHorizontal :: ClassName
```


#### `formInline`

``` purescript
formInline :: ClassName
```


#### `glyphicon`

``` purescript
glyphicon :: ClassName
```


#### `glyphiconAdjust`

``` purescript
glyphiconAdjust :: ClassName
```


#### `glyphiconAlert`

``` purescript
glyphiconAlert :: ClassName
```


#### `glyphiconAlignCenter`

``` purescript
glyphiconAlignCenter :: ClassName
```


#### `glyphiconAlignJustify`

``` purescript
glyphiconAlignJustify :: ClassName
```


#### `glyphiconAlignLeft`

``` purescript
glyphiconAlignLeft :: ClassName
```


#### `glyphiconAlignRight`

``` purescript
glyphiconAlignRight :: ClassName
```


#### `glyphiconApple`

``` purescript
glyphiconApple :: ClassName
```


#### `glyphiconArrowDown`

``` purescript
glyphiconArrowDown :: ClassName
```


#### `glyphiconArrowLeft`

``` purescript
glyphiconArrowLeft :: ClassName
```


#### `glyphiconArrowRight`

``` purescript
glyphiconArrowRight :: ClassName
```


#### `glyphiconArrowUp`

``` purescript
glyphiconArrowUp :: ClassName
```


#### `glyphiconAsterisk`

``` purescript
glyphiconAsterisk :: ClassName
```


#### `glyphiconBabyFormula`

``` purescript
glyphiconBabyFormula :: ClassName
```


#### `glyphiconBackward`

``` purescript
glyphiconBackward :: ClassName
```


#### `glyphiconBanCircle`

``` purescript
glyphiconBanCircle :: ClassName
```


#### `glyphiconBarcode`

``` purescript
glyphiconBarcode :: ClassName
```


#### `glyphiconBed`

``` purescript
glyphiconBed :: ClassName
```


#### `glyphiconBell`

``` purescript
glyphiconBell :: ClassName
```


#### `glyphiconBishop`

``` purescript
glyphiconBishop :: ClassName
```


#### `glyphiconBitcoin`

``` purescript
glyphiconBitcoin :: ClassName
```


#### `glyphiconBlackboard`

``` purescript
glyphiconBlackboard :: ClassName
```


#### `glyphiconBold`

``` purescript
glyphiconBold :: ClassName
```


#### `glyphiconBook`

``` purescript
glyphiconBook :: ClassName
```


#### `glyphiconBookmark`

``` purescript
glyphiconBookmark :: ClassName
```


#### `glyphiconBriefcase`

``` purescript
glyphiconBriefcase :: ClassName
```


#### `glyphiconBullhorn`

``` purescript
glyphiconBullhorn :: ClassName
```


#### `glyphiconCalendar`

``` purescript
glyphiconCalendar :: ClassName
```


#### `glyphiconCamera`

``` purescript
glyphiconCamera :: ClassName
```


#### `glyphiconCd`

``` purescript
glyphiconCd :: ClassName
```


#### `glyphiconCertificate`

``` purescript
glyphiconCertificate :: ClassName
```


#### `glyphiconCheck`

``` purescript
glyphiconCheck :: ClassName
```


#### `glyphiconChevronDown`

``` purescript
glyphiconChevronDown :: ClassName
```


#### `glyphiconChevronLeft`

``` purescript
glyphiconChevronLeft :: ClassName
```


#### `glyphiconChevronRight`

``` purescript
glyphiconChevronRight :: ClassName
```


#### `glyphiconChevronUp`

``` purescript
glyphiconChevronUp :: ClassName
```


#### `glyphiconCircleArrowDown`

``` purescript
glyphiconCircleArrowDown :: ClassName
```


#### `glyphiconCircleArrowLeft`

``` purescript
glyphiconCircleArrowLeft :: ClassName
```


#### `glyphiconCircleArrowRight`

``` purescript
glyphiconCircleArrowRight :: ClassName
```


#### `glyphiconCircleArrowUp`

``` purescript
glyphiconCircleArrowUp :: ClassName
```


#### `glyphiconCloud`

``` purescript
glyphiconCloud :: ClassName
```


#### `glyphiconCloudDownload`

``` purescript
glyphiconCloudDownload :: ClassName
```


#### `glyphiconCloudUpload`

``` purescript
glyphiconCloudUpload :: ClassName
```


#### `glyphiconCog`

``` purescript
glyphiconCog :: ClassName
```


#### `glyphiconCollapseDown`

``` purescript
glyphiconCollapseDown :: ClassName
```


#### `glyphiconCollapseUp`

``` purescript
glyphiconCollapseUp :: ClassName
```


#### `glyphiconComment`

``` purescript
glyphiconComment :: ClassName
```


#### `glyphiconCompressed`

``` purescript
glyphiconCompressed :: ClassName
```


#### `glyphiconConsole`

``` purescript
glyphiconConsole :: ClassName
```


#### `glyphiconCopy`

``` purescript
glyphiconCopy :: ClassName
```


#### `glyphiconCopyrightMark`

``` purescript
glyphiconCopyrightMark :: ClassName
```


#### `glyphiconCreditCard`

``` purescript
glyphiconCreditCard :: ClassName
```


#### `glyphiconCutlery`

``` purescript
glyphiconCutlery :: ClassName
```


#### `glyphiconDashboard`

``` purescript
glyphiconDashboard :: ClassName
```


#### `glyphiconDownload`

``` purescript
glyphiconDownload :: ClassName
```


#### `glyphiconDownloadAlt`

``` purescript
glyphiconDownloadAlt :: ClassName
```


#### `glyphiconDuplicate`

``` purescript
glyphiconDuplicate :: ClassName
```


#### `glyphiconEarphone`

``` purescript
glyphiconEarphone :: ClassName
```


#### `glyphiconEdit`

``` purescript
glyphiconEdit :: ClassName
```


#### `glyphiconEducation`

``` purescript
glyphiconEducation :: ClassName
```


#### `glyphiconEject`

``` purescript
glyphiconEject :: ClassName
```


#### `glyphiconEnvelope`

``` purescript
glyphiconEnvelope :: ClassName
```


#### `glyphiconEqualizer`

``` purescript
glyphiconEqualizer :: ClassName
```


#### `glyphiconErase`

``` purescript
glyphiconErase :: ClassName
```


#### `glyphiconEur`

``` purescript
glyphiconEur :: ClassName
```


#### `glyphiconEuro`

``` purescript
glyphiconEuro :: ClassName
```


#### `glyphiconExclamationSign`

``` purescript
glyphiconExclamationSign :: ClassName
```


#### `glyphiconExpand`

``` purescript
glyphiconExpand :: ClassName
```


#### `glyphiconExport`

``` purescript
glyphiconExport :: ClassName
```


#### `glyphiconEyeClose`

``` purescript
glyphiconEyeClose :: ClassName
```


#### `glyphiconEyeOpen`

``` purescript
glyphiconEyeOpen :: ClassName
```


#### `glyphiconFacetimeVideo`

``` purescript
glyphiconFacetimeVideo :: ClassName
```


#### `glyphiconFastBackward`

``` purescript
glyphiconFastBackward :: ClassName
```


#### `glyphiconFastForward`

``` purescript
glyphiconFastForward :: ClassName
```


#### `glyphiconFile`

``` purescript
glyphiconFile :: ClassName
```


#### `glyphiconFilm`

``` purescript
glyphiconFilm :: ClassName
```


#### `glyphiconFilter`

``` purescript
glyphiconFilter :: ClassName
```


#### `glyphiconFire`

``` purescript
glyphiconFire :: ClassName
```


#### `glyphiconFlag`

``` purescript
glyphiconFlag :: ClassName
```


#### `glyphiconFlash`

``` purescript
glyphiconFlash :: ClassName
```


#### `glyphiconFloppyDisk`

``` purescript
glyphiconFloppyDisk :: ClassName
```


#### `glyphiconFloppyOpen`

``` purescript
glyphiconFloppyOpen :: ClassName
```


#### `glyphiconFloppyRemove`

``` purescript
glyphiconFloppyRemove :: ClassName
```


#### `glyphiconFloppySave`

``` purescript
glyphiconFloppySave :: ClassName
```


#### `glyphiconFloppySaved`

``` purescript
glyphiconFloppySaved :: ClassName
```


#### `glyphiconFolderClose`

``` purescript
glyphiconFolderClose :: ClassName
```


#### `glyphiconFolderOpen`

``` purescript
glyphiconFolderOpen :: ClassName
```


#### `glyphiconFont`

``` purescript
glyphiconFont :: ClassName
```


#### `glyphiconForward`

``` purescript
glyphiconForward :: ClassName
```


#### `glyphiconFullscreen`

``` purescript
glyphiconFullscreen :: ClassName
```


#### `glyphiconGbp`

``` purescript
glyphiconGbp :: ClassName
```


#### `glyphiconGift`

``` purescript
glyphiconGift :: ClassName
```


#### `glyphiconGlass`

``` purescript
glyphiconGlass :: ClassName
```


#### `glyphiconGlobe`

``` purescript
glyphiconGlobe :: ClassName
```


#### `glyphiconGrain`

``` purescript
glyphiconGrain :: ClassName
```


#### `glyphiconHandDown`

``` purescript
glyphiconHandDown :: ClassName
```


#### `glyphiconHandLeft`

``` purescript
glyphiconHandLeft :: ClassName
```


#### `glyphiconHandRight`

``` purescript
glyphiconHandRight :: ClassName
```


#### `glyphiconHandUp`

``` purescript
glyphiconHandUp :: ClassName
```


#### `glyphiconHdVideo`

``` purescript
glyphiconHdVideo :: ClassName
```


#### `glyphiconHdd`

``` purescript
glyphiconHdd :: ClassName
```


#### `glyphiconHeader`

``` purescript
glyphiconHeader :: ClassName
```


#### `glyphiconHeadphones`

``` purescript
glyphiconHeadphones :: ClassName
```


#### `glyphiconHeart`

``` purescript
glyphiconHeart :: ClassName
```


#### `glyphiconHeartEmpty`

``` purescript
glyphiconHeartEmpty :: ClassName
```


#### `glyphiconHome`

``` purescript
glyphiconHome :: ClassName
```


#### `glyphiconHourglass`

``` purescript
glyphiconHourglass :: ClassName
```


#### `glyphiconIceLolly`

``` purescript
glyphiconIceLolly :: ClassName
```


#### `glyphiconIceLollyTasted`

``` purescript
glyphiconIceLollyTasted :: ClassName
```


#### `glyphiconImport`

``` purescript
glyphiconImport :: ClassName
```


#### `glyphiconInbox`

``` purescript
glyphiconInbox :: ClassName
```


#### `glyphiconIndentLeft`

``` purescript
glyphiconIndentLeft :: ClassName
```


#### `glyphiconIndentRight`

``` purescript
glyphiconIndentRight :: ClassName
```


#### `glyphiconInfoSign`

``` purescript
glyphiconInfoSign :: ClassName
```


#### `glyphiconItalic`

``` purescript
glyphiconItalic :: ClassName
```


#### `glyphiconKing`

``` purescript
glyphiconKing :: ClassName
```


#### `glyphiconKnight`

``` purescript
glyphiconKnight :: ClassName
```


#### `glyphiconLamp`

``` purescript
glyphiconLamp :: ClassName
```


#### `glyphiconLeaf`

``` purescript
glyphiconLeaf :: ClassName
```


#### `glyphiconLevelUp`

``` purescript
glyphiconLevelUp :: ClassName
```


#### `glyphiconLink`

``` purescript
glyphiconLink :: ClassName
```


#### `glyphiconList`

``` purescript
glyphiconList :: ClassName
```


#### `glyphiconListAlt`

``` purescript
glyphiconListAlt :: ClassName
```


#### `glyphiconLock`

``` purescript
glyphiconLock :: ClassName
```


#### `glyphiconLogIn`

``` purescript
glyphiconLogIn :: ClassName
```


#### `glyphiconLogOut`

``` purescript
glyphiconLogOut :: ClassName
```


#### `glyphiconMagnet`

``` purescript
glyphiconMagnet :: ClassName
```


#### `glyphiconMapMarker`

``` purescript
glyphiconMapMarker :: ClassName
```


#### `glyphiconMenuDown`

``` purescript
glyphiconMenuDown :: ClassName
```


#### `glyphiconMenuHamburger`

``` purescript
glyphiconMenuHamburger :: ClassName
```


#### `glyphiconMenuLeft`

``` purescript
glyphiconMenuLeft :: ClassName
```


#### `glyphiconMenuRight`

``` purescript
glyphiconMenuRight :: ClassName
```


#### `glyphiconMenuUp`

``` purescript
glyphiconMenuUp :: ClassName
```


#### `glyphiconMinus`

``` purescript
glyphiconMinus :: ClassName
```


#### `glyphiconMinusSign`

``` purescript
glyphiconMinusSign :: ClassName
```


#### `glyphiconModalWindow`

``` purescript
glyphiconModalWindow :: ClassName
```


#### `glyphiconMove`

``` purescript
glyphiconMove :: ClassName
```


#### `glyphiconMusic`

``` purescript
glyphiconMusic :: ClassName
```


#### `glyphiconNewWindow`

``` purescript
glyphiconNewWindow :: ClassName
```


#### `glyphiconObjectAlignBottom`

``` purescript
glyphiconObjectAlignBottom :: ClassName
```


#### `glyphiconObjectAlignHorizontal`

``` purescript
glyphiconObjectAlignHorizontal :: ClassName
```


#### `glyphiconObjectAlignLeft`

``` purescript
glyphiconObjectAlignLeft :: ClassName
```


#### `glyphiconObjectAlignRight`

``` purescript
glyphiconObjectAlignRight :: ClassName
```


#### `glyphiconObjectAlignTop`

``` purescript
glyphiconObjectAlignTop :: ClassName
```


#### `glyphiconObjectAlignVertical`

``` purescript
glyphiconObjectAlignVertical :: ClassName
```


#### `glyphiconOff`

``` purescript
glyphiconOff :: ClassName
```


#### `glyphiconOil`

``` purescript
glyphiconOil :: ClassName
```


#### `glyphiconOk`

``` purescript
glyphiconOk :: ClassName
```


#### `glyphiconOkCircle`

``` purescript
glyphiconOkCircle :: ClassName
```


#### `glyphiconOkSign`

``` purescript
glyphiconOkSign :: ClassName
```


#### `glyphiconOpen`

``` purescript
glyphiconOpen :: ClassName
```


#### `glyphiconOpenFile`

``` purescript
glyphiconOpenFile :: ClassName
```


#### `glyphiconOptionHorizontal`

``` purescript
glyphiconOptionHorizontal :: ClassName
```


#### `glyphiconOptionVertical`

``` purescript
glyphiconOptionVertical :: ClassName
```


#### `glyphiconPaperclip`

``` purescript
glyphiconPaperclip :: ClassName
```


#### `glyphiconPaste`

``` purescript
glyphiconPaste :: ClassName
```


#### `glyphiconPause`

``` purescript
glyphiconPause :: ClassName
```


#### `glyphiconPawn`

``` purescript
glyphiconPawn :: ClassName
```


#### `glyphiconPencil`

``` purescript
glyphiconPencil :: ClassName
```


#### `glyphiconPhone`

``` purescript
glyphiconPhone :: ClassName
```


#### `glyphiconPhoneAlt`

``` purescript
glyphiconPhoneAlt :: ClassName
```


#### `glyphiconPicture`

``` purescript
glyphiconPicture :: ClassName
```


#### `glyphiconPiggyBank`

``` purescript
glyphiconPiggyBank :: ClassName
```


#### `glyphiconPlane`

``` purescript
glyphiconPlane :: ClassName
```


#### `glyphiconPlay`

``` purescript
glyphiconPlay :: ClassName
```


#### `glyphiconPlayCircle`

``` purescript
glyphiconPlayCircle :: ClassName
```


#### `glyphiconPlus`

``` purescript
glyphiconPlus :: ClassName
```


#### `glyphiconPlusSign`

``` purescript
glyphiconPlusSign :: ClassName
```


#### `glyphiconPrint`

``` purescript
glyphiconPrint :: ClassName
```


#### `glyphiconPushpin`

``` purescript
glyphiconPushpin :: ClassName
```


#### `glyphiconQrcode`

``` purescript
glyphiconQrcode :: ClassName
```


#### `glyphiconQueen`

``` purescript
glyphiconQueen :: ClassName
```


#### `glyphiconQuestionSign`

``` purescript
glyphiconQuestionSign :: ClassName
```


#### `glyphiconRandom`

``` purescript
glyphiconRandom :: ClassName
```


#### `glyphiconRecord`

``` purescript
glyphiconRecord :: ClassName
```


#### `glyphiconRefresh`

``` purescript
glyphiconRefresh :: ClassName
```


#### `glyphiconRegistrationMark`

``` purescript
glyphiconRegistrationMark :: ClassName
```


#### `glyphiconRemove`

``` purescript
glyphiconRemove :: ClassName
```


#### `glyphiconRemoveCircle`

``` purescript
glyphiconRemoveCircle :: ClassName
```


#### `glyphiconRemoveSign`

``` purescript
glyphiconRemoveSign :: ClassName
```


#### `glyphiconRepeat`

``` purescript
glyphiconRepeat :: ClassName
```


#### `glyphiconResizeFull`

``` purescript
glyphiconResizeFull :: ClassName
```


#### `glyphiconResizeHorizontal`

``` purescript
glyphiconResizeHorizontal :: ClassName
```


#### `glyphiconResizeSmall`

``` purescript
glyphiconResizeSmall :: ClassName
```


#### `glyphiconResizeVertical`

``` purescript
glyphiconResizeVertical :: ClassName
```


#### `glyphiconRetweet`

``` purescript
glyphiconRetweet :: ClassName
```


#### `glyphiconRoad`

``` purescript
glyphiconRoad :: ClassName
```


#### `glyphiconRuble`

``` purescript
glyphiconRuble :: ClassName
```


#### `glyphiconSave`

``` purescript
glyphiconSave :: ClassName
```


#### `glyphiconSaveFile`

``` purescript
glyphiconSaveFile :: ClassName
```


#### `glyphiconSaved`

``` purescript
glyphiconSaved :: ClassName
```


#### `glyphiconScale`

``` purescript
glyphiconScale :: ClassName
```


#### `glyphiconScissors`

``` purescript
glyphiconScissors :: ClassName
```


#### `glyphiconScreenshot`

``` purescript
glyphiconScreenshot :: ClassName
```


#### `glyphiconSdVideo`

``` purescript
glyphiconSdVideo :: ClassName
```


#### `glyphiconSearch`

``` purescript
glyphiconSearch :: ClassName
```


#### `glyphiconSend`

``` purescript
glyphiconSend :: ClassName
```


#### `glyphiconShare`

``` purescript
glyphiconShare :: ClassName
```


#### `glyphiconShareAlt`

``` purescript
glyphiconShareAlt :: ClassName
```


#### `glyphiconShoppingCart`

``` purescript
glyphiconShoppingCart :: ClassName
```


#### `glyphiconSignal`

``` purescript
glyphiconSignal :: ClassName
```


#### `glyphiconSort`

``` purescript
glyphiconSort :: ClassName
```


#### `glyphiconSortByAlphabet`

``` purescript
glyphiconSortByAlphabet :: ClassName
```


#### `glyphiconSortByAlphabetAlt`

``` purescript
glyphiconSortByAlphabetAlt :: ClassName
```


#### `glyphiconSortByAttributes`

``` purescript
glyphiconSortByAttributes :: ClassName
```


#### `glyphiconSortByAttributesAlt`

``` purescript
glyphiconSortByAttributesAlt :: ClassName
```


#### `glyphiconSortByOrder`

``` purescript
glyphiconSortByOrder :: ClassName
```


#### `glyphiconSortByOrderAlt`

``` purescript
glyphiconSortByOrderAlt :: ClassName
```


#### `glyphiconSound5_1`

``` purescript
glyphiconSound5_1 :: ClassName
```


#### `glyphiconSound6_1`

``` purescript
glyphiconSound6_1 :: ClassName
```


#### `glyphiconSound7_1`

``` purescript
glyphiconSound7_1 :: ClassName
```


#### `glyphiconSoundDolby`

``` purescript
glyphiconSoundDolby :: ClassName
```


#### `glyphiconSoundStereo`

``` purescript
glyphiconSoundStereo :: ClassName
```


#### `glyphiconStar`

``` purescript
glyphiconStar :: ClassName
```


#### `glyphiconStarEmpty`

``` purescript
glyphiconStarEmpty :: ClassName
```


#### `glyphiconStats`

``` purescript
glyphiconStats :: ClassName
```


#### `glyphiconStepBackward`

``` purescript
glyphiconStepBackward :: ClassName
```


#### `glyphiconStepForward`

``` purescript
glyphiconStepForward :: ClassName
```


#### `glyphiconStop`

``` purescript
glyphiconStop :: ClassName
```


#### `glyphiconSubscript`

``` purescript
glyphiconSubscript :: ClassName
```


#### `glyphiconSubtitles`

``` purescript
glyphiconSubtitles :: ClassName
```


#### `glyphiconSunglasses`

``` purescript
glyphiconSunglasses :: ClassName
```


#### `glyphiconSuperscript`

``` purescript
glyphiconSuperscript :: ClassName
```


#### `glyphiconTag`

``` purescript
glyphiconTag :: ClassName
```


#### `glyphiconTags`

``` purescript
glyphiconTags :: ClassName
```


#### `glyphiconTasks`

``` purescript
glyphiconTasks :: ClassName
```


#### `glyphiconTent`

``` purescript
glyphiconTent :: ClassName
```


#### `glyphiconTextBackground`

``` purescript
glyphiconTextBackground :: ClassName
```


#### `glyphiconTextColor`

``` purescript
glyphiconTextColor :: ClassName
```


#### `glyphiconTextHeight`

``` purescript
glyphiconTextHeight :: ClassName
```


#### `glyphiconTextSize`

``` purescript
glyphiconTextSize :: ClassName
```


#### `glyphiconTextWidth`

``` purescript
glyphiconTextWidth :: ClassName
```


#### `glyphiconTh`

``` purescript
glyphiconTh :: ClassName
```


#### `glyphiconThLarge`

``` purescript
glyphiconThLarge :: ClassName
```


#### `glyphiconThList`

``` purescript
glyphiconThList :: ClassName
```


#### `glyphiconThumbsDown`

``` purescript
glyphiconThumbsDown :: ClassName
```


#### `glyphiconThumbsUp`

``` purescript
glyphiconThumbsUp :: ClassName
```


#### `glyphiconTime`

``` purescript
glyphiconTime :: ClassName
```


#### `glyphiconTint`

``` purescript
glyphiconTint :: ClassName
```


#### `glyphiconTower`

``` purescript
glyphiconTower :: ClassName
```


#### `glyphiconTransfer`

``` purescript
glyphiconTransfer :: ClassName
```


#### `glyphiconTrash`

``` purescript
glyphiconTrash :: ClassName
```


#### `glyphiconTreeConifer`

``` purescript
glyphiconTreeConifer :: ClassName
```


#### `glyphiconTreeDeciduous`

``` purescript
glyphiconTreeDeciduous :: ClassName
```


#### `glyphiconTriangleBottom`

``` purescript
glyphiconTriangleBottom :: ClassName
```


#### `glyphiconTriangleLeft`

``` purescript
glyphiconTriangleLeft :: ClassName
```


#### `glyphiconTriangleRight`

``` purescript
glyphiconTriangleRight :: ClassName
```


#### `glyphiconTriangleTop`

``` purescript
glyphiconTriangleTop :: ClassName
```


#### `glyphiconUnchecked`

``` purescript
glyphiconUnchecked :: ClassName
```


#### `glyphiconUpload`

``` purescript
glyphiconUpload :: ClassName
```


#### `glyphiconUsd`

``` purescript
glyphiconUsd :: ClassName
```


#### `glyphiconUser`

``` purescript
glyphiconUser :: ClassName
```


#### `glyphiconVolumeDown`

``` purescript
glyphiconVolumeDown :: ClassName
```


#### `glyphiconVolumeOff`

``` purescript
glyphiconVolumeOff :: ClassName
```


#### `glyphiconVolumeUp`

``` purescript
glyphiconVolumeUp :: ClassName
```


#### `glyphiconWarningSign`

``` purescript
glyphiconWarningSign :: ClassName
```


#### `glyphiconWrench`

``` purescript
glyphiconWrench :: ClassName
```


#### `glyphiconYen`

``` purescript
glyphiconYen :: ClassName
```


#### `glyphiconZoomIn`

``` purescript
glyphiconZoomIn :: ClassName
```


#### `glyphiconZoomOut`

``` purescript
glyphiconZoomOut :: ClassName
```


#### `gradient`

``` purescript
gradient :: ClassName
```


#### `h1`

``` purescript
h1 :: ClassName
```


#### `h2`

``` purescript
h2 :: ClassName
```


#### `h3`

``` purescript
h3 :: ClassName
```


#### `h4`

``` purescript
h4 :: ClassName
```


#### `h5`

``` purescript
h5 :: ClassName
```


#### `h6`

``` purescript
h6 :: ClassName
```


#### `hasError`

``` purescript
hasError :: ClassName
```


#### `hasFeedback`

``` purescript
hasFeedback :: ClassName
```


#### `hasSuccess`

``` purescript
hasSuccess :: ClassName
```


#### `hasWarning`

``` purescript
hasWarning :: ClassName
```


#### `helpBlock`

``` purescript
helpBlock :: ClassName
```


#### `hidden`

``` purescript
hidden :: ClassName
```


#### `hiddenLg`

``` purescript
hiddenLg :: ClassName
```


#### `hiddenMd`

``` purescript
hiddenMd :: ClassName
```


#### `hiddenPrint`

``` purescript
hiddenPrint :: ClassName
```


#### `hiddenSm`

``` purescript
hiddenSm :: ClassName
```


#### `hiddenXs`

``` purescript
hiddenXs :: ClassName
```


#### `hide`

``` purescript
hide :: ClassName
```


#### `iconBar`

``` purescript
iconBar :: ClassName
```


#### `iconNext`

``` purescript
iconNext :: ClassName
```


#### `iconPrev`

``` purescript
iconPrev :: ClassName
```


#### `imgCircle`

``` purescript
imgCircle :: ClassName
```


#### `imgResponsive`

``` purescript
imgResponsive :: ClassName
```


#### `imgRounded`

``` purescript
imgRounded :: ClassName
```


#### `imgThumbnail`

``` purescript
imgThumbnail :: ClassName
```


#### `in_`

``` purescript
in_ :: ClassName
```


#### `info`

``` purescript
info :: ClassName
```


#### `initialism`

``` purescript
initialism :: ClassName
```


#### `inputGroup`

``` purescript
inputGroup :: ClassName
```


#### `inputGroupAddon`

``` purescript
inputGroupAddon :: ClassName
```


#### `inputGroupBtn`

``` purescript
inputGroupBtn :: ClassName
```


#### `inputGroupLg`

``` purescript
inputGroupLg :: ClassName
```


#### `inputGroupSm`

``` purescript
inputGroupSm :: ClassName
```


#### `inputLg`

``` purescript
inputLg :: ClassName
```


#### `inputSm`

``` purescript
inputSm :: ClassName
```


#### `invisible`

``` purescript
invisible :: ClassName
```


#### `item`

``` purescript
item :: ClassName
```


#### `jumbotron`

``` purescript
jumbotron :: ClassName
```


#### `label`

``` purescript
label :: ClassName
```


#### `labelDanger`

``` purescript
labelDanger :: ClassName
```


#### `labelDefault`

``` purescript
labelDefault :: ClassName
```


#### `labelInfo`

``` purescript
labelInfo :: ClassName
```


#### `labelPrimary`

``` purescript
labelPrimary :: ClassName
```


#### `labelSuccess`

``` purescript
labelSuccess :: ClassName
```


#### `labelWarning`

``` purescript
labelWarning :: ClassName
```


#### `lead`

``` purescript
lead :: ClassName
```


#### `left`

``` purescript
left :: ClassName
```


#### `listGroup`

``` purescript
listGroup :: ClassName
```


#### `listGroupItem`

``` purescript
listGroupItem :: ClassName
```


#### `listGroupItemDanger`

``` purescript
listGroupItemDanger :: ClassName
```


#### `listGroupItemHeading`

``` purescript
listGroupItemHeading :: ClassName
```


#### `listGroupItemInfo`

``` purescript
listGroupItemInfo :: ClassName
```


#### `listGroupItemSuccess`

``` purescript
listGroupItemSuccess :: ClassName
```


#### `listGroupItemText`

``` purescript
listGroupItemText :: ClassName
```


#### `listGroupItemWarning`

``` purescript
listGroupItemWarning :: ClassName
```


#### `listInline`

``` purescript
listInline :: ClassName
```


#### `listUnstyled`

``` purescript
listUnstyled :: ClassName
```


#### `mark`

``` purescript
mark :: ClassName
```


#### `media`

``` purescript
media :: ClassName
```


#### `mediaBody`

``` purescript
mediaBody :: ClassName
```


#### `mediaBottom`

``` purescript
mediaBottom :: ClassName
```


#### `mediaHeading`

``` purescript
mediaHeading :: ClassName
```


#### `mediaLeft`

``` purescript
mediaLeft :: ClassName
```


#### `mediaList`

``` purescript
mediaList :: ClassName
```


#### `mediaMiddle`

``` purescript
mediaMiddle :: ClassName
```


#### `mediaObject`

``` purescript
mediaObject :: ClassName
```


#### `mediaRight`

``` purescript
mediaRight :: ClassName
```


#### `modal`

``` purescript
modal :: ClassName
```


#### `modalBackdrop`

``` purescript
modalBackdrop :: ClassName
```


#### `modalBody`

``` purescript
modalBody :: ClassName
```


#### `modalContent`

``` purescript
modalContent :: ClassName
```


#### `modalDialog`

``` purescript
modalDialog :: ClassName
```


#### `modalFooter`

``` purescript
modalFooter :: ClassName
```


#### `modalHeader`

``` purescript
modalHeader :: ClassName
```


#### `modalLg`

``` purescript
modalLg :: ClassName
```


#### `modalOpen`

``` purescript
modalOpen :: ClassName
```


#### `modalScrollbarMeasure`

``` purescript
modalScrollbarMeasure :: ClassName
```


#### `modalSm`

``` purescript
modalSm :: ClassName
```


#### `modalTitle`

``` purescript
modalTitle :: ClassName
```


#### `nav`

``` purescript
nav :: ClassName
```


#### `navDivider`

``` purescript
navDivider :: ClassName
```


#### `navJustified`

``` purescript
navJustified :: ClassName
```


#### `navPills`

``` purescript
navPills :: ClassName
```


#### `navStacked`

``` purescript
navStacked :: ClassName
```


#### `navTabs`

``` purescript
navTabs :: ClassName
```


#### `navTabsJustified`

``` purescript
navTabsJustified :: ClassName
```


#### `navbar`

``` purescript
navbar :: ClassName
```


#### `navbarBrand`

``` purescript
navbarBrand :: ClassName
```


#### `navbarBtn`

``` purescript
navbarBtn :: ClassName
```


#### `navbarCollapse`

``` purescript
navbarCollapse :: ClassName
```


#### `navbarDefault`

``` purescript
navbarDefault :: ClassName
```


#### `navbarFixedBottom`

``` purescript
navbarFixedBottom :: ClassName
```


#### `navbarFixedTop`

``` purescript
navbarFixedTop :: ClassName
```


#### `navbarForm`

``` purescript
navbarForm :: ClassName
```


#### `navbarHeader`

``` purescript
navbarHeader :: ClassName
```


#### `navbarInverse`

``` purescript
navbarInverse :: ClassName
```


#### `navbarLeft`

``` purescript
navbarLeft :: ClassName
```


#### `navbarLink`

``` purescript
navbarLink :: ClassName
```


#### `navbarNav`

``` purescript
navbarNav :: ClassName
```


#### `navbarRight`

``` purescript
navbarRight :: ClassName
```


#### `navbarStaticTop`

``` purescript
navbarStaticTop :: ClassName
```


#### `navbarText`

``` purescript
navbarText :: ClassName
```


#### `navbarToggle`

``` purescript
navbarToggle :: ClassName
```


#### `next`

``` purescript
next :: ClassName
```


#### `open`

``` purescript
open :: ClassName
```


#### `pageHeader`

``` purescript
pageHeader :: ClassName
```


#### `pager`

``` purescript
pager :: ClassName
```


#### `pagination`

``` purescript
pagination :: ClassName
```


#### `paginationLg`

``` purescript
paginationLg :: ClassName
```


#### `paginationSm`

``` purescript
paginationSm :: ClassName
```


#### `panel`

``` purescript
panel :: ClassName
```


#### `panelBody`

``` purescript
panelBody :: ClassName
```


#### `panelCollapse`

``` purescript
panelCollapse :: ClassName
```


#### `panelDanger`

``` purescript
panelDanger :: ClassName
```


#### `panelDefault`

``` purescript
panelDefault :: ClassName
```


#### `panelFooter`

``` purescript
panelFooter :: ClassName
```


#### `panelGroup`

``` purescript
panelGroup :: ClassName
```


#### `panelHeading`

``` purescript
panelHeading :: ClassName
```


#### `panelInfo`

``` purescript
panelInfo :: ClassName
```


#### `panelPrimary`

``` purescript
panelPrimary :: ClassName
```


#### `panelSuccess`

``` purescript
panelSuccess :: ClassName
```


#### `panelTitle`

``` purescript
panelTitle :: ClassName
```


#### `panelWarning`

``` purescript
panelWarning :: ClassName
```


#### `popover`

``` purescript
popover :: ClassName
```


#### `popoverContent`

``` purescript
popoverContent :: ClassName
```


#### `popoverTitle`

``` purescript
popoverTitle :: ClassName
```


#### `preScrollable`

``` purescript
preScrollable :: ClassName
```


#### `prev`

``` purescript
prev :: ClassName
```


#### `previous`

``` purescript
previous :: ClassName
```


#### `progress`

``` purescript
progress :: ClassName
```


#### `progressBar`

``` purescript
progressBar :: ClassName
```


#### `progressBarDanger`

``` purescript
progressBarDanger :: ClassName
```


#### `progressBarInfo`

``` purescript
progressBarInfo :: ClassName
```


#### `progressBarStriped`

``` purescript
progressBarStriped :: ClassName
```


#### `progressBarSuccess`

``` purescript
progressBarSuccess :: ClassName
```


#### `progressBarWarning`

``` purescript
progressBarWarning :: ClassName
```


#### `progressStriped`

``` purescript
progressStriped :: ClassName
```


#### `pullLeft`

``` purescript
pullLeft :: ClassName
```


#### `pullRight`

``` purescript
pullRight :: ClassName
```


#### `radio`

``` purescript
radio :: ClassName
```


#### `radioInline`

``` purescript
radioInline :: ClassName
```


#### `right`

``` purescript
right :: ClassName
```


#### `row`

``` purescript
row :: ClassName
```


#### `show_`

``` purescript
show_ :: ClassName
```


#### `small`

``` purescript
small :: ClassName
```


#### `srOnly`

``` purescript
srOnly :: ClassName
```


#### `srOnlyFocusable`

``` purescript
srOnlyFocusable :: ClassName
```


#### `success`

``` purescript
success :: ClassName
```


#### `svg`

``` purescript
svg :: ClassName
```


#### `tabContent`

``` purescript
tabContent :: ClassName
```


#### `tabPane`

``` purescript
tabPane :: ClassName
```


#### `table`

``` purescript
table :: ClassName
```


#### `tableBordered`

``` purescript
tableBordered :: ClassName
```


#### `tableCondensed`

``` purescript
tableCondensed :: ClassName
```


#### `tableHover`

``` purescript
tableHover :: ClassName
```


#### `tableResponsive`

``` purescript
tableResponsive :: ClassName
```


#### `tableStriped`

``` purescript
tableStriped :: ClassName
```


#### `textCapitalize`

``` purescript
textCapitalize :: ClassName
```


#### `textCenter`

``` purescript
textCenter :: ClassName
```


#### `textDanger`

``` purescript
textDanger :: ClassName
```


#### `textHide`

``` purescript
textHide :: ClassName
```


#### `textInfo`

``` purescript
textInfo :: ClassName
```


#### `textJustify`

``` purescript
textJustify :: ClassName
```


#### `textLeft`

``` purescript
textLeft :: ClassName
```


#### `textLowercase`

``` purescript
textLowercase :: ClassName
```


#### `textMuted`

``` purescript
textMuted :: ClassName
```


#### `textNowrap`

``` purescript
textNowrap :: ClassName
```


#### `textPrimary`

``` purescript
textPrimary :: ClassName
```


#### `textRight`

``` purescript
textRight :: ClassName
```


#### `textSuccess`

``` purescript
textSuccess :: ClassName
```


#### `textUppercase`

``` purescript
textUppercase :: ClassName
```


#### `textWarning`

``` purescript
textWarning :: ClassName
```


#### `thumbnail`

``` purescript
thumbnail :: ClassName
```


#### `tooltip`

``` purescript
tooltip :: ClassName
```


#### `tooltipArrow`

``` purescript
tooltipArrow :: ClassName
```


#### `tooltipInner`

``` purescript
tooltipInner :: ClassName
```


#### `top`

``` purescript
top :: ClassName
```


#### `topLeft`

``` purescript
topLeft :: ClassName
```


#### `topRight`

``` purescript
topRight :: ClassName
```


#### `ttf`

``` purescript
ttf :: ClassName
```


#### `visibleLg`

``` purescript
visibleLg :: ClassName
```


#### `visibleLgBlock`

``` purescript
visibleLgBlock :: ClassName
```


#### `visibleLgInline`

``` purescript
visibleLgInline :: ClassName
```


#### `visibleLgInlineBlock`

``` purescript
visibleLgInlineBlock :: ClassName
```


#### `visibleMd`

``` purescript
visibleMd :: ClassName
```


#### `visibleMdBlock`

``` purescript
visibleMdBlock :: ClassName
```


#### `visibleMdInline`

``` purescript
visibleMdInline :: ClassName
```


#### `visibleMdInlineBlock`

``` purescript
visibleMdInlineBlock :: ClassName
```


#### `visiblePrint`

``` purescript
visiblePrint :: ClassName
```


#### `visiblePrintBlock`

``` purescript
visiblePrintBlock :: ClassName
```


#### `visiblePrintInline`

``` purescript
visiblePrintInline :: ClassName
```


#### `visiblePrintInlineBlock`

``` purescript
visiblePrintInlineBlock :: ClassName
```


#### `visibleSm`

``` purescript
visibleSm :: ClassName
```


#### `visibleSmBlock`

``` purescript
visibleSmBlock :: ClassName
```


#### `visibleSmInline`

``` purescript
visibleSmInline :: ClassName
```


#### `visibleSmInlineBlock`

``` purescript
visibleSmInlineBlock :: ClassName
```


#### `visibleXs`

``` purescript
visibleXs :: ClassName
```


#### `visibleXsBlock`

``` purescript
visibleXsBlock :: ClassName
```


#### `visibleXsInline`

``` purescript
visibleXsInline :: ClassName
```


#### `visibleXsInlineBlock`

``` purescript
visibleXsInlineBlock :: ClassName
```


#### `warning`

``` purescript
warning :: ClassName
```


#### `well`

``` purescript
well :: ClassName
```


#### `wellLg`

``` purescript
wellLg :: ClassName
```


#### `wellSm`

``` purescript
wellSm :: ClassName
```


#### `woff`

``` purescript
woff :: ClassName
```


#### `woff2`

``` purescript
woff2 :: ClassName
```



## Module Halogen.Themes.Foundation5

#### `accordion`

``` purescript
accordion :: ClassName
```


#### `accordionNavigation`

``` purescript
accordionNavigation :: ClassName
```


#### `active`

``` purescript
active :: ClassName
```


#### `alert`

``` purescript
alert :: ClassName
```


#### `alertBox`

``` purescript
alertBox :: ClassName
```


#### `alertClose`

``` purescript
alertClose :: ClassName
```


#### `alt`

``` purescript
alt :: ClassName
```


#### `antialiased`

``` purescript
antialiased :: ClassName
```


#### `back`

``` purescript
back :: ClassName
```


#### `bottom`

``` purescript
bottom :: ClassName
```


#### `breadcrumbs`

``` purescript
breadcrumbs :: ClassName
```


#### `bulletItem`

``` purescript
bulletItem :: ClassName
```


#### `button`

``` purescript
button :: ClassName
```


#### `buttonBar`

``` purescript
buttonBar :: ClassName
```


#### `buttonGroup`

``` purescript
buttonGroup :: ClassName
```


#### `callout`

``` purescript
callout :: ClassName
```


#### `carousel`

``` purescript
carousel :: ClassName
```


#### `circle`

``` purescript
circle :: ClassName
```


#### `clearfix`

``` purescript
clearfix :: ClassName
```


#### `clearingAssembled`

``` purescript
clearingAssembled :: ClassName
```


#### `clearingBlackout`

``` purescript
clearingBlackout :: ClassName
```


#### `clearingCaption`

``` purescript
clearingCaption :: ClassName
```


#### `clearingClose`

``` purescript
clearingClose :: ClassName
```


#### `clearingContainer`

``` purescript
clearingContainer :: ClassName
```


#### `clearingFeature`

``` purescript
clearingFeature :: ClassName
```


#### `clearingFeaturedImg`

``` purescript
clearingFeaturedImg :: ClassName
```


#### `clearingMainNext`

``` purescript
clearingMainNext :: ClassName
```


#### `clearingMainPrev`

``` purescript
clearingMainPrev :: ClassName
```


#### `clearingThumbs`

``` purescript
clearingThumbs :: ClassName
```


#### `clearingTouchLabel`

``` purescript
clearingTouchLabel :: ClassName
```


#### `close`

``` purescript
close :: ClassName
```


#### `closeRevealModal`

``` purescript
closeRevealModal :: ClassName
```


#### `collapse`

``` purescript
collapse :: ClassName
```


#### `column`

``` purescript
column :: ClassName
```


#### `columns`

``` purescript
columns :: ClassName
```


#### `containToGrid`

``` purescript
containToGrid :: ClassName
```


#### `contained`

``` purescript
contained :: ClassName
```


#### `content`

``` purescript
content :: ClassName
```


#### `ctaButton`

``` purescript
ctaButton :: ClassName
```


#### `current`

``` purescript
current :: ClassName
```


#### `dark`

``` purescript
dark :: ClassName
```


#### `description`

``` purescript
description :: ClassName
```


#### `disabled`

``` purescript
disabled :: ClassName
```


#### `disc`

``` purescript
disc :: ClassName
```


#### `divider`

``` purescript
divider :: ClassName
```


#### `dropLeft`

``` purescript
dropLeft :: ClassName
```


#### `dropRight`

``` purescript
dropRight :: ClassName
```


#### `dropTop`

``` purescript
dropTop :: ClassName
```


#### `dropdown`

``` purescript
dropdown :: ClassName
```


#### `eightUp`

``` purescript
eightUp :: ClassName
```


#### `end`

``` purescript
end :: ClassName
```


#### `error`

``` purescript
error :: ClassName
```


#### `errorMessage`

``` purescript
errorMessage :: ClassName
```


#### `even`

``` purescript
even :: ClassName
```


#### `even2`

``` purescript
even2 :: ClassName
```


#### `even3`

``` purescript
even3 :: ClassName
```


#### `even4`

``` purescript
even4 :: ClassName
```


#### `even5`

``` purescript
even5 :: ClassName
```


#### `even6`

``` purescript
even6 :: ClassName
```


#### `even7`

``` purescript
even7 :: ClassName
```


#### `even8`

``` purescript
even8 :: ClassName
```


#### `exitOffCanvas`

``` purescript
exitOffCanvas :: ClassName
```


#### `expand`

``` purescript
expand :: ClassName
```


#### `expanded`

``` purescript
expanded :: ClassName
```


#### `fDropdown`

``` purescript
fDropdown :: ClassName
```


#### `fiveUp`

``` purescript
fiveUp :: ClassName
```


#### `fixHeight`

``` purescript
fixHeight :: ClassName
```


#### `fixed`

``` purescript
fixed :: ClassName
```


#### `flexVideo`

``` purescript
flexVideo :: ClassName
```


#### `fn`

``` purescript
fn :: ClassName
```


#### `foundationDataAttributeNamespace`

``` purescript
foundationDataAttributeNamespace :: ClassName
```


#### `foundationMqLarge`

``` purescript
foundationMqLarge :: ClassName
```


#### `foundationMqLargeOnly`

``` purescript
foundationMqLargeOnly :: ClassName
```


#### `foundationMqMedium`

``` purescript
foundationMqMedium :: ClassName
```


#### `foundationMqMediumOnly`

``` purescript
foundationMqMediumOnly :: ClassName
```


#### `foundationMqSmall`

``` purescript
foundationMqSmall :: ClassName
```


#### `foundationMqSmallOnly`

``` purescript
foundationMqSmallOnly :: ClassName
```


#### `foundationMqTopbar`

``` purescript
foundationMqTopbar :: ClassName
```


#### `foundationMqXlarge`

``` purescript
foundationMqXlarge :: ClassName
```


#### `foundationMqXlargeOnly`

``` purescript
foundationMqXlargeOnly :: ClassName
```


#### `foundationMqXxlarge`

``` purescript
foundationMqXxlarge :: ClassName
```


#### `foundationVersion`

``` purescript
foundationVersion :: ClassName
```


#### `fourUp`

``` purescript
fourUp :: ClassName
```


#### `full`

``` purescript
full :: ClassName
```


#### `hasDropdown`

``` purescript
hasDropdown :: ClassName
```


#### `hasForm`

``` purescript
hasForm :: ClassName
```


#### `hasSubmenu`

``` purescript
hasSubmenu :: ClassName
```


#### `hasTip`

``` purescript
hasTip :: ClassName
```


#### `heading`

``` purescript
heading :: ClassName
```


#### `hiddenForLarge`

``` purescript
hiddenForLarge :: ClassName
```


#### `hiddenForLargeDown`

``` purescript
hiddenForLargeDown :: ClassName
```


#### `hiddenForLargeOnly`

``` purescript
hiddenForLargeOnly :: ClassName
```


#### `hiddenForLargeUp`

``` purescript
hiddenForLargeUp :: ClassName
```


#### `hiddenForMedium`

``` purescript
hiddenForMedium :: ClassName
```


#### `hiddenForMediumDown`

``` purescript
hiddenForMediumDown :: ClassName
```


#### `hiddenForMediumOnly`

``` purescript
hiddenForMediumOnly :: ClassName
```


#### `hiddenForMediumUp`

``` purescript
hiddenForMediumUp :: ClassName
```


#### `hiddenForSmall`

``` purescript
hiddenForSmall :: ClassName
```


#### `hiddenForSmallDown`

``` purescript
hiddenForSmallDown :: ClassName
```


#### `hiddenForSmallOnly`

``` purescript
hiddenForSmallOnly :: ClassName
```


#### `hiddenForSmallUp`

``` purescript
hiddenForSmallUp :: ClassName
```


#### `hiddenForXlarge`

``` purescript
hiddenForXlarge :: ClassName
```


#### `hiddenForXlargeDown`

``` purescript
hiddenForXlargeDown :: ClassName
```


#### `hiddenForXlargeOnly`

``` purescript
hiddenForXlargeOnly :: ClassName
```


#### `hiddenForXlargeUp`

``` purescript
hiddenForXlargeUp :: ClassName
```


#### `hiddenForXxlarge`

``` purescript
hiddenForXxlarge :: ClassName
```


#### `hiddenForXxlargeDown`

``` purescript
hiddenForXxlargeDown :: ClassName
```


#### `hiddenForXxlargeOnly`

``` purescript
hiddenForXxlargeOnly :: ClassName
```


#### `hiddenForXxlargeUp`

``` purescript
hiddenForXxlargeUp :: ClassName
```


#### `hide`

``` purescript
hide :: ClassName
```


#### `hideForLandscape`

``` purescript
hideForLandscape :: ClassName
```


#### `hideForLarge`

``` purescript
hideForLarge :: ClassName
```


#### `hideForLargeDown`

``` purescript
hideForLargeDown :: ClassName
```


#### `hideForLargeOnly`

``` purescript
hideForLargeOnly :: ClassName
```


#### `hideForLargeUp`

``` purescript
hideForLargeUp :: ClassName
```


#### `hideForMedium`

``` purescript
hideForMedium :: ClassName
```


#### `hideForMediumDown`

``` purescript
hideForMediumDown :: ClassName
```


#### `hideForMediumOnly`

``` purescript
hideForMediumOnly :: ClassName
```


#### `hideForMediumUp`

``` purescript
hideForMediumUp :: ClassName
```


#### `hideForPortrait`

``` purescript
hideForPortrait :: ClassName
```


#### `hideForPrint`

``` purescript
hideForPrint :: ClassName
```


#### `hideForSmall`

``` purescript
hideForSmall :: ClassName
```


#### `hideForSmallDown`

``` purescript
hideForSmallDown :: ClassName
```


#### `hideForSmallOnly`

``` purescript
hideForSmallOnly :: ClassName
```


#### `hideForSmallUp`

``` purescript
hideForSmallUp :: ClassName
```


#### `hideForTouch`

``` purescript
hideForTouch :: ClassName
```


#### `hideForXlarge`

``` purescript
hideForXlarge :: ClassName
```


#### `hideForXlargeDown`

``` purescript
hideForXlargeDown :: ClassName
```


#### `hideForXlargeOnly`

``` purescript
hideForXlargeOnly :: ClassName
```


#### `hideForXlargeUp`

``` purescript
hideForXlargeUp :: ClassName
```


#### `hideForXxlarge`

``` purescript
hideForXxlarge :: ClassName
```


#### `hideForXxlargeDown`

``` purescript
hideForXxlargeDown :: ClassName
```


#### `hideForXxlargeOnly`

``` purescript
hideForXxlargeOnly :: ClassName
```


#### `hideForXxlargeUp`

``` purescript
hideForXxlargeUp :: ClassName
```


#### `hideOnPrint`

``` purescript
hideOnPrint :: ClassName
```


#### `hover`

``` purescript
hover :: ClassName
```


#### `iconBar`

``` purescript
iconBar :: ClassName
```


#### `info`

``` purescript
info :: ClassName
```


#### `inline`

``` purescript
inline :: ClassName
```


#### `inlineList`

``` purescript
inlineList :: ClassName
```


#### `innerWrap`

``` purescript
innerWrap :: ClassName
```


#### `invisible`

``` purescript
invisible :: ClassName
```


#### `ir`

``` purescript
ir :: ClassName
```


#### `item`

``` purescript
item :: ClassName
```


#### `joyrideCloseTip`

``` purescript
joyrideCloseTip :: ClassName
```


#### `joyrideContentWrapper`

``` purescript
joyrideContentWrapper :: ClassName
```


#### `joyrideExposeCover`

``` purescript
joyrideExposeCover :: ClassName
```


#### `joyrideExposeWrapper`

``` purescript
joyrideExposeWrapper :: ClassName
```


#### `joyrideList`

``` purescript
joyrideList :: ClassName
```


#### `joyrideModalBg`

``` purescript
joyrideModalBg :: ClassName
```


#### `joyrideNub`

``` purescript
joyrideNub :: ClassName
```


#### `joyridePrevTip`

``` purescript
joyridePrevTip :: ClassName
```


#### `joyrideTimerIndicator`

``` purescript
joyrideTimerIndicator :: ClassName
```


#### `joyrideTimerIndicatorWrap`

``` purescript
joyrideTimerIndicatorWrap :: ClassName
```


#### `joyrideTipGuide`

``` purescript
joyrideTipGuide :: ClassName
```


#### `jsGenerated`

``` purescript
jsGenerated :: ClassName
```


#### `keystroke`

``` purescript
keystroke :: ClassName
```


#### `label`

``` purescript
label :: ClassName
```


#### `labelRight`

``` purescript
labelRight :: ClassName
```


#### `large`

``` purescript
large :: ClassName
```


#### `large1`

``` purescript
large1 :: ClassName
```


#### `large10`

``` purescript
large10 :: ClassName
```


#### `large11`

``` purescript
large11 :: ClassName
```


#### `large12`

``` purescript
large12 :: ClassName
```


#### `large2`

``` purescript
large2 :: ClassName
```


#### `large3`

``` purescript
large3 :: ClassName
```


#### `large4`

``` purescript
large4 :: ClassName
```


#### `large5`

``` purescript
large5 :: ClassName
```


#### `large6`

``` purescript
large6 :: ClassName
```


#### `large7`

``` purescript
large7 :: ClassName
```


#### `large8`

``` purescript
large8 :: ClassName
```


#### `large9`

``` purescript
large9 :: ClassName
```


#### `largeBlockGrid1`

``` purescript
largeBlockGrid1 :: ClassName
```


#### `largeBlockGrid10`

``` purescript
largeBlockGrid10 :: ClassName
```


#### `largeBlockGrid11`

``` purescript
largeBlockGrid11 :: ClassName
```


#### `largeBlockGrid12`

``` purescript
largeBlockGrid12 :: ClassName
```


#### `largeBlockGrid2`

``` purescript
largeBlockGrid2 :: ClassName
```


#### `largeBlockGrid3`

``` purescript
largeBlockGrid3 :: ClassName
```


#### `largeBlockGrid4`

``` purescript
largeBlockGrid4 :: ClassName
```


#### `largeBlockGrid5`

``` purescript
largeBlockGrid5 :: ClassName
```


#### `largeBlockGrid6`

``` purescript
largeBlockGrid6 :: ClassName
```


#### `largeBlockGrid7`

``` purescript
largeBlockGrid7 :: ClassName
```


#### `largeBlockGrid8`

``` purescript
largeBlockGrid8 :: ClassName
```


#### `largeBlockGrid9`

``` purescript
largeBlockGrid9 :: ClassName
```


#### `largeCentered`

``` purescript
largeCentered :: ClassName
```


#### `largeCollapse`

``` purescript
largeCollapse :: ClassName
```


#### `largeOffset0`

``` purescript
largeOffset0 :: ClassName
```


#### `largeOffset1`

``` purescript
largeOffset1 :: ClassName
```


#### `largeOffset10`

``` purescript
largeOffset10 :: ClassName
```


#### `largeOffset11`

``` purescript
largeOffset11 :: ClassName
```


#### `largeOffset2`

``` purescript
largeOffset2 :: ClassName
```


#### `largeOffset3`

``` purescript
largeOffset3 :: ClassName
```


#### `largeOffset4`

``` purescript
largeOffset4 :: ClassName
```


#### `largeOffset5`

``` purescript
largeOffset5 :: ClassName
```


#### `largeOffset6`

``` purescript
largeOffset6 :: ClassName
```


#### `largeOffset7`

``` purescript
largeOffset7 :: ClassName
```


#### `largeOffset8`

``` purescript
largeOffset8 :: ClassName
```


#### `largeOffset9`

``` purescript
largeOffset9 :: ClassName
```


#### `largeOnlyTextCenter`

``` purescript
largeOnlyTextCenter :: ClassName
```


#### `largeOnlyTextJustify`

``` purescript
largeOnlyTextJustify :: ClassName
```


#### `largeOnlyTextLeft`

``` purescript
largeOnlyTextLeft :: ClassName
```


#### `largeOnlyTextRight`

``` purescript
largeOnlyTextRight :: ClassName
```


#### `largePull0`

``` purescript
largePull0 :: ClassName
```


#### `largePull1`

``` purescript
largePull1 :: ClassName
```


#### `largePull10`

``` purescript
largePull10 :: ClassName
```


#### `largePull11`

``` purescript
largePull11 :: ClassName
```


#### `largePull2`

``` purescript
largePull2 :: ClassName
```


#### `largePull3`

``` purescript
largePull3 :: ClassName
```


#### `largePull4`

``` purescript
largePull4 :: ClassName
```


#### `largePull5`

``` purescript
largePull5 :: ClassName
```


#### `largePull6`

``` purescript
largePull6 :: ClassName
```


#### `largePull7`

``` purescript
largePull7 :: ClassName
```


#### `largePull8`

``` purescript
largePull8 :: ClassName
```


#### `largePull9`

``` purescript
largePull9 :: ClassName
```


#### `largePush0`

``` purescript
largePush0 :: ClassName
```


#### `largePush1`

``` purescript
largePush1 :: ClassName
```


#### `largePush10`

``` purescript
largePush10 :: ClassName
```


#### `largePush11`

``` purescript
largePush11 :: ClassName
```


#### `largePush2`

``` purescript
largePush2 :: ClassName
```


#### `largePush3`

``` purescript
largePush3 :: ClassName
```


#### `largePush4`

``` purescript
largePush4 :: ClassName
```


#### `largePush5`

``` purescript
largePush5 :: ClassName
```


#### `largePush6`

``` purescript
largePush6 :: ClassName
```


#### `largePush7`

``` purescript
largePush7 :: ClassName
```


#### `largePush8`

``` purescript
largePush8 :: ClassName
```


#### `largePush9`

``` purescript
largePush9 :: ClassName
```


#### `largeResetOrder`

``` purescript
largeResetOrder :: ClassName
```


#### `largeTextCenter`

``` purescript
largeTextCenter :: ClassName
```


#### `largeTextJustify`

``` purescript
largeTextJustify :: ClassName
```


#### `largeTextLeft`

``` purescript
largeTextLeft :: ClassName
```


#### `largeTextRight`

``` purescript
largeTextRight :: ClassName
```


#### `largeUncentered`

``` purescript
largeUncentered :: ClassName
```


#### `largeUncollapse`

``` purescript
largeUncollapse :: ClassName
```


#### `largeVertical`

``` purescript
largeVertical :: ClassName
```


#### `lead`

``` purescript
lead :: ClassName
```


#### `left`

``` purescript
left :: ClassName
```


#### `leftAlign`

``` purescript
leftAlign :: ClassName
```


#### `leftOffCanvasMenu`

``` purescript
leftOffCanvasMenu :: ClassName
```


#### `leftSmall`

``` purescript
leftSmall :: ClassName
```


#### `leftSubmenu`

``` purescript
leftSubmenu :: ClassName
```


#### `ltIe9`

``` purescript
ltIe9 :: ClassName
```


#### `mapCanvas`

``` purescript
mapCanvas :: ClassName
```


#### `medium`

``` purescript
medium :: ClassName
```


#### `medium1`

``` purescript
medium1 :: ClassName
```


#### `medium10`

``` purescript
medium10 :: ClassName
```


#### `medium11`

``` purescript
medium11 :: ClassName
```


#### `medium12`

``` purescript
medium12 :: ClassName
```


#### `medium2`

``` purescript
medium2 :: ClassName
```


#### `medium3`

``` purescript
medium3 :: ClassName
```


#### `medium4`

``` purescript
medium4 :: ClassName
```


#### `medium5`

``` purescript
medium5 :: ClassName
```


#### `medium6`

``` purescript
medium6 :: ClassName
```


#### `medium7`

``` purescript
medium7 :: ClassName
```


#### `medium8`

``` purescript
medium8 :: ClassName
```


#### `medium9`

``` purescript
medium9 :: ClassName
```


#### `mediumBlockGrid1`

``` purescript
mediumBlockGrid1 :: ClassName
```


#### `mediumBlockGrid10`

``` purescript
mediumBlockGrid10 :: ClassName
```


#### `mediumBlockGrid11`

``` purescript
mediumBlockGrid11 :: ClassName
```


#### `mediumBlockGrid12`

``` purescript
mediumBlockGrid12 :: ClassName
```


#### `mediumBlockGrid2`

``` purescript
mediumBlockGrid2 :: ClassName
```


#### `mediumBlockGrid3`

``` purescript
mediumBlockGrid3 :: ClassName
```


#### `mediumBlockGrid4`

``` purescript
mediumBlockGrid4 :: ClassName
```


#### `mediumBlockGrid5`

``` purescript
mediumBlockGrid5 :: ClassName
```


#### `mediumBlockGrid6`

``` purescript
mediumBlockGrid6 :: ClassName
```


#### `mediumBlockGrid7`

``` purescript
mediumBlockGrid7 :: ClassName
```


#### `mediumBlockGrid8`

``` purescript
mediumBlockGrid8 :: ClassName
```


#### `mediumBlockGrid9`

``` purescript
mediumBlockGrid9 :: ClassName
```


#### `mediumCentered`

``` purescript
mediumCentered :: ClassName
```


#### `mediumCollapse`

``` purescript
mediumCollapse :: ClassName
```


#### `mediumOffset0`

``` purescript
mediumOffset0 :: ClassName
```


#### `mediumOffset1`

``` purescript
mediumOffset1 :: ClassName
```


#### `mediumOffset10`

``` purescript
mediumOffset10 :: ClassName
```


#### `mediumOffset11`

``` purescript
mediumOffset11 :: ClassName
```


#### `mediumOffset2`

``` purescript
mediumOffset2 :: ClassName
```


#### `mediumOffset3`

``` purescript
mediumOffset3 :: ClassName
```


#### `mediumOffset4`

``` purescript
mediumOffset4 :: ClassName
```


#### `mediumOffset5`

``` purescript
mediumOffset5 :: ClassName
```


#### `mediumOffset6`

``` purescript
mediumOffset6 :: ClassName
```


#### `mediumOffset7`

``` purescript
mediumOffset7 :: ClassName
```


#### `mediumOffset8`

``` purescript
mediumOffset8 :: ClassName
```


#### `mediumOffset9`

``` purescript
mediumOffset9 :: ClassName
```


#### `mediumOnlyTextCenter`

``` purescript
mediumOnlyTextCenter :: ClassName
```


#### `mediumOnlyTextJustify`

``` purescript
mediumOnlyTextJustify :: ClassName
```


#### `mediumOnlyTextLeft`

``` purescript
mediumOnlyTextLeft :: ClassName
```


#### `mediumOnlyTextRight`

``` purescript
mediumOnlyTextRight :: ClassName
```


#### `mediumPull0`

``` purescript
mediumPull0 :: ClassName
```


#### `mediumPull1`

``` purescript
mediumPull1 :: ClassName
```


#### `mediumPull10`

``` purescript
mediumPull10 :: ClassName
```


#### `mediumPull11`

``` purescript
mediumPull11 :: ClassName
```


#### `mediumPull2`

``` purescript
mediumPull2 :: ClassName
```


#### `mediumPull3`

``` purescript
mediumPull3 :: ClassName
```


#### `mediumPull4`

``` purescript
mediumPull4 :: ClassName
```


#### `mediumPull5`

``` purescript
mediumPull5 :: ClassName
```


#### `mediumPull6`

``` purescript
mediumPull6 :: ClassName
```


#### `mediumPull7`

``` purescript
mediumPull7 :: ClassName
```


#### `mediumPull8`

``` purescript
mediumPull8 :: ClassName
```


#### `mediumPull9`

``` purescript
mediumPull9 :: ClassName
```


#### `mediumPush0`

``` purescript
mediumPush0 :: ClassName
```


#### `mediumPush1`

``` purescript
mediumPush1 :: ClassName
```


#### `mediumPush10`

``` purescript
mediumPush10 :: ClassName
```


#### `mediumPush11`

``` purescript
mediumPush11 :: ClassName
```


#### `mediumPush2`

``` purescript
mediumPush2 :: ClassName
```


#### `mediumPush3`

``` purescript
mediumPush3 :: ClassName
```


#### `mediumPush4`

``` purescript
mediumPush4 :: ClassName
```


#### `mediumPush5`

``` purescript
mediumPush5 :: ClassName
```


#### `mediumPush6`

``` purescript
mediumPush6 :: ClassName
```


#### `mediumPush7`

``` purescript
mediumPush7 :: ClassName
```


#### `mediumPush8`

``` purescript
mediumPush8 :: ClassName
```


#### `mediumPush9`

``` purescript
mediumPush9 :: ClassName
```


#### `mediumResetOrder`

``` purescript
mediumResetOrder :: ClassName
```


#### `mediumTextCenter`

``` purescript
mediumTextCenter :: ClassName
```


#### `mediumTextJustify`

``` purescript
mediumTextJustify :: ClassName
```


#### `mediumTextLeft`

``` purescript
mediumTextLeft :: ClassName
```


#### `mediumTextRight`

``` purescript
mediumTextRight :: ClassName
```


#### `mediumUncentered`

``` purescript
mediumUncentered :: ClassName
```


#### `mediumUncollapse`

``` purescript
mediumUncollapse :: ClassName
```


#### `mediumVertical`

``` purescript
mediumVertical :: ClassName
```


#### `mega`

``` purescript
mega :: ClassName
```


#### `menuIcon`

``` purescript
menuIcon :: ClassName
```


#### `meter`

``` purescript
meter :: ClassName
```


#### `middle`

``` purescript
middle :: ClassName
```


#### `moveLeft`

``` purescript
moveLeft :: ClassName
```


#### `moveRight`

``` purescript
moveRight :: ClassName
```


#### `moved`

``` purescript
moved :: ClassName
```


#### `name`

``` purescript
name :: ClassName
```


#### `noBullet`

``` purescript
noBullet :: ClassName
```


#### `noCsstransforms`

``` purescript
noCsstransforms :: ClassName
```


#### `noJs`

``` purescript
noJs :: ClassName
```


#### `noPip`

``` purescript
noPip :: ClassName
```


#### `notClick`

``` purescript
notClick :: ClassName
```


#### `nub`

``` purescript
nub :: ClassName
```


#### `offCanvasList`

``` purescript
offCanvasList :: ClassName
```


#### `offCanvasWrap`

``` purescript
offCanvasWrap :: ClassName
```


#### `offcanvasOverlap`

``` purescript
offcanvasOverlap :: ClassName
```


#### `offcanvasOverlapLeft`

``` purescript
offcanvasOverlapLeft :: ClassName
```


#### `offcanvasOverlapRight`

``` purescript
offcanvasOverlapRight :: ClassName
```


#### `open`

``` purescript
open :: ClassName
```


#### `opened`

``` purescript
opened :: ClassName
```


#### `opposite`

``` purescript
opposite :: ClassName
```


#### `orbitBullets`

``` purescript
orbitBullets :: ClassName
```


#### `orbitBulletsContainer`

``` purescript
orbitBulletsContainer :: ClassName
```


#### `orbitCaption`

``` purescript
orbitCaption :: ClassName
```


#### `orbitContainer`

``` purescript
orbitContainer :: ClassName
```


#### `orbitNext`

``` purescript
orbitNext :: ClassName
```


#### `orbitPrev`

``` purescript
orbitPrev :: ClassName
```


#### `orbitProgress`

``` purescript
orbitProgress :: ClassName
```


#### `orbitSlideNumber`

``` purescript
orbitSlideNumber :: ClassName
```


#### `orbitSlidesContainer`

``` purescript
orbitSlidesContainer :: ClassName
```


#### `orbitStackOnSmall`

``` purescript
orbitStackOnSmall :: ClassName
```


#### `orbitTimer`

``` purescript
orbitTimer :: ClassName
```


#### `pagination`

``` purescript
pagination :: ClassName
```


#### `paginationCentered`

``` purescript
paginationCentered :: ClassName
```


#### `panel`

``` purescript
panel :: ClassName
```


#### `parentLink`

``` purescript
parentLink :: ClassName
```


#### `paused`

``` purescript
paused :: ClassName
```


#### `postfix`

``` purescript
postfix :: ClassName
```


#### `postfixRadius`

``` purescript
postfixRadius :: ClassName
```


#### `postfixRound`

``` purescript
postfixRound :: ClassName
```


#### `prefix`

``` purescript
prefix :: ClassName
```


#### `prefixRadius`

``` purescript
prefixRadius :: ClassName
```


#### `prefixRound`

``` purescript
prefixRound :: ClassName
```


#### `preloader`

``` purescript
preloader :: ClassName
```


#### `price`

``` purescript
price :: ClassName
```


#### `pricingTable`

``` purescript
pricingTable :: ClassName
```


#### `printOnly`

``` purescript
printOnly :: ClassName
```


#### `progress`

``` purescript
progress :: ClassName
```


#### `pull0`

``` purescript
pull0 :: ClassName
```


#### `pull1`

``` purescript
pull1 :: ClassName
```


#### `pull10`

``` purescript
pull10 :: ClassName
```


#### `pull11`

``` purescript
pull11 :: ClassName
```


#### `pull2`

``` purescript
pull2 :: ClassName
```


#### `pull3`

``` purescript
pull3 :: ClassName
```


#### `pull4`

``` purescript
pull4 :: ClassName
```


#### `pull5`

``` purescript
pull5 :: ClassName
```


#### `pull6`

``` purescript
pull6 :: ClassName
```


#### `pull7`

``` purescript
pull7 :: ClassName
```


#### `pull8`

``` purescript
pull8 :: ClassName
```


#### `pull9`

``` purescript
pull9 :: ClassName
```


#### `push0`

``` purescript
push0 :: ClassName
```


#### `push1`

``` purescript
push1 :: ClassName
```


#### `push10`

``` purescript
push10 :: ClassName
```


#### `push11`

``` purescript
push11 :: ClassName
```


#### `push2`

``` purescript
push2 :: ClassName
```


#### `push3`

``` purescript
push3 :: ClassName
```


#### `push4`

``` purescript
push4 :: ClassName
```


#### `push5`

``` purescript
push5 :: ClassName
```


#### `push6`

``` purescript
push6 :: ClassName
```


#### `push7`

``` purescript
push7 :: ClassName
```


#### `push8`

``` purescript
push8 :: ClassName
```


#### `push9`

``` purescript
push9 :: ClassName
```


#### `radius`

``` purescript
radius :: ClassName
```


#### `rangeSlider`

``` purescript
rangeSlider :: ClassName
```


#### `rangeSliderActiveSegment`

``` purescript
rangeSliderActiveSegment :: ClassName
```


#### `rangeSliderHandle`

``` purescript
rangeSliderHandle :: ClassName
```


#### `revealModal`

``` purescript
revealModal :: ClassName
```


#### `revealModalBg`

``` purescript
revealModalBg :: ClassName
```


#### `right`

``` purescript
right :: ClassName
```


#### `rightAlign`

``` purescript
rightAlign :: ClassName
```


#### `rightOffCanvasMenu`

``` purescript
rightOffCanvasMenu :: ClassName
```


#### `rightSmall`

``` purescript
rightSmall :: ClassName
```


#### `rightSubmenu`

``` purescript
rightSubmenu :: ClassName
```


#### `round`

``` purescript
round :: ClassName
```


#### `row`

``` purescript
row :: ClassName
```


#### `rtl`

``` purescript
rtl :: ClassName
```


#### `secondary`

``` purescript
secondary :: ClassName
```


#### `sevenUp`

``` purescript
sevenUp :: ClassName
```


#### `showForLandscape`

``` purescript
showForLandscape :: ClassName
```


#### `showForLarge`

``` purescript
showForLarge :: ClassName
```


#### `showForLargeDown`

``` purescript
showForLargeDown :: ClassName
```


#### `showForLargeOnly`

``` purescript
showForLargeOnly :: ClassName
```


#### `showForLargeUp`

``` purescript
showForLargeUp :: ClassName
```


#### `showForMedium`

``` purescript
showForMedium :: ClassName
```


#### `showForMediumDown`

``` purescript
showForMediumDown :: ClassName
```


#### `showForMediumOnly`

``` purescript
showForMediumOnly :: ClassName
```


#### `showForMediumUp`

``` purescript
showForMediumUp :: ClassName
```


#### `showForPortrait`

``` purescript
showForPortrait :: ClassName
```


#### `showForPrint`

``` purescript
showForPrint :: ClassName
```


#### `showForSmall`

``` purescript
showForSmall :: ClassName
```


#### `showForSmallDown`

``` purescript
showForSmallDown :: ClassName
```


#### `showForSmallOnly`

``` purescript
showForSmallOnly :: ClassName
```


#### `showForSmallUp`

``` purescript
showForSmallUp :: ClassName
```


#### `showForTouch`

``` purescript
showForTouch :: ClassName
```


#### `showForXlarge`

``` purescript
showForXlarge :: ClassName
```


#### `showForXlargeDown`

``` purescript
showForXlargeDown :: ClassName
```


#### `showForXlargeOnly`

``` purescript
showForXlargeOnly :: ClassName
```


#### `showForXlargeUp`

``` purescript
showForXlargeUp :: ClassName
```


#### `showForXxlarge`

``` purescript
showForXxlarge :: ClassName
```


#### `showForXxlargeDown`

``` purescript
showForXxlargeDown :: ClassName
```


#### `showForXxlargeOnly`

``` purescript
showForXxlargeOnly :: ClassName
```


#### `showForXxlargeUp`

``` purescript
showForXxlargeUp :: ClassName
```


#### `sideNav`

``` purescript
sideNav :: ClassName
```


#### `sixUp`

``` purescript
sixUp :: ClassName
```


#### `slideshowWrapper`

``` purescript
slideshowWrapper :: ClassName
```


#### `small`

``` purescript
small :: ClassName
```


#### `small1`

``` purescript
small1 :: ClassName
```


#### `small10`

``` purescript
small10 :: ClassName
```


#### `small11`

``` purescript
small11 :: ClassName
```


#### `small12`

``` purescript
small12 :: ClassName
```


#### `small2`

``` purescript
small2 :: ClassName
```


#### `small3`

``` purescript
small3 :: ClassName
```


#### `small4`

``` purescript
small4 :: ClassName
```


#### `small5`

``` purescript
small5 :: ClassName
```


#### `small6`

``` purescript
small6 :: ClassName
```


#### `small7`

``` purescript
small7 :: ClassName
```


#### `small8`

``` purescript
small8 :: ClassName
```


#### `small9`

``` purescript
small9 :: ClassName
```


#### `smallBlockGrid1`

``` purescript
smallBlockGrid1 :: ClassName
```


#### `smallBlockGrid10`

``` purescript
smallBlockGrid10 :: ClassName
```


#### `smallBlockGrid11`

``` purescript
smallBlockGrid11 :: ClassName
```


#### `smallBlockGrid12`

``` purescript
smallBlockGrid12 :: ClassName
```


#### `smallBlockGrid2`

``` purescript
smallBlockGrid2 :: ClassName
```


#### `smallBlockGrid3`

``` purescript
smallBlockGrid3 :: ClassName
```


#### `smallBlockGrid4`

``` purescript
smallBlockGrid4 :: ClassName
```


#### `smallBlockGrid5`

``` purescript
smallBlockGrid5 :: ClassName
```


#### `smallBlockGrid6`

``` purescript
smallBlockGrid6 :: ClassName
```


#### `smallBlockGrid7`

``` purescript
smallBlockGrid7 :: ClassName
```


#### `smallBlockGrid8`

``` purescript
smallBlockGrid8 :: ClassName
```


#### `smallBlockGrid9`

``` purescript
smallBlockGrid9 :: ClassName
```


#### `smallCentered`

``` purescript
smallCentered :: ClassName
```


#### `smallCollapse`

``` purescript
smallCollapse :: ClassName
```


#### `smallOffset0`

``` purescript
smallOffset0 :: ClassName
```


#### `smallOffset1`

``` purescript
smallOffset1 :: ClassName
```


#### `smallOffset10`

``` purescript
smallOffset10 :: ClassName
```


#### `smallOffset11`

``` purescript
smallOffset11 :: ClassName
```


#### `smallOffset2`

``` purescript
smallOffset2 :: ClassName
```


#### `smallOffset3`

``` purescript
smallOffset3 :: ClassName
```


#### `smallOffset4`

``` purescript
smallOffset4 :: ClassName
```


#### `smallOffset5`

``` purescript
smallOffset5 :: ClassName
```


#### `smallOffset6`

``` purescript
smallOffset6 :: ClassName
```


#### `smallOffset7`

``` purescript
smallOffset7 :: ClassName
```


#### `smallOffset8`

``` purescript
smallOffset8 :: ClassName
```


#### `smallOffset9`

``` purescript
smallOffset9 :: ClassName
```


#### `smallOnlyTextCenter`

``` purescript
smallOnlyTextCenter :: ClassName
```


#### `smallOnlyTextJustify`

``` purescript
smallOnlyTextJustify :: ClassName
```


#### `smallOnlyTextLeft`

``` purescript
smallOnlyTextLeft :: ClassName
```


#### `smallOnlyTextRight`

``` purescript
smallOnlyTextRight :: ClassName
```


#### `smallPull0`

``` purescript
smallPull0 :: ClassName
```


#### `smallPull1`

``` purescript
smallPull1 :: ClassName
```


#### `smallPull10`

``` purescript
smallPull10 :: ClassName
```


#### `smallPull11`

``` purescript
smallPull11 :: ClassName
```


#### `smallPull2`

``` purescript
smallPull2 :: ClassName
```


#### `smallPull3`

``` purescript
smallPull3 :: ClassName
```


#### `smallPull4`

``` purescript
smallPull4 :: ClassName
```


#### `smallPull5`

``` purescript
smallPull5 :: ClassName
```


#### `smallPull6`

``` purescript
smallPull6 :: ClassName
```


#### `smallPull7`

``` purescript
smallPull7 :: ClassName
```


#### `smallPull8`

``` purescript
smallPull8 :: ClassName
```


#### `smallPull9`

``` purescript
smallPull9 :: ClassName
```


#### `smallPush0`

``` purescript
smallPush0 :: ClassName
```


#### `smallPush1`

``` purescript
smallPush1 :: ClassName
```


#### `smallPush10`

``` purescript
smallPush10 :: ClassName
```


#### `smallPush11`

``` purescript
smallPush11 :: ClassName
```


#### `smallPush2`

``` purescript
smallPush2 :: ClassName
```


#### `smallPush3`

``` purescript
smallPush3 :: ClassName
```


#### `smallPush4`

``` purescript
smallPush4 :: ClassName
```


#### `smallPush5`

``` purescript
smallPush5 :: ClassName
```


#### `smallPush6`

``` purescript
smallPush6 :: ClassName
```


#### `smallPush7`

``` purescript
smallPush7 :: ClassName
```


#### `smallPush8`

``` purescript
smallPush8 :: ClassName
```


#### `smallPush9`

``` purescript
smallPush9 :: ClassName
```


#### `smallResetOrder`

``` purescript
smallResetOrder :: ClassName
```


#### `smallTextCenter`

``` purescript
smallTextCenter :: ClassName
```


#### `smallTextJustify`

``` purescript
smallTextJustify :: ClassName
```


#### `smallTextLeft`

``` purescript
smallTextLeft :: ClassName
```


#### `smallTextRight`

``` purescript
smallTextRight :: ClassName
```


#### `smallUncentered`

``` purescript
smallUncentered :: ClassName
```


#### `smallUncollapse`

``` purescript
smallUncollapse :: ClassName
```


#### `smallVertical`

``` purescript
smallVertical :: ClassName
```


#### `split`

``` purescript
split :: ClassName
```


#### `square`

``` purescript
square :: ClassName
```


#### `stack`

``` purescript
stack :: ClassName
```


#### `stackForSmall`

``` purescript
stackForSmall :: ClassName
```


#### `subNav`

``` purescript
subNav :: ClassName
```


#### `subheader`

``` purescript
subheader :: ClassName
```


#### `success`

``` purescript
success :: ClassName
```


#### `summary`

``` purescript
summary :: ClassName
```


#### `switch`

``` purescript
switch :: ClassName
```


#### `tab`

``` purescript
tab :: ClassName
```


#### `tabBar`

``` purescript
tabBar :: ClassName
```


#### `tabBarSection`

``` purescript
tabBarSection :: ClassName
```


#### `tabTitle`

``` purescript
tabTitle :: ClassName
```


#### `tabs`

``` purescript
tabs :: ClassName
```


#### `tabsContent`

``` purescript
tabsContent :: ClassName
```


#### `tapToClose`

``` purescript
tapToClose :: ClassName
```


#### `textCenter`

``` purescript
textCenter :: ClassName
```


#### `textJustify`

``` purescript
textJustify :: ClassName
```


#### `textLeft`

``` purescript
textLeft :: ClassName
```


#### `textRight`

``` purescript
textRight :: ClassName
```


#### `th`

``` purescript
th :: ClassName
```


#### `threeUp`

``` purescript
threeUp :: ClassName
```


#### `tiny`

``` purescript
tiny :: ClassName
```


#### `tipLeft`

``` purescript
tipLeft :: ClassName
```


#### `tipRight`

``` purescript
tipRight :: ClassName
```


#### `tipTop`

``` purescript
tipTop :: ClassName
```


#### `title`

``` purescript
title :: ClassName
```


#### `titleArea`

``` purescript
titleArea :: ClassName
```


#### `toback`

``` purescript
toback :: ClassName
```


#### `toggleTopbar`

``` purescript
toggleTopbar :: ClassName
```


#### `tooltip`

``` purescript
tooltip :: ClassName
```


#### `top`

``` purescript
top :: ClassName
```


#### `topBar`

``` purescript
topBar :: ClassName
```


#### `topBarSection`

``` purescript
topBarSection :: ClassName
```


#### `touch`

``` purescript
touch :: ClassName
```


#### `twoUp`

``` purescript
twoUp :: ClassName
```


#### `unavailable`

``` purescript
unavailable :: ClassName
```


#### `vcard`

``` purescript
vcard :: ClassName
```


#### `vertical`

``` purescript
vertical :: ClassName
```


#### `verticalRange`

``` purescript
verticalRange :: ClassName
```


#### `vevent`

``` purescript
vevent :: ClassName
```


#### `vimeo`

``` purescript
vimeo :: ClassName
```


#### `visible`

``` purescript
visible :: ClassName
```


#### `visibleForLarge`

``` purescript
visibleForLarge :: ClassName
```


#### `visibleForLargeDown`

``` purescript
visibleForLargeDown :: ClassName
```


#### `visibleForLargeOnly`

``` purescript
visibleForLargeOnly :: ClassName
```


#### `visibleForLargeUp`

``` purescript
visibleForLargeUp :: ClassName
```


#### `visibleForMedium`

``` purescript
visibleForMedium :: ClassName
```


#### `visibleForMediumDown`

``` purescript
visibleForMediumDown :: ClassName
```


#### `visibleForMediumOnly`

``` purescript
visibleForMediumOnly :: ClassName
```


#### `visibleForMediumUp`

``` purescript
visibleForMediumUp :: ClassName
```


#### `visibleForSmall`

``` purescript
visibleForSmall :: ClassName
```


#### `visibleForSmallDown`

``` purescript
visibleForSmallDown :: ClassName
```


#### `visibleForSmallOnly`

``` purescript
visibleForSmallOnly :: ClassName
```


#### `visibleForSmallUp`

``` purescript
visibleForSmallUp :: ClassName
```


#### `visibleForXlarge`

``` purescript
visibleForXlarge :: ClassName
```


#### `visibleForXlargeDown`

``` purescript
visibleForXlargeDown :: ClassName
```


#### `visibleForXlargeOnly`

``` purescript
visibleForXlargeOnly :: ClassName
```


#### `visibleForXlargeUp`

``` purescript
visibleForXlargeUp :: ClassName
```


#### `visibleForXxlarge`

``` purescript
visibleForXxlarge :: ClassName
```


#### `visibleForXxlargeDown`

``` purescript
visibleForXxlargeDown :: ClassName
```


#### `visibleForXxlargeOnly`

``` purescript
visibleForXxlargeOnly :: ClassName
```


#### `visibleForXxlargeUp`

``` purescript
visibleForXxlargeUp :: ClassName
```


#### `visibleImg`

``` purescript
visibleImg :: ClassName
```


#### `warning`

``` purescript
warning :: ClassName
```


#### `widescreen`

``` purescript
widescreen :: ClassName
```


#### `xlarge`

``` purescript
xlarge :: ClassName
```


#### `xlargeOnlyTextCenter`

``` purescript
xlargeOnlyTextCenter :: ClassName
```


#### `xlargeOnlyTextJustify`

``` purescript
xlargeOnlyTextJustify :: ClassName
```


#### `xlargeOnlyTextLeft`

``` purescript
xlargeOnlyTextLeft :: ClassName
```


#### `xlargeOnlyTextRight`

``` purescript
xlargeOnlyTextRight :: ClassName
```


#### `xlargeTextCenter`

``` purescript
xlargeTextCenter :: ClassName
```


#### `xlargeTextJustify`

``` purescript
xlargeTextJustify :: ClassName
```


#### `xlargeTextLeft`

``` purescript
xlargeTextLeft :: ClassName
```


#### `xlargeTextRight`

``` purescript
xlargeTextRight :: ClassName
```


#### `xxlargeOnlyTextCenter`

``` purescript
xxlargeOnlyTextCenter :: ClassName
```


#### `xxlargeOnlyTextJustify`

``` purescript
xxlargeOnlyTextJustify :: ClassName
```


#### `xxlargeOnlyTextLeft`

``` purescript
xxlargeOnlyTextLeft :: ClassName
```


#### `xxlargeOnlyTextRight`

``` purescript
xxlargeOnlyTextRight :: ClassName
```


#### `xxlargeTextCenter`

``` purescript
xxlargeTextCenter :: ClassName
```


#### `xxlargeTextJustify`

``` purescript
xxlargeTextJustify :: ClassName
```


#### `xxlargeTextLeft`

``` purescript
xxlargeTextLeft :: ClassName
```


#### `xxlargeTextRight`

``` purescript
xxlargeTextRight :: ClassName
```



## Module Halogen.HTML.Events.Forms


Convenience functions for working with form elements.

#### `onValueChanged`

``` purescript
onValueChanged :: forall value i. (IsForeign value) => (value -> EventHandler i) -> H.Attribute i
```

Attach an event handler which will produce an input when the value of an input field changes

An input will not be produced if the value cannot be cast to the appropriate type.

#### `onChecked`

``` purescript
onChecked :: forall i. (Boolean -> EventHandler i) -> H.Attribute i
```

Attach an event handler which will fire when a checkbox is checked or unchecked

#### `onInput`

``` purescript
onInput :: forall value i. (IsForeign value) => (value -> EventHandler i) -> H.Attribute i
```

Attach an event handler which will fire on input


## Module Halogen.HTML.Events.Handler


This module defines the `EventHandler` functor, which can be used
to perform standard operations on HTML events.

#### `EventHandler`

``` purescript
data EventHandler a
```

This applicative functor supports the following operations on events:

- `preventDefault`
- `stopPropagation`
- `stopImmediatePropagation`

It can be used as follows:

```purescript
import Control.Functor (($>))

H.a (E.onclick \_ -> E.preventDefault $> ClickHandler) (H.text "Click here")
```

#### `preventDefault`

``` purescript
preventDefault :: EventHandler Unit
```

Call the `preventDefault` method on the current event

#### `stopPropagation`

``` purescript
stopPropagation :: EventHandler Unit
```

Call the `stopPropagation` method on the current event

#### `stopImmediatePropagation`

``` purescript
stopImmediatePropagation :: EventHandler Unit
```

Call the `stopImmediatePropagation` method on the current event

#### `functorEventHandler`

``` purescript
instance functorEventHandler :: Functor EventHandler
```


#### `applyEventHandler`

``` purescript
instance applyEventHandler :: Apply EventHandler
```


#### `applicativeEventHandler`

``` purescript
instance applicativeEventHandler :: Applicative EventHandler
```


#### `runEventHandler`

``` purescript
runEventHandler :: forall a fields eff. Event fields -> EventHandler a -> Eff (dom :: DOM | eff) a
```

This function can be used to update an event and return the wrapped value


## Module Halogen.HTML.Events.Types


This module defines types for common DOM events

#### `Event`

``` purescript
type Event fields = { "type" :: String, timeStamp :: Number, target :: Node, currentTarget :: Node, cancelable :: Boolean, bubbles :: Boolean | fields }
```

This record synonym captures the properties which appear on every DOM event.

The `fields` type parameter allows us to attach different types of additional
properties to represent more specific types of events.

#### `MouseEvent`

``` purescript
type MouseEvent = (which :: Number, metaKey :: Boolean, altKey :: Boolean, shiftKey :: Boolean, ctrlKey :: Boolean, screenY :: Number, screenX :: Number, clientY :: Number, clientX :: Number, relatedTarget :: Node, detail :: Number, button :: Number)
```

Identifies the additional fields which are available on mouse events.

#### `KeyboardEvent`

``` purescript
type KeyboardEvent = (which :: Number, metaKey :: Boolean, altKey :: Boolean, shiftKey :: Boolean, ctrlKey :: Boolean, keyCode :: Number, charCode :: Number)
```

Identifies the additional fields which are available on keyboard events.

#### `FocusEvent`

``` purescript
type FocusEvent = (relatedTarget :: Node)
```

Identifies the additional fields which are available on focus events.


## Module Halogen.Themes.Bootstrap3.Breadcrumbs


This module provides convenience functions for creating _breadcrumb_ navigation elements.

#### `URL`

``` purescript
data URL
```

A type-safe wrapper for a URL

#### `url`

``` purescript
url :: String -> URL
```

Create a `URL`

#### `runURL`

``` purescript
runURL :: URL -> String
```

Unwrap a URL

#### `Crumb`

``` purescript
data Crumb a
  = LinkCrumb URL
  | DataCrumb a
```

There are two types of crumbs:

- `LinkCrumb` creates crumbs which link to URLs.
- `DataCrumb` contains data which may be used to generate inputs or requests.


#### `CrumbTrail`

``` purescript
data CrumbTrail a
  = CrumbTrail [Tuple String (Crumb a)] String [Tuple String (Crumb a)]
```

A `CrumbTrail` is a zipper with a current location, and crumbs behind and in front of us.

#### `breadcrumbs`

``` purescript
breadcrumbs :: forall a i. CrumbTrail i -> H.HTML a i
```

Create a breadcrumb navigation element from an array of `Crumb`s.


## Module Halogen.Themes.Bootstrap3.InputGroup


This module provides convenience functions for creating _input groups_.

#### `inputGroup`

``` purescript
inputGroup :: forall a i. Maybe (H.HTML a i) -> H.HTML a i -> Maybe (H.HTML a i) -> H.HTML a i
```

Create an input group.

An input group consists of a control with optional elements placed before and after.



