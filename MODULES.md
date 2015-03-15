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

#### `AttributeValue`

``` purescript
data AttributeValue i
  = StringAttribute AttributeName String
  | BooleanAttribute AttributeName Boolean
  | MapAttribute AttributeName (StrMap String)
  | HandlerAttribute (forall r. (forall event. EventName event -> (Event event -> EventHandler (Maybe i)) -> r) -> r)
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
  = Attribute [AttributeValue i]
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

#### `handler`

``` purescript
handler :: forall fields i. H.EventName fields -> (Event fields -> EventHandler i) -> H.Attribute i
```

This function can be used to attach custom event handlers.

#### `handlerMaybe`

``` purescript
handlerMaybe :: forall fields i. H.EventName fields -> (Event fields -> EventHandler (Maybe i)) -> H.Attribute i
```

This function can be used to attach custom event handlers.

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



## Module Halogen.HTML.Target


This module defines a type of _link targets_, which can be used as the target of a hyperlink or button.

This type is quite useful when defining reusable components.

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

#### `Target`

``` purescript
data Target a
  = LinkTarget URL
  | DataTarget a
```

There are two types of target:

- `LinkTarget` creates a target which links to a URL.
- `DataTarget` creates a target which carries data which may be used to generate inputs or requests.

#### `target`

``` purescript
target :: forall i. Target i -> H.Attribute i
```

Attach a `Target` to an element using the `href` or `onclick` attribute as appropriate


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



