# Rendering Halogen HTML

Halogen HTML elements are the smallest building block of Halogen applications. These elements describe what you want to see on the screen.

Halogen HTML elements are not components (we'll get to components in the next chapter), and they can't be rendered without a component. However, it's common to write helper functions that produce Halogen HTML and then use those functions in a component.

We'll explore writing HTML without components or events in this chapter.

## Halogen HTML

You can write Halogen HTML using functions from the `Halogen.HTML` or `Halogen.HTML.Keyed` modules as in this example:

```purs
import Halogen.HTML as HH

element = HH.h1 [ ] [ HH.text "Hello, world" ]
```

Halogen HTML elements can be thought of like browser DOM elements, but they are controlled by the Halogen library instead of being actual elements in the DOM. Under the hood, Halogen takes care of updating the actual DOM to match the code you have written.

Elements in Halogen accept two arguments:

1. An array of attributes, properties, event handlers, and/or references to apply to the element. These correspond with ordinary HTML properties like `placeholder` and event handlers like `onClick`. We'll learn how to handle events in the next chapter, and we'll only focus on properties in this chapter.
2. An array of children, if the element supports children.

As a brief example, let's translate this ordinary HTML into Halogen HTML:

```html
<div id="root">
  <input placeholder="Name" />
  <button class="btn-primary" type="submit">
    Submit
  </button>
</div>
```

Let's break down our Halogen HTML:

1. Our Halogen code has the same shape as our ordinary HTML: a `div` containing an `input` and a `button`, which itself contains plain text.
2. Properties move from key-value pairs inside the tags into an array of properties for the element.
3. Child elements move from being inside an open and closing tag into an array of children, if the element supports children.

Functions for writing properties in your HTML come from the `Halogen.HTML.Properties` module.

```purs
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

html =
  HH.div
    [ HP.id_ "root" ]
    [ HH.input
        [ HP.placeholder "Name" ]
    , HH.button
        [ HP.classes [ HH.ClassName "btn-primary" ]
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Submit" ]
    ]
```

You can see Halogen's emphasis on type safety displayed here.

1. A text input can't have children, so Halogen doesn't allow the element to take further elements as an argument.
2. Only some values are possible for a button's `type` property, so Halogen restricts them with a sum type.
3. CSS classes use a `ClassName` newtype so that they can be treated specially when needed; for example, the `classes` function ensures that your classes are space-separated when they're combined.

Some HTML elements and properties clash with reserved keywords in PureScript or with common functions from the Prelude, so Halogen adds an underscore to them. That's why you see `type_` instead of `type` in the example above. (You also see `id_` instead of `id`, but in PureScript 0.12 the `id` function was renamed to `identity`; future versions of Halogen will just use `id` without an underscore.)

When you don't need to set any properties on a Halogen HTML element, you can use its underscored version instead. For example, the `div` and `button` elements below have no properties:

```purs
html = HH.div [ ] [ HH.button [ ] [ HH.text "Click me!"] ]
```

That means we can rewrite them using their underscored versions. This can help keep your HTML tidy.

```purs
html = HH.div_ [ HH.button_ [ HH.text "Click me!" ] ]
```

## Writing Functions in Halogen HTML

It's common to write helper functions for Halogen HTML. Since Halogen HTML is built from ordinary PureScript functions, you can freely intersperse other functions in your code. 

In this example, our function accepts an integer and renders it as text:

```purs
header :: forall w i. Int -> HH.HTML w i
header visits = 
  HH.h1_ 
    [ HH.text $ "You've had " <> show visits <> " visitors" ] 
```

We can also render lists of things:

```purs
lakes = [ "Lake Norman", "Lake Wylie" ]

html :: forall w i. HH.HTML w i
html = HH.div_ (map HH.text lakes)
-- same as: HH.div_ [ HH.text "Lake Norman", HH.text "Lake Wylie" ]
```

These function introduced a new type, `HH.HTML`, which you haven't seen before. Don't worry! This is the type of Halogen HTML, and we'll learn about it in the next section. For now, let's continue learning about using functions in HTML.

One common requirement is to conditionally render some HTML. You can do this with ordinary `if` and `case` statements, but it's useful to write helper functions for common patterns. Let's walk through two helper functions you might write in your own applications, which will help us get more practice writing functions with Halogen HTML.

First, you may sometimes need to deal with elements that may or may not exist. A function like the one below lets you render a value if it exists, and render an empty node otherwise.

```purs
maybeElem :: forall w i a. Maybe a -> (a -> HH.HTML w i) -> HH.HTML w i
maybeElem val f =
  case val of
    Just x -> f x
    _ -> HH.text ""

-- Render the name, if there is one
renderName :: forall w i. Maybe String -> HH.HTML w i
renderName mbName = maybeElem mbName \name -> HH.text name
```

Second, you may want to render some HTML only if a condition is true, without computing the HTML if it fails the condition. You can do this by hiding its evaluation behind a function so the HTML is only computed when the condition is true.

```purs
whenElem :: forall w i. Boolean -> (Unit -> HH.HTML w i) -> HH.HTML w i
whenElem cond f = if cond then f unit else HH.text ""

-- Render the old number, but only if it is different from the new number
renderOld :: forall w i. { old :: Number, new :: Number } -> HH.HTML w i
renderOld { old, new } = 
  whenElem (old /= new) \_ -> 
    HH.div_ [ HH.text $ show old ]
```

Now that we've explored a few ways to work with HTML, let's learn more about the types that describe it.

## HTML Types

So far we've written HTML without type signatures. But when you write Halogen HTML in your application you'll include the type signatures.

### `HTML w i` 

`HTML` is the core type for HTML in Halogen. It is used for HTML elements that are not tied to a particular kind of component. For example, it's used as the type for the `h1`, `text`, and `button` elements we've seen so far. You can also use this type when defining your own custom HTML elements.

The `HTML` type takes two type parameters: `w`, which stands for "widget" and describes what components can be used in the HTML, and `i`, which stands for "input" and represents the type used to handle DOM events.

When you write helper functions for Halogen HTML that don't need to respond to DOM events, then you will typically use the `HTML` type without specifying what `w` and `i` are. For example, this helper function lets you create a button, given a label:

```purs
primaryButton :: forall w i. String -> HH.HTML w i
primaryButton label =
  HH.button
    [ HP.classes [ HH.ClassName "primary" ] ]
    [ HH.text label ]
```

You could also accept HTML as the label instead of accepting just a string:

```purs
primaryButton :: forall w i. HH.HTML w i -> HH.HTML w i
primaryButton label =
  HH.button
    [ HP.classes [ HH.ClassName "primary" ] ]
    [ label ]
```

Of course, being a button, you probably want to do something when it's clicked. Don't worry -- we'll cover handling DOM events in the next chapter!

### `ComponentHTML` and `PlainHTML`

There are two other HTML types you will commonly see in Halogen applications.

`ComponentHTML` is used when you write HTML that is meant to work with a particular type of component. It can also be used outside of components, but it is most commonly used within them. We'll learn more about this type in the next chapter.

`PlainHTML` is a more restrictive version of `HTML` that's used for HTML that doesn't contain components and doesn't respond to events in the DOM. The type lets you hide `HTML`'s two type parameters, which is convenient when you're passing HTML around as a value. However, if you want to combine values of this type with other HTML that does respond to DOM events or contain components, you'll need to convert it with `fromPlainHTML`.

### `IProp`

When you look up functions from the `Halogen.HTML.Properties` and `Halogen.HTML.Events` modules, you'll see the `IProp` type featured prominently. For example, here's the `placeholder` function which will let you set the string placeholder property on a text field:

```purs
placeholder :: forall r i. String -> IProp (placeholder :: String | r) i
placeholder = prop (PropName "placeholder")
```

The `IProp` type is used for events and properties. It uses a row type to uniquely identify particular events and properties; when you then use one of these properties with a Halogen HTML element, Halogen is able to verify whether the element you're applying the property to actually supports it.

This is possible because Halogen HTML elements also carry a row type which lists all the properties and events that it can support. When you apply a property or event to the element, Halogen looks up in the HTML element's row type whether or not it supports the property or event.

This helps ensure your HTML is well-formed. For example, `<div>` elements do not support the `placeholder` property according to the DOM spec. Accordingly, if you try to give a `div` a `placeholder` property in Halogen you'll get a compile-time error:

```purs
-- ERROR: Could not match type ( placeholder :: String | r )
-- with type ( accessKey :: String, class :: String, ... )
html = HH.div [ HP.placeholder "blah" ] [ ]
```

This error tells you that you've tried to use a property with an element that doesn't support it. It first lists the property you tried to use, and then it lists the properties that the element _does_ support. Another example of Halogen's type safety in action!

### Adding missing properties

Not all properties are currently available in Halogen.

For example, you may try to write:
```purs
html = HH.iframe [ HP.sandbox "allow-scripts" ]
```
Only to receive this error:
```
Unknown value HP.sandbox
```
Even though it seems like this property should be [supported](https://pursuit.purescript.org/packages/purescript-dom-indexed/docs/DOM.HTML.Indexed#t:HTMLiframe):
```purs
type HTMLiframe = Noninteractive (height :: CSSPixel, name :: String, onLoad :: Event, sandbox :: String, src :: String, srcDoc :: String, width :: CSSPixel)
```
The solution is to write your own implementation of this missing property:
```purs
sandbox :: forall r i. String -> HH.IProp ( sandbox :: String | r ) i
sandbox = HH.prop (HH.PropName "sandbox")
```
Then you can use it in your HTML element:
```purs
html = HH.iframe [ sandbox "allow-scripts" ]
```
