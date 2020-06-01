# Halogen Guide

Halogen is a declarative, component-based UI library for PureScript that emphasizes type safety. In this guide you will learn the core ideas and patterns needed to write real-world applications in Halogen.

Here is a tiny Halogen app that lets you increment and decrement a counter:

```purs
module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = Increment | Decrement

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Just Decrement ] [ HH.text "-" ]
      , HH.div_ [ HH.text $ show state ]
      , HH.button [ HE.onClick \_ -> Just Increment ] [ HH.text "+" ]
      ]

  handleAction = case _ of
    Increment -> H.modify_ \state -> state + 1
    Decrement -> H.modify_ \state -> state - 1
```

You can paste this example (and any other full examples in this guide) into [Try PureScript](https://try.purescript.org). We highly recommend doing this to explore the examples interactively! For example, try changing the buttons so they use the words `"Increment"` and `"Decrement"` instead of the symbols `"+"` and `"-"`.

> By default, Try PureScript will compile every time you make a change. You can also disable the auto-compile feature, which will cause Try PureScript to wait for you to click the "Compile" button to compile your Halogen application.

Don't worry if this code is overwhelming at first -- when you've read the next few chapters of the guide you'll gain a solid understanding of how this component works and how to write your own.

## How to Read This Guide

In this guide we'll explore the building blocks of Halogen apps: elements and components. When you understand these you can create complex apps from small, reusable pieces.

This is a step-by-step introduction to Halogen's main concepts. Each chapter builds on knowledge introduced in previous chapters, so we recommend reading through the guide in order.

Halogen is a PureScript library, and it assumes basic knowledge of PureScript concepts like functions, records, arrays, `do` notation, `Effect`, and `Aff`. It will also help if you understand the basics of HTML and the DOM. If you need a refresher, we recommend:

* For PureScript: the [PureScript Book](https://book.purescript.org) and Jordan Martinez's [PureScript Reference](https://github.com/JordanMartinez/purescript-jordans-reference).
* For HTML: the MDN introductions to [HTML](https://developer.mozilla.org/en-US/docs/Learn/HTML/Introduction_to_HTML) and [DOM events](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Building_blocks/Events).

## Table of Contents

1. [Rendering Halogen HTML](./01-Rendering-Halogen-HTML.md)
2. [Introducing Components](./02-Introducing-Components.md)
3. [Performing Effects](./03-Performing-Effects.md)
4. [Lifecycles & Subscriptions](./04-Lifecycles-Subscriptions.md)
5. [Parent & Child Components](./05-Parent-Child-Components.md)
6. [Running An Application](./06-Running-Application.md)
7. [Next Steps](./07-Next-Steps.md)

