-- | This module defines well-typed wrappers for common DOM events, so that
-- | they may be safely embedded in HTML documents.

module Halogen.HTML.Events
  ( input
  , input_

  , onabort
  , onbeforeunload
  , onerror
  , onhashchange
  , onload
  , onpageshow
  , onpagehide
  , onresize
  , onscroll
  , onunload
  , onchange
  , oninput
  , oninvalid
  , onreset
  , onsearch
  , onselect
  , onsubmit
  , onclick
  , oncontextmenu
  , ondblclick
  , onmousedown
  , onmouseenter
  , onmouseleave
  , onmousemove
  , onmouseover
  , onmouseout
  , onmouseup
  , onkeydown
  , onkeypress
  , onkeyup
  , onblur
  , onfocus
  , onfocusin
  , onfocusout
  ) where

import Data.Maybe

import Halogen.HTML.Events.Handler
import Halogen.HTML.Events.Types

import qualified Halogen.HTML.Attributes as H

-- | A helper function which can be used to create simple event handlers.
-- |
-- | Often we don't need to use `EventHandler` or the monad underlying our component, and just need
-- | to generate an input to the signal function.
-- |
-- | This function provides an alternative to making two nested calls to `pure`:
-- |
-- | ```purescript
-- | onclick (input \_ -> Input)
-- | ```
input :: forall i m a. (Applicative m) => (a -> i) -> a -> EventHandler (m i)
input f e = pure (pure (f e))

-- | A helper function for simple event handlers that provide an input to the signal function,
-- | where there is no need to make use of the event value to generate the input.
-- |
-- | ```purescript
-- | onclick (input_ Input)
-- | ```
input_ :: forall i m a. (Applicative m) => i -> a -> EventHandler (m i)
input_ x _ = pure (pure x)

onabort	:: forall i. (Event () -> EventHandler i) -> H.Attr i
onabort = H.handler (H.eventName "abort")

onbeforeunload :: forall i. (Event () -> EventHandler i) -> H.Attr i
onbeforeunload = H.handler (H.eventName "beforeunload")

onerror :: forall i. (Event () -> EventHandler i) -> H.Attr i
onerror = H.handler (H.eventName "error")

onhashchange :: forall i. (Event () -> EventHandler i) -> H.Attr i
onhashchange = H.handler (H.eventName "hashchange")

onload :: forall i. (Event () -> EventHandler i) -> H.Attr i
onload = H.handler (H.eventName "load")

onpageshow :: forall i. (Event () -> EventHandler i) -> H.Attr i
onpageshow = H.handler (H.eventName "pageshow")

onpagehide :: forall i. (Event () -> EventHandler i) -> H.Attr i
onpagehide = H.handler (H.eventName "pagehide")

onresize :: forall i. (Event () -> EventHandler i) -> H.Attr i
onresize = H.handler (H.eventName "resize")

onscroll :: forall i. (Event () -> EventHandler i) -> H.Attr i
onscroll = H.handler (H.eventName "scroll")

onunload :: forall i. (Event () -> EventHandler i) -> H.Attr i
onunload = H.handler (H.eventName "unload")

onchange :: forall i. (Event () -> EventHandler i) -> H.Attr i
onchange = H.handler (H.eventName "change")

oninput :: forall i. (Event () -> EventHandler i) -> H.Attr i
oninput = H.handler (H.eventName "input")

oninvalid :: forall i. (Event () -> EventHandler i) -> H.Attr i
oninvalid = H.handler (H.eventName "invalid")

onreset :: forall i. (Event () -> EventHandler i) -> H.Attr i
onreset = H.handler (H.eventName "reset")

onsearch :: forall i. (Event () -> EventHandler i) -> H.Attr i
onsearch = H.handler (H.eventName "search")

onselect :: forall i. (Event () -> EventHandler i) -> H.Attr i
onselect = H.handler (H.eventName "select")

onsubmit :: forall i. (Event () -> EventHandler i) -> H.Attr i
onsubmit = H.handler (H.eventName "submit")

onclick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onclick = H.handler (H.eventName "click")

oncontextmenu :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
oncontextmenu = H.handler (H.eventName "contextmenu")

ondblclick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
ondblclick = H.handler (H.eventName "dblclick")

onmousedown :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmousedown = H.handler (H.eventName "mousedown")

onmouseenter :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmouseenter = H.handler (H.eventName "mouseenter")

onmouseleave :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmouseleave = H.handler (H.eventName "mouseleave")

onmousemove :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmousemove = H.handler (H.eventName "mousemove")

onmouseover :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmouseover = H.handler (H.eventName "mouseover")

onmouseout :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmouseout = H.handler (H.eventName "mouseout")

onmouseup :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmouseup = H.handler (H.eventName "mouseup")

onkeydown :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attr i
onkeydown = H.handler (H.eventName "keydown")

onkeypress :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attr i
onkeypress = H.handler (H.eventName "keypress")

onkeyup :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attr i
onkeyup = H.handler (H.eventName "keyup")

onblur :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
onblur = H.handler (H.eventName "blur")

onfocus :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
onfocus = H.handler (H.eventName "focus")

onfocusin :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
onfocusin = H.handler (H.eventName "focusin")

onfocusout :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
onfocusout = H.handler (H.eventName "focusout")
