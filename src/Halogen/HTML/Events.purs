-- | This module defines well-typed wrappers for common DOM events, so that
-- | they may be safely embedded in HTML documents.

module Halogen.HTML.Events 
  ( createHandler
  , input
  
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

-- | This function can be used to attach custom event handlers.
createHandler :: forall fields i. H.EventName fields -> (Event fields -> EventHandler i) -> H.Attr i
createHandler key f = H.handler key \e -> Just <$> f e

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

onabort	:: forall i. (Event () -> EventHandler i) -> H.Attr i
onabort = createHandler (H.eventName "abort")

onbeforeunload :: forall i. (Event () -> EventHandler i) -> H.Attr i
onbeforeunload = createHandler (H.eventName "beforeunload")

onerror :: forall i. (Event () -> EventHandler i) -> H.Attr i
onerror = createHandler (H.eventName "error")

onhashchange :: forall i. (Event () -> EventHandler i) -> H.Attr i
onhashchange = createHandler (H.eventName "hashchange")

onload :: forall i. (Event () -> EventHandler i) -> H.Attr i
onload = createHandler (H.eventName "load")

onpageshow :: forall i. (Event () -> EventHandler i) -> H.Attr i
onpageshow = createHandler (H.eventName "pageshow")

onpagehide :: forall i. (Event () -> EventHandler i) -> H.Attr i
onpagehide = createHandler (H.eventName "pagehide")

onresize :: forall i. (Event () -> EventHandler i) -> H.Attr i
onresize = createHandler (H.eventName "resize")

onscroll :: forall i. (Event () -> EventHandler i) -> H.Attr i
onscroll = createHandler (H.eventName "scroll")

onunload :: forall i. (Event () -> EventHandler i) -> H.Attr i
onunload = createHandler (H.eventName "unload")

onchange :: forall i. (Event () -> EventHandler i) -> H.Attr i
onchange = createHandler (H.eventName "change")

oninput :: forall i. (Event () -> EventHandler i) -> H.Attr i
oninput = createHandler (H.eventName "input")

oninvalid :: forall i. (Event () -> EventHandler i) -> H.Attr i
oninvalid = createHandler (H.eventName "invalid")

onreset :: forall i. (Event () -> EventHandler i) -> H.Attr i
onreset = createHandler (H.eventName "reset")

onsearch :: forall i. (Event () -> EventHandler i) -> H.Attr i
onsearch = createHandler (H.eventName "search")

onselect :: forall i. (Event () -> EventHandler i) -> H.Attr i
onselect = createHandler (H.eventName "select")

onsubmit :: forall i. (Event () -> EventHandler i) -> H.Attr i
onsubmit = createHandler (H.eventName "submit")

onclick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onclick = createHandler (H.eventName "click")

oncontextmenu :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
oncontextmenu = createHandler (H.eventName "contextmenu")

ondblclick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
ondblclick = createHandler (H.eventName "dblclick")

onmousedown :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmousedown = createHandler (H.eventName "mousedown")

onmouseenter :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmouseenter = createHandler (H.eventName "mouseenter")

onmouseleave :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmouseleave = createHandler (H.eventName "mouseleave")

onmousemove :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmousemove = createHandler (H.eventName "mousemove")

onmouseover :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmouseover = createHandler (H.eventName "mouseover")

onmouseout :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmouseout = createHandler (H.eventName "mouseout")

onmouseup :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onmouseup = createHandler (H.eventName "mouseup")

onkeydown :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attr i
onkeydown = createHandler (H.eventName "keydown")

onkeypress :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attr i
onkeypress = createHandler (H.eventName "keypress")

onkeyup :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attr i
onkeyup = createHandler (H.eventName "keyup")

onblur :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
onblur = createHandler (H.eventName "blur")

onfocus :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
onfocus = createHandler (H.eventName "focus")

onfocusin :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
onfocusin = createHandler (H.eventName "focusin")

onfocusout :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
onfocusout = createHandler (H.eventName "focusout")