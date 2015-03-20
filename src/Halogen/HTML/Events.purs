-- | This module defines well-typed wrappers for common DOM events, so that
-- | they may be safely embedded in HTML documents.

module Halogen.HTML.Events 
  ( createHandler
  
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

import qualified Halogen.HTML as H

-- | This function can be used to attach custom event handlers.
createHandler :: forall fields attr i. (H.AttrRepr attr) => H.EventName fields -> (Event fields -> EventHandler i) -> attr i
createHandler key f = H.handler key \e -> Just <$> f e

onabort	:: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onabort = createHandler (H.eventName "abort")

onbeforeunload :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onbeforeunload = createHandler (H.eventName "beforeunload")

onerror :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onerror = createHandler (H.eventName "error")

onhashchange :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onhashchange = createHandler (H.eventName "hashchange")

onload :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onload = createHandler (H.eventName "load")

onpageshow :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onpageshow = createHandler (H.eventName "pageshow")

onpagehide :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onpagehide = createHandler (H.eventName "pagehide")

onresize :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onresize = createHandler (H.eventName "resize")

onscroll :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onscroll = createHandler (H.eventName "scroll")

onunload :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onunload = createHandler (H.eventName "unload")

onchange :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onchange = createHandler (H.eventName "change")

oninput :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
oninput = createHandler (H.eventName "input")

oninvalid :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
oninvalid = createHandler (H.eventName "invalid")

onreset :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onreset = createHandler (H.eventName "reset")

onsearch :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onsearch = createHandler (H.eventName "search")

onselect :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onselect = createHandler (H.eventName "select")

onsubmit :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
onsubmit = createHandler (H.eventName "submit")

onclick :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
onclick = createHandler (H.eventName "click")

oncontextmenu :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
oncontextmenu = createHandler (H.eventName "contextmenu")

ondblclick :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
ondblclick = createHandler (H.eventName "dblclick")

onmousedown :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
onmousedown = createHandler (H.eventName "mousedown")

onmouseenter :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
onmouseenter = createHandler (H.eventName "mouseenter")

onmouseleave :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
onmouseleave = createHandler (H.eventName "mouseleave")

onmousemove :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
onmousemove = createHandler (H.eventName "mousemove")

onmouseover :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
onmouseover = createHandler (H.eventName "mouseover")

onmouseout :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
onmouseout = createHandler (H.eventName "mouseout")

onmouseup :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
onmouseup = createHandler (H.eventName "mouseup")

onkeydown :: forall attr i. (H.AttrRepr attr) => (Event KeyboardEvent -> EventHandler i) -> attr i
onkeydown = createHandler (H.eventName "keydown")

onkeypress :: forall attr i. (H.AttrRepr attr) => (Event KeyboardEvent -> EventHandler i) -> attr i
onkeypress = createHandler (H.eventName "keypress")

onkeyup :: forall attr i. (H.AttrRepr attr) => (Event KeyboardEvent -> EventHandler i) -> attr i
onkeyup = createHandler (H.eventName "keyup")

onblur :: forall attr i. (H.AttrRepr attr) => (Event FocusEvent -> EventHandler i) -> attr i
onblur = createHandler (H.eventName "blur")

onfocus :: forall attr i. (H.AttrRepr attr) => (Event FocusEvent -> EventHandler i) -> attr i
onfocus = createHandler (H.eventName "focus")

onfocusin :: forall attr i. (H.AttrRepr attr) => (Event FocusEvent -> EventHandler i) -> attr i
onfocusin = createHandler (H.eventName "focusin")

onfocusout :: forall attr i. (H.AttrRepr attr) => (Event FocusEvent -> EventHandler i) -> attr i
onfocusout = createHandler (H.eventName "focusout")