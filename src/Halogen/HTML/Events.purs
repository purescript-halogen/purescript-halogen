-- | This module defines well-typed wrappers for common DOM events, so that
-- | they may be safely embedded in HTML documents.

module Halogen.HTML.Events 
  ( handler
  , handlerMaybe
  
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
handler :: forall fields i. H.EventName fields -> (Event fields -> EventHandler i) -> H.Attribute i
handler key f = handlerMaybe key \e -> Just <$> f e

-- | This function can be used to attach custom event handlers.
handlerMaybe :: forall fields i. H.EventName fields -> (Event fields -> EventHandler (Maybe i)) -> H.Attribute i
handlerMaybe key f = H.Attribute [H.HandlerAttribute (\k -> k key (\e -> f e))]

onabort	:: forall i. (Event () -> EventHandler i) -> H.Attribute i
onabort = handler $ H.eventName "abort"

onbeforeunload :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onbeforeunload = handler $ H.eventName "beforeunload"

onerror :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onerror = handler $ H.eventName "error"

onhashchange :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onhashchange = handler $ H.eventName "hashchange"

onload :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onload = handler $ H.eventName "load"

onpageshow :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onpageshow = handler $ H.eventName "pageshow"

onpagehide :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onpagehide = handler $ H.eventName "pagehide"

onresize :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onresize = handler $ H.eventName "resize"

onscroll :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onscroll = handler $ H.eventName "scroll"

onunload :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onunload = handler $ H.eventName "unload"

onchange :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onchange = handler $ H.eventName "change"

oninput :: forall i. (Event () -> EventHandler i) -> H.Attribute i
oninput = handler $ H.eventName "input"

oninvalid :: forall i. (Event () -> EventHandler i) -> H.Attribute i
oninvalid = handler $ H.eventName "invalid"

onreset :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onreset = handler $ H.eventName "reset"

onsearch :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onsearch = handler $ H.eventName "search"

onselect :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onselect = handler $ H.eventName "select"

onsubmit :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onsubmit = handler $ H.eventName "submit"

onclick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onclick = handler $ H.eventName "click"

oncontextmenu :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
oncontextmenu = handler $ H.eventName "contextmenu"

ondblclick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
ondblclick = handler $ H.eventName "dblclick"

onmousedown :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmousedown = handler $ H.eventName "mousedown"

onmouseenter :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmouseenter = handler $ H.eventName "mouseenter"

onmouseleave :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmouseleave = handler $ H.eventName "mouseleave"

onmousemove :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmousemove = handler $ H.eventName "mousemove"

onmouseover :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmouseover = handler $ H.eventName "mouseover"

onmouseout :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmouseout = handler $ H.eventName "mouseout"

onmouseup :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmouseup = handler $ H.eventName "mouseup"

onkeydown :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attribute i
onkeydown = handler $ H.eventName "keydown"

onkeypress :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attribute i
onkeypress = handler $ H.eventName "keypress"

onkeyup :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attribute i
onkeyup = handler $ H.eventName "keyup"

onblur :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attribute i
onblur = handler $ H.eventName "blur"

onfocus :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attribute i
onfocus = handler $ H.eventName "focus"

onfocusin :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attribute i
onfocusin = handler $ H.eventName "focusin"

onfocusout :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attribute i
onfocusout = handler $ H.eventName "focusout"