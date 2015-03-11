-- | This module defines well-typed wrappers for common DOM events, so that
-- | they may be safely embedded in HTML documents.

module Halogen.HTML.Events 
  ( onabort
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

import Halogen.HTML.Events.Unsafe
import Halogen.HTML.Events.Handler
import Halogen.HTML.Events.Types

import qualified Halogen.HTML as H

onabort	:: forall i. (Event () -> EventHandler i) -> H.Attribute i
onabort = unsafeHandler $ H.attributeName "abort"

onbeforeunload :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onbeforeunload = unsafeHandler $ H.attributeName "beforeunload"

onerror :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onerror = unsafeHandler $ H.attributeName "error"

onhashchange :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onhashchange = unsafeHandler $ H.attributeName "hashchange"

onload :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onload = unsafeHandler $ H.attributeName "load"

onpageshow :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onpageshow = unsafeHandler $ H.attributeName "pageshow"

onpagehide :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onpagehide = unsafeHandler $ H.attributeName "pagehide"

onresize :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onresize = unsafeHandler $ H.attributeName "resize"

onscroll :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onscroll = unsafeHandler $ H.attributeName "scroll"

onunload :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onunload = unsafeHandler $ H.attributeName "unload"

onchange :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onchange = unsafeHandler $ H.attributeName "change"

oninput :: forall i. (Event () -> EventHandler i) -> H.Attribute i
oninput = unsafeHandler $ H.attributeName "input"

oninvalid :: forall i. (Event () -> EventHandler i) -> H.Attribute i
oninvalid = unsafeHandler $ H.attributeName "invalid"

onreset :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onreset = unsafeHandler $ H.attributeName "reset"

onsearch :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onsearch = unsafeHandler $ H.attributeName "search"

onselect :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onselect = unsafeHandler $ H.attributeName "select"

onsubmit :: forall i. (Event () -> EventHandler i) -> H.Attribute i
onsubmit = unsafeHandler $ H.attributeName "submit"

onclick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onclick = unsafeHandler $ H.attributeName "click"

oncontextmenu :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
oncontextmenu = unsafeHandler $ H.attributeName "contextmenu"

ondblclick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
ondblclick = unsafeHandler $ H.attributeName "dblclick"

onmousedown :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmousedown = unsafeHandler $ H.attributeName "mousedown"

onmouseenter :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmouseenter = unsafeHandler $ H.attributeName "mouseenter"

onmouseleave :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmouseleave = unsafeHandler $ H.attributeName "mouseleave"

onmousemove :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmousemove = unsafeHandler $ H.attributeName "mousemove"

onmouseover :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmouseover = unsafeHandler $ H.attributeName "mouseover"

onmouseout :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmouseout = unsafeHandler $ H.attributeName "mouseout"

onmouseup :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attribute i
onmouseup = unsafeHandler $ H.attributeName "mouseup"

onkeydown :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attribute i
onkeydown = unsafeHandler $ H.attributeName "keydown"

onkeypress :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attribute i
onkeypress = unsafeHandler $ H.attributeName "keypress"

onkeyup :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attribute i
onkeyup = unsafeHandler $ H.attributeName "keyup"

onblur :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attribute i
onblur = unsafeHandler $ H.attributeName "blur"

onfocus :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attribute i
onfocus = unsafeHandler $ H.attributeName "focus"

onfocusin :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attribute i
onfocusin = unsafeHandler $ H.attributeName "focusin"

onfocusout :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attribute i
onfocusout = unsafeHandler $ H.attributeName "focusout"