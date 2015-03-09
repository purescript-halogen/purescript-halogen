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

import Halogen.HTML (Attribute())
import Halogen.HTML.Events.Unsafe
import Halogen.HTML.Events.Handler
import Halogen.HTML.Events.Types

onabort	:: forall i. (Event () -> EventHandler i) -> Attribute i
onabort = unsafeHandler "abort"

onbeforeunload :: forall i. (Event () -> EventHandler i) -> Attribute i
onbeforeunload = unsafeHandler "beforeunload"

onerror :: forall i. (Event () -> EventHandler i) -> Attribute i
onerror = unsafeHandler "error"

onhashchange :: forall i. (Event () -> EventHandler i) -> Attribute i
onhashchange = unsafeHandler "hashchange"

onload :: forall i. (Event () -> EventHandler i) -> Attribute i
onload = unsafeHandler "load"

onpageshow :: forall i. (Event () -> EventHandler i) -> Attribute i
onpageshow = unsafeHandler "pageshow"

onpagehide :: forall i. (Event () -> EventHandler i) -> Attribute i
onpagehide = unsafeHandler "pagehide"

onresize :: forall i. (Event () -> EventHandler i) -> Attribute i
onresize = unsafeHandler "resize"

onscroll :: forall i. (Event () -> EventHandler i) -> Attribute i
onscroll = unsafeHandler "scroll"

onunload :: forall i. (Event () -> EventHandler i) -> Attribute i
onunload = unsafeHandler "unload"

onchange :: forall i. (Event () -> EventHandler i) -> Attribute i
onchange = unsafeHandler "change"

oninput :: forall i. (Event () -> EventHandler i) -> Attribute i
oninput = unsafeHandler "input"

oninvalid :: forall i. (Event () -> EventHandler i) -> Attribute i
oninvalid = unsafeHandler "invalid"

onreset :: forall i. (Event () -> EventHandler i) -> Attribute i
onreset = unsafeHandler "reset"

onsearch :: forall i. (Event () -> EventHandler i) -> Attribute i
onsearch = unsafeHandler "search"

onselect :: forall i. (Event () -> EventHandler i) -> Attribute i
onselect = unsafeHandler "select"

onsubmit :: forall i. (Event () -> EventHandler i) -> Attribute i
onsubmit = unsafeHandler "submit"

onclick :: forall i. (Event MouseEvent -> EventHandler i) -> Attribute i
onclick = unsafeHandler "click"

oncontextmenu :: forall i. (Event MouseEvent -> EventHandler i) -> Attribute i
oncontextmenu = unsafeHandler "contextmenu"

ondblclick :: forall i. (Event MouseEvent -> EventHandler i) -> Attribute i
ondblclick = unsafeHandler "dblclick"

onmousedown :: forall i. (Event MouseEvent -> EventHandler i) -> Attribute i
onmousedown = unsafeHandler "mousedown"

onmouseenter :: forall i. (Event MouseEvent -> EventHandler i) -> Attribute i
onmouseenter = unsafeHandler "mouseenter"

onmouseleave :: forall i. (Event MouseEvent -> EventHandler i) -> Attribute i
onmouseleave = unsafeHandler "mouseleave"

onmousemove :: forall i. (Event MouseEvent -> EventHandler i) -> Attribute i
onmousemove = unsafeHandler "mousemove"

onmouseover :: forall i. (Event MouseEvent -> EventHandler i) -> Attribute i
onmouseover = unsafeHandler "mouseover"

onmouseout :: forall i. (Event MouseEvent -> EventHandler i) -> Attribute i
onmouseout = unsafeHandler "mouseout"

onmouseup :: forall i. (Event MouseEvent -> EventHandler i) -> Attribute i
onmouseup = unsafeHandler "mouseup"

onkeydown :: forall i. (Event KeyboardEvent -> EventHandler i) -> Attribute i
onkeydown = unsafeHandler "keydown"

onkeypress :: forall i. (Event KeyboardEvent -> EventHandler i) -> Attribute i
onkeypress = unsafeHandler "keypress"

onkeyup :: forall i. (Event KeyboardEvent -> EventHandler i) -> Attribute i
onkeyup = unsafeHandler "keyup"

onblur :: forall i. (Event FocusEvent -> EventHandler i) -> Attribute i
onblur = unsafeHandler "blur"

onfocus :: forall i. (Event FocusEvent -> EventHandler i) -> Attribute i
onfocus = unsafeHandler "focus"

onfocusin :: forall i. (Event FocusEvent -> EventHandler i) -> Attribute i
onfocusin = unsafeHandler "focusin"

onfocusout :: forall i. (Event FocusEvent -> EventHandler i) -> Attribute i
onfocusout = unsafeHandler "focusout"