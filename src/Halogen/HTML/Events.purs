-- | This module defines well-typed wrappers for common DOM events, so that
-- | they may be safely embedded in HTML documents.

module Halogen.HTML.Events 
  ( input
  , withEventHandlerT
  , handlerT
  
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
input :: forall i m a. (Applicative m) => (a -> i) -> a -> EventHandlerT m i
input f e = pure (f e)

-- | Create an event handler which uses `EventHandlerT`.
withEventHandlerT :: forall i m e. (e -> EventHandlerT m i) -> e -> EventHandler (m i)
withEventHandlerT f = unwrapEventHandler <<< f

-- | Attach an event handler which uses `EventHandlerT`.
handlerT :: forall fields m i. H.EventName fields -> (Event fields -> EventHandlerT m i) -> H.Attr (m i)
handlerT name = H.handler name <<< withEventHandlerT

onabort	:: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onabort = handlerT (H.eventName "abort")

onbeforeunload :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onbeforeunload = handlerT (H.eventName "beforeunload")

onerror :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onerror = handlerT (H.eventName "error")

onhashchange :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onhashchange = handlerT (H.eventName "hashchange")

onload :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onload = handlerT (H.eventName "load")

onpageshow :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onpageshow = handlerT (H.eventName "pageshow")

onpagehide :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onpagehide = handlerT (H.eventName "pagehide")

onresize :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onresize = handlerT (H.eventName "resize")

onscroll :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onscroll = handlerT (H.eventName "scroll")

onunload :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onunload = handlerT (H.eventName "unload")

onchange :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onchange = handlerT (H.eventName "change")

oninput :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
oninput = handlerT (H.eventName "input")

oninvalid :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
oninvalid = handlerT (H.eventName "invalid")

onreset :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onreset = handlerT (H.eventName "reset")

onsearch :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onsearch = handlerT (H.eventName "search")

onselect :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onselect = handlerT (H.eventName "select")

onsubmit :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
onsubmit = handlerT (H.eventName "submit")

onclick :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
onclick = handlerT (H.eventName "click")

oncontextmenu :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
oncontextmenu = handlerT (H.eventName "contextmenu")

ondblclick :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
ondblclick = handlerT (H.eventName "dblclick")

onmousedown :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
onmousedown = handlerT (H.eventName "mousedown")

onmouseenter :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
onmouseenter = handlerT (H.eventName "mouseenter")

onmouseleave :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
onmouseleave = handlerT (H.eventName "mouseleave")

onmousemove :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
onmousemove = handlerT (H.eventName "mousemove")

onmouseover :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
onmouseover = handlerT (H.eventName "mouseover")

onmouseout :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
onmouseout = handlerT (H.eventName "mouseout")

onmouseup :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
onmouseup = handlerT (H.eventName "mouseup")

onkeydown :: forall m i. (Event KeyboardEvent -> EventHandlerT m i) -> H.Attr (m i)
onkeydown = handlerT (H.eventName "keydown")

onkeypress :: forall m i. (Event KeyboardEvent -> EventHandlerT m i) -> H.Attr (m i)
onkeypress = handlerT (H.eventName "keypress")

onkeyup :: forall m i. (Event KeyboardEvent -> EventHandlerT m i) -> H.Attr (m i)
onkeyup = handlerT (H.eventName "keyup")

onblur :: forall m i. (Event FocusEvent -> EventHandlerT m i) -> H.Attr (m i)
onblur = handlerT (H.eventName "blur")

onfocus :: forall m i. (Event FocusEvent -> EventHandlerT m i) -> H.Attr (m i)
onfocus = handlerT (H.eventName "focus")

onfocusin :: forall m i. (Event FocusEvent -> EventHandlerT m i) -> H.Attr (m i)
onfocusin = handlerT (H.eventName "focusin")

onfocusout :: forall m i. (Event FocusEvent -> EventHandlerT m i) -> H.Attr (m i)
onfocusout = handlerT (H.eventName "focusout")