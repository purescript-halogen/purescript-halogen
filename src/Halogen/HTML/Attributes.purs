module Halogen.HTML.Attributes 
  ( Attribute()
  
  , attributesToProps
  
  , unsafeAttribute
  , unsafeHandler
  , unsafeHandler'
  
  -- Attributes
  
  , alt
  , charset
  , class_
  , classes
  , content
  , for
  , height
  , href
  , httpEquiv
  , id_
  , name
  , rel
  , src
  , target
  , title
  , type_
  , value
  , width
  , disabled
  , enabled
  
  -- Event Handlers
  
  , Event()
  
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
  
  , MouseEvent()
  
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

  , KeyboardEvent()
  
  , onkeydown
  , onkeypress
  , onkeyup
  
  , FocusEvent()

  , onblur 
  , onfocus
  , onfocusin
  , onfocusout
  
  ) where

import DOM

import Data.Maybe
import Data.Monoid
import Data.String (joinWith)
import Data.Function (runFn3)
import Data.Foldable (for_)

import Control.Monad.Eff
import Control.Monad.ST

import Halogen.VirtualDOM

-- | A HTML attribute which can be used in a document of type `HTML i`.
data Attribute i = Attribute (forall h eff eff1. (i -> Eff eff Unit) -> STProps h -> Eff (st :: ST h | eff1) Unit)

instance functorAttribute :: Functor Attribute where
  (<$>) f (Attribute h) = Attribute \k -> h (f >>> k)
  
instance semigroupAttribute :: Semigroup (Attribute i) where
  (<>) (Attribute f) (Attribute g) = Attribute \k props -> do
    f k props
    g k props

instance monoidAttribute :: Monoid (Attribute i) where
  mempty = Attribute \_ _ -> return unit

-- | This function can be used to define custom string attributes.
unsafeAttribute :: forall i value. String -> value -> Attribute i
unsafeAttribute key value = Attribute \_ props -> runFn3 prop key value props

-- | This function can be used to attach custom event handlers.
unsafeHandler :: forall event eff i. String -> (event -> i) -> Attribute i
unsafeHandler key f = unsafeHandler' key (Just <<< f)

-- | This function can be used to attach custom event handlers.
unsafeHandler' :: forall event i. String -> (event -> Maybe i) -> Attribute i
unsafeHandler' key f = Attribute \k props -> runFn3 handlerProp key (\e -> maybe (return unit) k (f e)) props

-- | Convert a collection of attributes to `Props` by providing an event handler
attributesToProps :: forall i eff. (i -> Eff eff Unit) -> [Attribute i] -> Props
attributesToProps k attribs
  | Data.Array.null attribs = emptyProps
  | otherwise = runProps do stp <- newProps
                            for_ attribs (addProp stp)
                            return stp
  where    
  addProp :: forall h eff. STProps h -> Attribute i -> Eff (st :: ST h | eff) Unit
  addProp props (Attribute f) = f k props

alt :: forall i. String -> Attribute i
alt = unsafeAttribute "alt"
     
charset :: forall i. String -> Attribute i
charset = unsafeAttribute "charset"

class_ :: forall i. String -> Attribute i
class_ = unsafeAttribute "className"

classes :: forall i. [String] -> Attribute i
classes ss = class_ (joinWith " " ss)

content :: forall i. String -> Attribute i
content = unsafeAttribute "content"

for :: forall i. String -> Attribute i
for = unsafeAttribute "for"

height :: forall i. Number -> Attribute i
height = unsafeAttribute "height" <<< show

href :: forall i. String -> Attribute i
href = unsafeAttribute "href"

httpEquiv :: forall i. String -> Attribute i
httpEquiv = unsafeAttribute "http-equiv"

id_ :: forall i. String -> Attribute i
id_ = unsafeAttribute "id"
   
name :: forall i. String -> Attribute i
name = unsafeAttribute "name"
       
rel :: forall i. String -> Attribute i
rel = unsafeAttribute "rel"
    
src :: forall i. String -> Attribute i
src = unsafeAttribute "src"
   
target :: forall i. String -> Attribute i
target = unsafeAttribute "target"
   
title :: forall i. String -> Attribute i
title = unsafeAttribute "title"
   
type_ :: forall i. String -> Attribute i
type_ = unsafeAttribute "type"
   
value :: forall i. String -> Attribute i
value = unsafeAttribute "value"
   
width :: forall i. Number -> Attribute i
width = unsafeAttribute "width" <<< show
   
disabled :: forall i. Boolean -> Attribute i
disabled = unsafeAttribute "disabled"
   
enabled :: forall i. Boolean -> Attribute i
enabled = disabled <<< not

type Event fields = 
  { bubbles :: Boolean
  , cancelable :: Boolean
  , currentTarget :: Node
  , target :: Node
  , timeStamp :: Number
  , "type" :: String
  | fields
  }

onabort	:: forall i. (Event () -> i) -> Attribute i
onabort = unsafeHandler "onabort"

onbeforeunload :: forall i. (Event () -> i) -> Attribute i
onbeforeunload = unsafeHandler "onbeforeunload"

onerror :: forall i. (Event () -> i) -> Attribute i
onerror = unsafeHandler "onerror"

onhashchange :: forall i. (Event () -> i) -> Attribute i
onhashchange = unsafeHandler "onhashchange"

onload :: forall i. (Event () -> i) -> Attribute i
onload = unsafeHandler "onload"

onpageshow :: forall i. (Event () -> i) -> Attribute i
onpageshow = unsafeHandler "onpageshow"

onpagehide :: forall i. (Event () -> i) -> Attribute i
onpagehide = unsafeHandler "onpagehide"

onresize :: forall i. (Event () -> i) -> Attribute i
onresize = unsafeHandler "onresize"

onscroll :: forall i. (Event () -> i) -> Attribute i
onscroll = unsafeHandler "onscroll"

onunload :: forall i. (Event () -> i) -> Attribute i
onunload = unsafeHandler "onunload"

onchange :: forall i. (Event () -> i) -> Attribute i
onchange = unsafeHandler "onchange"

oninput :: forall i. (Event () -> i) -> Attribute i
oninput = unsafeHandler "oninput"

oninvalid :: forall i. (Event () -> i) -> Attribute i
oninvalid = unsafeHandler "oninvalid"

onreset :: forall i. (Event () -> i) -> Attribute i
onreset = unsafeHandler "onreset"

onsearch :: forall i. (Event () -> i) -> Attribute i
onsearch = unsafeHandler "onsearch"

onselect :: forall i. (Event () -> i) -> Attribute i
onselect = unsafeHandler "onselect"

onsubmit :: forall i. (Event () -> i) -> Attribute i
onsubmit = unsafeHandler "onsubmit"

type MouseEvent = 
  ( button :: Number
  , detail :: Number
  , relatedTarget :: Node
  , clientX :: Number
  , clientY :: Number
  , screenX	:: Number
  , screenY	:: Number
  , ctrlKey	:: Boolean
  , shiftKey :: Boolean
  , altKey :: Boolean
  , metaKey	:: Boolean
  , which :: Number
  )

onclick :: forall i. (Event MouseEvent -> i) -> Attribute i
onclick = unsafeHandler "onclick"

oncontextmenu :: forall i. (Event MouseEvent -> i) -> Attribute i
oncontextmenu = unsafeHandler "oncontextmenu"

ondblclick :: forall i. (Event MouseEvent -> i) -> Attribute i
ondblclick = unsafeHandler "ondblclick"

onmousedown :: forall i. (Event MouseEvent -> i) -> Attribute i
onmousedown = unsafeHandler "onmousedown"

onmouseenter :: forall i. (Event MouseEvent -> i) -> Attribute i
onmouseenter = unsafeHandler "onmouseenter"

onmouseleave :: forall i. (Event MouseEvent -> i) -> Attribute i
onmouseleave = unsafeHandler "onmouseleave"

onmousemove :: forall i. (Event MouseEvent -> i) -> Attribute i
onmousemove = unsafeHandler "onmousemove"

onmouseover :: forall i. (Event MouseEvent -> i) -> Attribute i
onmouseover = unsafeHandler "onmouseover"

onmouseout :: forall i. (Event MouseEvent -> i) -> Attribute i
onmouseout = unsafeHandler "onmouseout"

onmouseup :: forall i. (Event MouseEvent -> i) -> Attribute i
onmouseup = unsafeHandler "onmouseup"

type KeyboardEvent = 
  ( charCode :: Number
  , keyCode :: Number
  , ctrlKey	:: Boolean
  , shiftKey :: Boolean
  , altKey :: Boolean
  , metaKey	:: Boolean
  , which :: Number
  )

onkeydown :: forall i. (Event KeyboardEvent -> i) -> Attribute i
onkeydown = unsafeHandler "onkeydown"

onkeypress :: forall i. (Event KeyboardEvent -> i) -> Attribute i
onkeypress = unsafeHandler "onkeypress"

onkeyup :: forall i. (Event KeyboardEvent -> i) -> Attribute i
onkeyup = unsafeHandler "onkeyup"

type FocusEvent = 
  ( relatedTarget :: Node
  )

onblur :: forall i. (Event FocusEvent -> i) -> Attribute i
onblur = unsafeHandler "onblur"

onfocus :: forall i. (Event FocusEvent -> i) -> Attribute i
onfocus = unsafeHandler "onfocus"

onfocusin :: forall i. (Event FocusEvent -> i) -> Attribute i
onfocusin = unsafeHandler "onfocusin"

onfocusout :: forall i. (Event FocusEvent -> i) -> Attribute i
onfocusout = unsafeHandler "onfocusout"