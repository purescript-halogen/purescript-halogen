module Halogen.HTML.Attributes 
  ( Attribute()
  
  , MouseEvent()
  
  , attributesToProps
  
  , unsafeStringAttribute
  , unsafeHandler
  
  -- Attributes
  
  , alt
  , charset
  , class_
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
  
  -- Event Handlers
  
  , onclick
  
  ) where


import Data.Function (runFn3)
import Data.Foldable (for_)

import Control.Monad.Eff
import Control.Monad.ST

import Halogen.VirtualDOM

-- | A HTML attribute which can be used in a document of type `HTML i`.
data Attribute i = Attribute (forall h eff eff1. (i -> Eff eff Unit) -> STProps h -> Eff (st :: ST h | eff1) Unit)

instance functorAttribute :: Functor Attribute where
  (<$>) f (Attribute h) = Attribute \k -> h (f >>> k)

-- | This function can be used to define custom string attributes.
unsafeStringAttribute :: forall i. String -> String -> Attribute i
unsafeStringAttribute key value = Attribute \_ props -> runFn3 stringProp key value props

-- | This function can be used to attach custom event handlers.
unsafeHandler :: forall event eff i. String -> (event -> i) -> Attribute i
unsafeHandler key f = Attribute \k props -> runFn3 handlerProp key (f >>> k) props

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
alt = unsafeStringAttribute "alt"
     
charset :: forall i. String -> Attribute i
charset = unsafeStringAttribute "charset"

class_ :: forall i. String -> Attribute i
class_ = unsafeStringAttribute "class"

content :: forall i. String -> Attribute i
content = unsafeStringAttribute "content"

for :: forall i. String -> Attribute i
for = unsafeStringAttribute "for"

height :: forall i. Number -> Attribute i
height = unsafeStringAttribute "height" <<< show

href :: forall i. String -> Attribute i
href = unsafeStringAttribute "href"

httpEquiv :: forall i. String -> Attribute i
httpEquiv = unsafeStringAttribute "http-equiv"

id_ :: forall i. String -> Attribute i
id_ = unsafeStringAttribute "id"
   
name :: forall i. String -> Attribute i
name = unsafeStringAttribute "name"
       
rel :: forall i. String -> Attribute i
rel = unsafeStringAttribute "rel"
    
src :: forall i. String -> Attribute i
src = unsafeStringAttribute "src"
   
target :: forall i. String -> Attribute i
target = unsafeStringAttribute "target"
   
title :: forall i. String -> Attribute i
title = unsafeStringAttribute "title"
   
type_ :: forall i. String -> Attribute i
type_ = unsafeStringAttribute "type"
   
value :: forall i. String -> Attribute i
value = unsafeStringAttribute "value"
   
width :: forall i. Number -> Attribute i
width = unsafeStringAttribute "width" <<< show

data MouseEvent

onclick :: forall i. (MouseEvent -> i) -> Attribute i
onclick = unsafeHandler "onclick"