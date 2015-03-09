module Halogen.HTML.Attributes 
  ( ClassName()
  , className
  , runClassName
  
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
  , checked
  , placeholder
  ) where

import DOM

import Data.Array (map)
import Data.String (joinWith)

import Control.Monad.Eff
import Control.Monad.ST

import Halogen.HTML (Attribute())
import Halogen.HTML.Attributes.Unsafe
import Halogen.Internal.VirtualDOM

-- | A wrapper for strings which are used as CSS classes
newtype ClassName = ClassName String

-- Create a class name
className :: String -> ClassName
className = ClassName

-- | Unpack a class name
runClassName :: ClassName -> String
runClassName (ClassName s) = s

alt :: forall i. String -> Attribute i
alt = unsafeAttribute "alt"
     
charset :: forall i. String -> Attribute i
charset = unsafeAttribute "charset"

class_ :: forall i. ClassName -> Attribute i
class_ = unsafeAttribute "className" <<< runClassName

classes :: forall i. [ClassName] -> Attribute i
classes ss = unsafeAttribute "className" (joinWith " " $ map runClassName ss)

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

checked :: forall i. Boolean -> Attribute i
checked = unsafeAttribute "checked"
   
placeholder :: forall i. String -> Attribute i
placeholder = unsafeAttribute "placeholder"
