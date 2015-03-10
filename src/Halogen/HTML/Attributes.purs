module Halogen.HTML.Attributes 
  ( ClassName()
  , className
  , runClassName
  
  , addClass
  
  , attribute
  
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

import Data.Tuple
import Data.Either (either)
import Data.Foreign
import Data.Monoid (mempty)
import Data.Array (map)
import Data.String (joinWith)
import Data.Traversable (mapAccumL)

import Control.Monad.Eff
import Control.Monad.ST

import Halogen.HTML (Attribute(..), AttributeValue(..))
import Halogen.Internal.VirtualDOM

-- | A wrapper for strings which are used as CSS classes
newtype ClassName = ClassName String

-- Create a class name
className :: String -> ClassName
className = ClassName

-- | Unpack a class name
runClassName :: ClassName -> String
runClassName (ClassName s) = s

-- \ This convenience function can be used to add a class name to an existing set of attributes.
-- |
-- | If the `class` attribute already exists, the class name will be appended. If not, it will be added as
-- | a new attribute.
addClass :: forall i. ClassName -> Attribute i -> Attribute i
addClass cn@(ClassName c) (Attribute xs) =
  case mapAccumL go false xs of
    Tuple false ys -> Attribute ys <> class_ cn
    Tuple true ys -> Attribute ys
  where
  go :: Boolean -> Tuple String (AttributeValue i) -> Tuple Boolean (Tuple String (AttributeValue i))
  go false (Tuple "className" (ValueAttribute cs)) = Tuple true (Tuple "className" (ValueAttribute (cs ++ " " ++ c)))
  go b (Tuple k v) = Tuple b (Tuple k v)
    
-- | This function can be used to define custom attributes.
attribute :: forall i value. String -> String -> Attribute i
attribute key value = Attribute [Tuple key (ValueAttribute value)]

alt :: forall i. String -> Attribute i
alt = attribute "alt"
     
charset :: forall i. String -> Attribute i
charset = attribute "charset"

class_ :: forall i. ClassName -> Attribute i
class_ = attribute "className" <<< runClassName

classes :: forall i. [ClassName] -> Attribute i
classes ss = attribute "className" (joinWith " " $ map runClassName ss)

content :: forall i. String -> Attribute i
content = attribute "content"

for :: forall i. String -> Attribute i
for = attribute "for"

height :: forall i. Number -> Attribute i
height = attribute "height" <<< show

href :: forall i. String -> Attribute i
href = attribute "href"

httpEquiv :: forall i. String -> Attribute i
httpEquiv = attribute "http-equiv"

id_ :: forall i. String -> Attribute i
id_ = attribute "id"
   
name :: forall i. String -> Attribute i
name = attribute "name"
       
rel :: forall i. String -> Attribute i
rel = attribute "rel"
    
src :: forall i. String -> Attribute i
src = attribute "src"
   
target :: forall i. String -> Attribute i
target = attribute "target"
   
title :: forall i. String -> Attribute i
title = attribute "title"
   
type_ :: forall i. String -> Attribute i
type_ = attribute "type"
   
value :: forall i. String -> Attribute i
value = attribute "value"
   
width :: forall i. Number -> Attribute i
width = attribute "width" <<< show
   
disabled :: forall i. Boolean -> Attribute i
disabled false = mempty
disabled true = attribute "disabled" "disabled"
   
enabled :: forall i. Boolean -> Attribute i
enabled = disabled <<< not

checked :: forall i. Boolean -> Attribute i
checked false = mempty
checked true = attribute "checked" "checked"
   
placeholder :: forall i. String -> Attribute i
placeholder = attribute "placeholder"
