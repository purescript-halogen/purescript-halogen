-- | This module enumerates some common HTML attributes, and provides additional
-- | helper functions for working with CSS classes.

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
  , style
  ) where

import DOM

import Data.Tuple
import Data.Either (either)
import Data.Foreign
import Data.StrMap (StrMap())
import Data.Monoid (mempty)
import Data.Array (map)
import Data.String (joinWith)
import Data.Traversable (mapAccumL)

import Control.Monad.Eff
import Control.Monad.ST

import Halogen.Internal.VirtualDOM

import qualified Halogen.HTML as H

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
addClass :: forall i. ClassName -> H.Attribute i -> H.Attribute i
addClass cn@(ClassName c) (H.Attribute xs) =
  case mapAccumL go false xs of
    Tuple false ys -> H.Attribute ys <> class_ cn
    Tuple true ys -> H.Attribute ys
  where
  go :: Boolean -> Tuple H.AttributeName (H.AttributeValue i) -> Tuple Boolean (Tuple H.AttributeName (H.AttributeValue i))
  go false (Tuple name (H.StringAttribute cs)) | H.runAttributeName name == className = 
    Tuple true (Tuple (H.attributeName className) (H.StringAttribute (cs ++ " " ++ c)))
  go b (Tuple k v) = Tuple b (Tuple k v)
  
  className :: String
  className = "className"
    
-- | This function can be used to define custom attributes.
attribute :: forall i value. H.AttributeName -> String -> H.Attribute i
attribute key value = H.Attribute [Tuple key (H.StringAttribute value)]

alt :: forall i. String -> H.Attribute i
alt = attribute $ H.attributeName "alt"
     
charset :: forall i. String -> H.Attribute i
charset = attribute $ H.attributeName "charset"

class_ :: forall i. ClassName -> H.Attribute i
class_ = attribute (H.attributeName "className") <<< runClassName

classes :: forall i. [ClassName] -> H.Attribute i
classes ss = attribute (H.attributeName "className") (joinWith " " $ map runClassName ss)

content :: forall i. String -> H.Attribute i
content = attribute $ H.attributeName "content"

for :: forall i. String -> H.Attribute i
for = attribute $ H.attributeName "for"

height :: forall i. Number -> H.Attribute i
height = attribute (H.attributeName "height") <<< show

href :: forall i. String -> H.Attribute i
href = attribute $ H.attributeName "href"

httpEquiv :: forall i. String -> H.Attribute i
httpEquiv = attribute $ H.attributeName "http-equiv"

id_ :: forall i. String -> H.Attribute i
id_ = attribute $ H.attributeName "id"
   
name :: forall i. String -> H.Attribute i
name = attribute $ H.attributeName "name"
       
rel :: forall i. String -> H.Attribute i
rel = attribute $ H.attributeName "rel"
    
src :: forall i. String -> H.Attribute i
src = attribute $ H.attributeName "src"
   
target :: forall i. String -> H.Attribute i
target = attribute $ H.attributeName "target"
   
title :: forall i. String -> H.Attribute i
title = attribute $ H.attributeName "title"
   
type_ :: forall i. String -> H.Attribute i
type_ = attribute $ H.attributeName "type"
   
value :: forall i. String -> H.Attribute i
value = attribute $ H.attributeName "value"
   
width :: forall i. Number -> H.Attribute i
width = attribute (H.attributeName "width") <<< show
   
disabled :: forall i. Boolean -> H.Attribute i
disabled b = H.Attribute [Tuple (H.attributeName "disabled") (H.BooleanAttribute b)]
   
enabled :: forall i. Boolean -> H.Attribute i
enabled = disabled <<< not
   
checked :: forall i. Boolean -> H.Attribute i
checked b = H.Attribute [Tuple (H.attributeName "checked") (H.BooleanAttribute b)]
   
placeholder :: forall i. String -> H.Attribute i
placeholder = attribute $ H.attributeName "placeholder"

style :: forall i. StrMap String -> H.Attribute i
style m = H.Attribute [Tuple (H.attributeName "style") (H.MapAttribute m)]
