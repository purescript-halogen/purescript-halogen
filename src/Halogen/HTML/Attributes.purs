-- | This module enumerates some common HTML attributes, and provides additional
-- | helper functions for working with CSS classes.

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

alt :: forall attr i. (H.AttrRepr attr) => String -> attr i
alt = H.attr $ H.attributeName "alt"
     
charset :: forall attr i. (H.AttrRepr attr) => String -> attr i
charset = H.attr $ H.attributeName "charset"

class_ :: forall attr i. (H.AttrRepr attr) => ClassName -> attr i
class_ = H.attr (H.attributeName "className") <<< runClassName

classes :: forall attr i. (H.AttrRepr attr) => [ClassName] -> attr i
classes ss = H.attr (H.attributeName "className") (joinWith " " $ map runClassName ss)

content :: forall attr i. (H.AttrRepr attr) => String -> attr i
content = H.attr $ H.attributeName "content"

for :: forall attr i. (H.AttrRepr attr) => String -> attr i
for = H.attr $ H.attributeName "for"

height :: forall attr i. (H.AttrRepr attr) => Number -> attr i
height = H.attr (H.attributeName "height") <<< show

href :: forall attr i. (H.AttrRepr attr) => String -> attr i
href = H.attr $ H.attributeName "href"

httpEquiv :: forall attr i. (H.AttrRepr attr) => String -> attr i
httpEquiv = H.attr $ H.attributeName "http-equiv"

id_ :: forall attr i. (H.AttrRepr attr) => String -> attr i
id_ = H.attr $ H.attributeName "id"
   
name :: forall attr i. (H.AttrRepr attr) => String -> attr i
name = H.attr $ H.attributeName "name"
       
rel :: forall attr i. (H.AttrRepr attr) => String -> attr i
rel = H.attr $ H.attributeName "rel"
    
src :: forall attr i. (H.AttrRepr attr) => String -> attr i
src = H.attr $ H.attributeName "src"
   
target :: forall attr i. (H.AttrRepr attr) => String -> attr i
target = H.attr $ H.attributeName "target"
   
title :: forall attr i. (H.AttrRepr attr) => String -> attr i
title = H.attr $ H.attributeName "title"
   
type_ :: forall attr i. (H.AttrRepr attr) => String -> attr i
type_ = H.attr $ H.attributeName "type"
   
value :: forall attr i. (H.AttrRepr attr) => String -> attr i
value = H.attr $ H.attributeName "value"
   
width :: forall attr i. (H.AttrRepr attr) => Number -> attr i
width = H.attr (H.attributeName "width") <<< show
   
disabled :: forall attr i. (H.AttrRepr attr) => Boolean -> attr i
disabled = H.attr $ H.attributeName "disabled"
   
enabled :: forall attr i. (H.AttrRepr attr) => Boolean -> attr i
enabled = disabled <<< not
   
checked :: forall attr i. (H.AttrRepr attr) => Boolean -> attr i
checked = H.attr $ H.attributeName "checked"
   
placeholder :: forall attr i. (H.AttrRepr attr) => String -> attr i
placeholder = H.attr $ H.attributeName "placeholder"

style :: forall attr i. (H.AttrRepr attr) => StrMap String -> attr i
style = H.attr $ H.attributeName "style"
