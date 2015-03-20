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

alt :: forall i. String -> H.Attr i
alt = H.attr $ H.attributeName "alt"
     
charset :: forall i. String -> H.Attr i
charset = H.attr $ H.attributeName "charset"

class_ :: forall i. ClassName -> H.Attr i
class_ = H.attr (H.attributeName "className") <<< runClassName

classes :: forall i. [ClassName] -> H.Attr i
classes ss = H.attr (H.attributeName "className") (joinWith " " $ map runClassName ss)

content :: forall i. String -> H.Attr i
content = H.attr $ H.attributeName "content"

for :: forall i. String -> H.Attr i
for = H.attr $ H.attributeName "for"

height :: forall i. Number -> H.Attr i
height = H.attr (H.attributeName "height") <<< show

href :: forall i. String -> H.Attr i
href = H.attr $ H.attributeName "href"

httpEquiv :: forall i. String -> H.Attr i
httpEquiv = H.attr $ H.attributeName "http-equiv"

id_ :: forall i. String -> H.Attr i
id_ = H.attr $ H.attributeName "id"
   
name :: forall i. String -> H.Attr i
name = H.attr $ H.attributeName "name"
       
rel :: forall i. String -> H.Attr i
rel = H.attr $ H.attributeName "rel"
    
src :: forall i. String -> H.Attr i
src = H.attr $ H.attributeName "src"
   
target :: forall i. String -> H.Attr i
target = H.attr $ H.attributeName "target"
   
title :: forall i. String -> H.Attr i
title = H.attr $ H.attributeName "title"
   
type_ :: forall i. String -> H.Attr i
type_ = H.attr $ H.attributeName "type"
   
value :: forall i. String -> H.Attr i
value = H.attr $ H.attributeName "value"
   
width :: forall i. Number -> H.Attr i
width = H.attr (H.attributeName "width") <<< show
   
disabled :: forall i. Boolean -> H.Attr i
disabled = H.attr $ H.attributeName "disabled"
   
enabled :: forall i. Boolean -> H.Attr i
enabled = disabled <<< not
   
checked :: forall i. Boolean -> H.Attr i
checked = H.attr $ H.attributeName "checked"
   
placeholder :: forall i. String -> H.Attr i
placeholder = H.attr $ H.attributeName "placeholder"

style :: forall i. StrMap String -> H.Attr i
style = H.attr $ H.attributeName "style"
