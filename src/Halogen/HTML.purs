module Halogen.HTML
  ( HTML()
  , Attribute(..)
  
  , MouseEvent()
  
  , text
  
  , button
  , button'
  , div
  , div'
  
  , renderHtml
  ) where

import Data.Array
import Data.Tuple
import Data.Foldable (foldMap)

import Control.Monad.Eff

import VirtualDOM
import VirtualDOM.VTree

import Halogen.Props

data MouseEvent

-- TODO: add more event types
data Attribute i
  = OnClick (MouseEvent -> i)

instance functorAttribute :: Functor Attribute where
  (<$>) f (OnClick g) = OnClick (f <<< g)

data HTML i
  = Text String
  | Element String [Attribute i] [HTML i]

instance functorHTML :: Functor HTML where
  (<$>) _ (Text s) = Text s
  (<$>) f (Element name attribs children) = Element name (map (f <$>) attribs) (map (f <$>) children)

renderHtml :: forall i eff. (i -> Eff eff Unit) -> HTML i -> VTree
renderHtml _ (Text s) = vtext s
renderHtml k (Element name attribs children) = vnode name (propsToRecord (foldMap attributeToProps attribs)) (map (renderHtml k) children)
  where
  attributeToProps :: Attribute i -> Props
  attributeToProps (OnClick f) = handlerProp "onclick" (k <<< f)

text :: forall i. String -> HTML i
text = Text

-- TODO: add remaining HTML elements

button :: forall i. [Attribute i] -> [HTML i] -> HTML i
button = Element "button"

button' :: forall i. [HTML i] -> HTML i
button' = button []

div :: forall i. [Attribute i] -> [HTML i] -> HTML i
div = Element "div"

div' :: forall i. [HTML i] -> HTML i
div' = div []

