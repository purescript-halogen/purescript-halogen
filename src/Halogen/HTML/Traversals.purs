-- | This module defines an initial encoding of the `HTML` type,
-- | which can be used to implement traversals.

module Halogen.HTML.Traversals
  ( Attr(..)
  , HTML(..)
  
  , toAttr
  , fromAttr
  
  , toHTML
  , fromHTML

  , graft

  , modify
  ) where
      
import Data.Maybe
import Data.Foldable (foldMap)
import Data.Bifunctor

import Control.Alt
import Control.Plus
      
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

import Halogen.HTML.Events.Types
import Halogen.HTML.Events.Handler

-- | A single attribute is either
-- |
-- | - An attribute
-- | - An event handler
-- |
-- | Both are encoded as existentials-as-universals.
data Attr i
  = Attr (forall r. (forall value. (A.IsAttribute value) => A.AttributeName value -> value -> r) -> r)
  | Handler (forall r. (forall fields. A.EventName fields -> (Event fields -> EventHandler (Maybe i)) -> r) -> r)

-- | An initial encoding of HTML nodes.
data HTML a i
  = Text String
  | Element H.TagName [Attr i] [HTML a i]
  | Placeholder a

-- | Convert the final encoding to the initial encoding.
toAttr :: forall i. A.Attr i -> Attr i
toAttr = A.runAttr

-- | Convert the initial encoding to the final encoding.
fromAttr :: forall i. Attr i -> A.Attr i
fromAttr (Attr f) = f \name value -> A.attr name value
fromAttr (Handler f) = f \name handler -> A.handler name handler
  
-- | Convert the final encoding to the initial encoding.
toHTML :: forall p i. (forall node. (H.HTMLRepr node) => node p i) -> HTML p i
toHTML html = html 

-- | Convert the initial encoding to the final encoding.
fromHTML :: forall p i node. (H.HTMLRepr node) => HTML p i -> node p i
fromHTML (Text s) = H.text s
fromHTML (Element name attrs els) = H.element name (fromAttr <$> attrs) (fromHTML <$> els)
fromHTML (Placeholder p) = H.placeholder p

-- | Replace placeholder nodes with HTML documents.
graft :: forall a b i. HTML a i -> (a -> HTML b i) -> HTML b i 
graft (Placeholder a) f = f a
graft (Element name attr els) f = Element name attr ((`graft` f) <$> els)
graft (Text s) _ = Text s

-- | Modify a HTML structure by using the intermediate representation presented in
-- | this module.
modify :: forall p q i j node. (H.HTMLRepr node) => (HTML p i -> HTML q j) -> (forall node. (H.HTMLRepr node) => node p i) -> node q j
modify f html = fromHTML $ f $ toHTML html

instance functorAttr :: Functor Attr where
  (<$>) _ (Attr g) = Attr \k -> g k
  (<$>) f (Handler g) = Handler \k -> g (\name handler -> k name (\e -> (f <$>) <$> handler e))
  
instance attrRepr :: A.AttrRepr Attr where 
  attr name value = Attr \k -> k name value
  handler name handler = Handler \k -> k name handler
  
instance bifunctorHTML :: Bifunctor HTML where
  bimap _ _ (Text s) = Text s
  bimap f g (Element name attr els) = Element name ((g <$>) <$> attr) (bimap f g <$> els)
  bimap f _ (Placeholder a) = Placeholder (f a)
  
instance htmlRepr :: H.HTMLRepr HTML where 
  text = Text
  element name attr els = Element name (A.runAttr <$> attr) els
  placeholder = Placeholder
