-- | This module defines an initial encoding of the `HTML` type,
-- | which can be used to implement traversals.

module Halogen.HTML.Traversals
  ( Attr(..)
  , HTML(..)
  , SingleAttr(..)
  
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

import Halogen.HTML.Events.Types
import Halogen.HTML.Events.Handler

-- | A single attribute is either
-- |
-- | - An attribute
-- | - An event handler
-- |
-- | Both are encoded as existentials-as-universals.
data SingleAttr i
  = SingleAttr (forall r. (forall value. (Show value) => H.AttributeName value -> value -> r) -> r)
  | SingleHandler (forall r. (forall fields. H.EventName fields -> (Event fields -> EventHandler (Maybe i)) -> r) -> r)

-- | An initial encoding of attributes.
newtype Attr i = Attr [SingleAttr i]

-- | An initial encoding of HTML nodes.
data HTML a i
  = Text String
  | Element H.TagName (Attr i) [HTML a i]
  | Placeholder a

-- | Convert the final encoding to the initial encoding.
toAttr :: forall i. H.Attr i -> Attr i
toAttr = H.runAttr

-- | Convert the initial encoding to the final encoding.
fromAttr :: forall i. Attr i -> H.Attr i
fromAttr (Attr xs) = foldMap go xs
  where
  go :: SingleAttr i -> H.Attr i
  go (SingleAttr f) = f \name value -> H.attr_ name value
  go (SingleHandler f) = f \name handler -> H.handler_ name handler
  
-- | Convert the final encoding to the initial encoding.
toHTML :: forall p i. H.HTML p i -> HTML p i
toHTML = H.runHTML

-- | Convert the initial encoding to the final encoding.
fromHTML :: forall p i. HTML p i -> H.HTML p i
fromHTML (Text s) = H.text_ s
fromHTML (Element name attrs els) = H.element_ name (fromAttr attrs) (fromHTML <$> els)
fromHTML (Placeholder p) = H.placeholder_ p

-- | Replace placeholder nodes with HTML documents.
graft :: forall a b i. HTML a i -> (a -> HTML b i) -> HTML b i 
graft (Placeholder a) f = f a
graft (Element name attr els) f = Element name attr ((`graft` f) <$> els)
graft (Text s) _ = Text s

-- | Modify a HTML structure by using the intermediate representation presented in
-- | this module.
modify :: forall p q i j. (HTML p i -> HTML q j) -> H.HTML p i -> H.HTML q j
modify f = fromHTML <<< f <<< toHTML

instance functorSingleAttr :: Functor SingleAttr where
  (<$>) _ (SingleAttr g) = SingleAttr \k -> g k
  (<$>) f (SingleHandler g) = SingleHandler \k -> g (\name handler -> k name (\e -> (f <$>) <$> handler e))
  
instance functorAttr :: Functor Attr where
  (<$>) f (Attr as) = Attr ((f <$>) <$> as)
  
instance altAttr :: Alt Attr where
  (<|>) (Attr as) (Attr bs) = Attr (as <> bs)
  
instance plusAttr :: Plus Attr where
  empty = Attr []
  
instance attrRepr :: H.AttrRepr Attr where 
  attr_ name value = Attr [ SingleAttr \k -> k name value ]
  handler_ name handler = Attr [ SingleHandler \k -> k name handler ]
  
instance bifunctorHTML :: Bifunctor HTML where
  bimap _ _ (Text s) = Text s
  bimap f g (Element name attr els) = Element name (g <$> attr) (bimap f g <$> els)
  bimap f _ (Placeholder a) = Placeholder (f a)
  
instance htmlRepr :: H.HTMLRepr HTML where 
  text_ = Text
  element_ name attr els = Element name (H.runAttr attr) els
  placeholder_ = Placeholder
