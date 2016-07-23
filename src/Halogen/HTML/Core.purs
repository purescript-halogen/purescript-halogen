--| The core types and smart constructors for the HTML DSL.
module Halogen.HTML.Core
  ( HTML(..)
  , element
  , fillSlot

  , Prop(..)
  , PropF(..)
  , HandlerF(..)
  , prop
  , handler

  , class IsProp
  , toPropString

  , Namespace
  , namespace
  , runNamespace

  , TagName
  , tagName
  , runTagName

  , PropName
  , propName
  , runPropName

  , AttrName
  , attrName
  , runAttrName

  , EventName
  , eventName
  , runEventName

  , ClassName
  , className
  , runClassName

  ) where

import Prelude
import Control.Monad.Eff (foreachE, runPure)
import Control.Monad.ST (writeSTRef, readSTRef, newSTRef, runST)
import DOM.HTML.Types (HTMLElement)
import Data.Array.ST (emptySTArray, pushSTArray, STArray) as A
import Data.Bifunctor (class Bifunctor, rmap)
import Data.Exists (Exists, mkExists)
import Data.ExistsR (ExistsR, mkExistsR, runExistsR)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Halogen.Component.Tree (mkTree', graftTree)
import Halogen.HTML.Events.Handler (EventHandler)
import Halogen.HTML.Events.Types (Event)
import Halogen.RenderDSL (class RenderDSL)
import Unsafe.Coerce (unsafeCoerce)

-- | An initial encoding of HTML nodes.
data HTML p i
  = Text String
  | Element (Maybe Namespace) TagName (Array (Prop i)) (Array (HTML p i))
  | Slot p

instance bifunctorHTML :: Bifunctor HTML where
  bimap f g = go
    where
    go (Text s) = Text s
    go (Element ns name props els) = Element ns name (map g <$> props) (go <$> els)
    go (Slot p) = Slot (f p)

instance functorHTML :: Functor (HTML p) where
  map = rmap

-- | A smart constructor for HTML elements.
element :: forall p i. TagName -> Array (Prop i) -> Array (HTML p i) -> HTML p i
element = Element Nothing

-- | Populates the slot placeholder values in a `HTML` value.
fillSlot :: forall p p' i i' m. Applicative m => (p -> m (HTML p' i')) -> (i -> i') -> HTML p i -> m (HTML p' i')
fillSlot _ _ (Text s) = pure $ Text s
fillSlot f g (Element ns name props els) = Element ns name (map g <$> props) <$> traverse (fillSlot f g) els
fillSlot f _ (Slot p) = f p

-- | A property can be:
-- | - A JavaScript property for an element (typed, and may not have a
-- |   corresponding attribute).
-- | - A raw attribute for an element (stringly typed, will be added directly to
-- |   the rendered element)
-- | - A key value used for hinting when diffing HTML.
-- | - An event handler.
-- | - A function that is triggered once the element for the property has
-- |   been added to or removed from the DOM.
data Prop i
  = Prop (Exists PropF)
  | Attr (Maybe Namespace) AttrName String
  | Key String
  | Handler (ExistsR (HandlerF i))
  | Ref (Maybe HTMLElement -> i)

instance functorProp :: Functor Prop where
  map _ (Prop e) = Prop e
  map _ (Key k) = Key k
  map _ (Attr ns k v) = Attr ns k v
  map f (Handler e) = runExistsR (\(HandlerF name k) -> Handler (mkExistsR (HandlerF name (map (map f) <<< k)))) e
  map f (Ref g) = Ref (f <<< g)

-- | The data which represents a typed property, hidden inside an existential
-- | package in the `Prop` type.
data PropF value = PropF (PropName value) value (Maybe (Tuple AttrName (AttrName -> PropName value -> value -> String)))

-- | The data which represents a typed event handler, hidden inside an
-- | existential package in the `Prop` type.
data HandlerF i fields = HandlerF (EventName fields) (Event fields -> EventHandler (Maybe i))

-- | Create an attribute
prop :: forall value i. IsProp value => PropName value -> Maybe AttrName -> value -> Prop i
prop name attr v = Prop (mkExists (PropF name v (flip Tuple toPropString <$> attr)))

-- | Create an event handler
handler :: forall fields i. EventName fields -> (Event fields -> EventHandler (Maybe i)) -> Prop i
handler name k = Handler (mkExistsR (HandlerF name k))

-- | This type class captures those property types which can be used as
-- | attribute values.
-- |
-- | `toPropString` is an alternative to `show`, and is needed by `renderAttr`
-- | in the string renderer.
class IsProp a where
  toPropString :: AttrName -> PropName a -> a -> String

instance stringIsProp :: IsProp String where
  toPropString _ _ s = s

instance intIsProp :: IsProp Int where
  toPropString _ _ i = show i

instance numberIsProp :: IsProp Number where
  toPropString _ _ n = show n

instance booleanIsProp :: IsProp Boolean where
  toPropString name _ true = runAttrName name
  toPropString _ _ false = ""

-- | A type-safe wrapper for a attribute or tag namespace.
newtype Namespace = Namespace String

-- | Create a namespace
namespace :: String -> Namespace
namespace = Namespace

-- | Unwrap a `Namespace` to get the value as a `String`.
runNamespace :: Namespace -> String
runNamespace (Namespace ns) = ns

-- | A type-safe wrapper for a HTML tag name
newtype TagName = TagName String

-- | Create a tag name
tagName :: String -> TagName
tagName = TagName

-- | Unwrap a `TagName` to get the tag name as a `String`.
runTagName :: TagName -> String
runTagName (TagName s) = s

-- | A type-safe wrapper for property names.
-- |
-- | The phantom type `value` describes the type of value which this property
-- | requires.
newtype PropName value = PropName String

-- | Create an attribute name
propName :: forall value. String -> PropName value
propName = PropName

-- | Unpack an attribute name
runPropName :: forall value. PropName value -> String
runPropName (PropName s) = s

-- | A type-safe wrapper for attribute names.
newtype AttrName = AttrName String

attrName :: String -> AttrName
attrName = AttrName

runAttrName :: AttrName -> String
runAttrName (AttrName ns) = ns

-- | A type-safe wrapper for event names.
-- |
-- | The phantom type `fields` describes the event type which we can expect to
-- | exist on events corresponding to this name.
newtype EventName (fields :: # *) = EventName String

-- Create an event name
eventName :: forall fields. String -> EventName fields
eventName = EventName

-- | Unpack an event name
runEventName :: forall fields. EventName fields -> String
runEventName (EventName s) = s

-- | A wrapper for strings which are used as CSS classes.
newtype ClassName = ClassName String

-- Create a class name
className :: String -> ClassName
className = ClassName

-- | Unpack a class name
runClassName :: ClassName -> String
runClassName (ClassName s) = s

instance htmlTree :: RenderDSL HTML where
  emptyTree = mkTree'
    { slot: unit
    , html: defer \_ -> Text ""
    , eq: \_ _ -> false
    , thunk: false
    }
  initialTree r = mkTree'
    { slot: unit
    , html: defer \_ -> unsafeCoerce r
    , eq: \_ _ -> false
    , thunk: false
    }
  mapTree = graftTree
  installChildren child c = go
    where
      go (Text s) st = Tuple (Text s) st
      go (Slot p) st = child Slot p st
      go (Element ns name props els) st = runPure $ runST do
        arr <- A.emptySTArray
        acc <- newSTRef st
        foreachE els \el -> do
          st' <- readSTRef acc
          case go el st' of
            Tuple el' st'' -> do
              void $ A.pushSTArray arr el'
              void $ writeSTRef acc st''
        acc' <- readSTRef acc
        pure $ Tuple (Element ns name (map c <$> props) $ unsafeFreeze arr) acc'

-- Prevents an unnecessary array copy
unsafeFreeze :: forall h a. A.STArray h a -> Array a
unsafeFreeze = unsafeCoerce
