-- | This module defines a type of composable _components_.

module Halogen.Component 
  ( Component()
  , runComponent
  
  , component
  , component'
  
  , install
  , combine
  , hide
  
  , widget
  
  , mapP
  , hoistComponent
  ) where

import Data.DOM.Simple.Types

import Data.Int
import Data.Maybe      
import Data.Void (Void(), absurd)
import Data.Either
import Data.Exists (Exists(), mkExists, runExists)
import Data.Bifunctor (Bifunctor, lmap, rmap)
import Data.Profunctor (Profunctor, dimap)

import Control.Monad.Eff

import Halogen.HTML (HTML(), placeholder, graft)
import Halogen.Signal (SF(), SF1(), mergeWith', stateful, startingAt, input, tail)
import Halogen.Internal.VirtualDOM (Widget())

import qualified Halogen.HTML.Widget as W
      
-- | This will be hidden inside the existential package `Component`.
newtype ComponentF p m req res i = ComponentF (SF1 (Either i req) (HTML p (m (Either i res))))
      
-- | A component.
-- | 
-- | The type parameters are, in order:
-- |
-- | - `p`, the type of _placeholders_
-- | - `m`, the monad used to track effects required by external requests
-- | - `req`, the type of external requests
-- | - `res`, the type of external responses
-- | 
-- | Request and response types are public, but the component may also use an _internal_ type
-- | of messages, as illustrated by the type of the `component` function.
-- |
-- | The main interface to Halogen is the `runUI` function, which takes a component as an argument,
-- | with certain constraints between the type arguments. This module leaves the type arguments
-- | unrestricted, allowing components to be composed in various ways.
-- |
-- | If you do not use a particular feature (e.g. placeholders, requests), you might like to leave 
-- | the corresponding type parameter unconstrained in the declaration of your component. 
newtype Component p m req res = Component (Exists (ComponentF p m req res))

-- | Create a component by providing a signal function.
-- |
-- | The signal function should consume external requests and produce DOM nodes. The DOM
-- | nodes in turn will create (monadic) external requests.
-- |
-- | See the `Halogen.Signal` documentation.
component :: forall p m req res. (Functor m) => SF1 req (HTML p (m res)) -> Component p m req res
component sf = component' (dimap f (rmap (g <$>)) sf)
  where
  f :: Either Void req -> req
  f = either absurd id
  
  g :: res -> Either Void res
  g = Right

-- | A variant of `component` which creates a component with some internal, hidden input type.
component' :: forall p m req res i. SF1 (Either i req) (HTML p (m (Either i res))) -> Component p m req res
component' sf = Component (mkExists (ComponentF sf))

-- | Construct a `Component` from a third-party widget.
-- |
-- | The function argument is a record with the following properties:
-- |
-- | - `name` - the type of the widget, required by `virtual-dom` to distinguish different
-- |   types of widget.
-- | - `id` - a unique ID which belongs to this instance of the widget type, required by 
-- |   `virtual-dom` to distinguish widgets from each other.
-- | - `init` - an action which initializes the component and returns the `HTMLElement` it corresponds
-- |   to in the DOM. This action receives the driver function for the component so that it can
-- |   generate events. It can also create a piece of state of type `s` which is shared with the
-- |   other lifecycle functions.
-- | - `update` - Update the widget based on an input message.
-- | - `destroy` - Release any resources associated with the widget as it is about to be removed
-- |   from the DOM.
widget :: forall eff req res s m. 
  (Functor m) => 
  { name    :: String
  , id      :: String
  , init    :: (res -> Eff eff Unit) -> Eff eff { state :: s, node :: HTMLElement }
  , update  :: req -> s -> HTMLElement -> Eff eff (Maybe HTMLElement)
  , destroy :: s -> HTMLElement -> Eff eff Unit
  } -> 
  Component (Widget eff res) m req res
widget spec = component (placeholder <$> ((updateWith <$> input <*> version) `startingAt` w0))
  where
  w0 :: Widget eff res
  w0 = buildWidget zero (\_ _ -> return Nothing)
      
  updateWith :: req -> Int -> Widget eff res
  updateWith i n = buildWidget n (spec.update i)
  
  buildWidget :: Int -> (s -> HTMLElement -> Eff eff (Maybe HTMLElement)) -> Widget eff res
  buildWidget ref update = W.widget
    { ref: ref
    , name: spec.name
    , id: spec.id
    , init: spec.init
    , update: update
    , destroy: spec.destroy 
    } 
    
  version :: forall i. SF i Int
  version = tail $ stateful zero (\i _ -> i + one)
  
-- | Map a function over the placeholders in a component          
mapP :: forall p q m req res. (p -> q) -> Component p m req res -> Component q m req res
mapP f = runComponent \sf -> component' ((lmap f) <$> sf)

-- | Map a natural transformation over the monad type argument of a `Component`.
-- |
-- | This function may be useful during testing, to mock requests with a different monad.
hoistComponent :: forall p m n req res. (forall a. m a -> n a) -> Component p m req res -> Component p n req res
hoistComponent f = runComponent \sf -> component' ((rmap f) <$> sf)
    
-- | Unpack a component.
-- |
-- | The rank-2 type ensures that the hidden message type must be used abstractly.
runComponent :: forall p m req res r. (forall i. SF1 (Either i req) (HTML p (m (Either i res))) -> r) -> Component p m req res -> r
runComponent f (Component e) = runExists (\(ComponentF sf) -> f sf) e

-- | Install a component inside another, by replacing a placeholder.
-- |
-- | The placeholders labelled with `Nothing` in the second component will be replaced with the
-- | first component. Placeholders labelled with `Just` will remain as placeholders.
install :: forall p m req1 req2 res1 res2.
             (Functor m) => 
             Component p         m req1 res1 -> 
             Component (Maybe p) m req2 res2 ->
             Component p         m (Either req1 req2) (Either res1 res2)
install = combine render
  where
  render :: forall a. HTML p a -> HTML (Maybe p) a -> HTML p a
  render doc1 doc2 = graft doc2 (maybe doc1 placeholder)

-- | Hide some of the internal messages in a `Component`.
hide :: forall p m i req res. (Functor m) => Component p m (Either i req) (Either i res) -> Component p m req res
hide = runComponent \sf1 -> component' (dimap g ((f <$>) <$>) sf1)
  where
  f :: forall a b c. Either a (Either b c) -> Either (Either a b) c
  f (Left a) = Left (Left a)
  f (Right (Left b)) = Left (Right b)
  f (Right (Right c)) = Right c
  
  g :: forall a b c. Either (Either a b) c -> Either a (Either b c)
  g (Left (Left a)) = Left a
  g (Left (Right b)) = Right (Left b)
  g (Right c) = Right (Right c)

-- | Combine two components into a single component.
-- |
-- | The first argument is a function which combines the two rendered HTML documents into a single document.
-- |
-- | This function works on request and response types by taking the _sum_ in each component. The left summand
-- | gets dispatched to (resp. is generated by) the first component, and the right summand to the second component.
combine :: forall p q r m req1 req2 res1 res2. 
             (Functor m) =>
             (forall a. HTML p a -> HTML q a -> HTML r a) -> 
             Component p m req1 res1 -> 
             Component q m req2 res2 -> 
             Component r m (Either req1 req2) (Either res1 res2)
combine f = runComponent \sf1 -> runComponent \sf2 -> component' (mergeWith' f1 f2 sf1 sf2)
  where
  f1 :: forall i1 i2. Either (Either i1 i2) (Either req1 req2) -> Either (Either i1 req1) (Either i2 req2)
  f1 (Left (Left i1)) = Left (Left i1)
  f1 (Left (Right i2)) = Right (Left i2)
  f1 (Right (Left req1)) = Left (Right req1)
  f1 (Right (Right req2)) = Right (Right req2)
      
  f2 :: forall i1 i2. HTML p (m (Either i1 res1)) -> HTML q (m (Either i2 res2)) -> HTML r (m (Either (Either i1 i2) (Either res1 res2)))
  f2 n1 n2 = rmap (f3 <$>) (f (rmap (Left <$>) n1) (rmap (Right <$>) n2))

  f3 :: forall i1 i2. Either (Either i1 res1) (Either i2 res2) -> Either (Either i1 i2) (Either res1 res2)
  f3 (Left (Left i1)) = Left (Left i1)
  f3 (Right (Left i1)) = Left (Right i1)
  f3 (Left (Right res1)) = Right (Left res1)
  f3 (Right (Right res2)) = Right (Right res2)

instance functorComponent :: (Functor m) => Functor (Component p m req) where
  (<$>) = Data.Profunctor.rmap
  
instance profunctorComponent :: (Functor m) => Profunctor (Component p m) where
  dimap f g = runComponent \sf -> component' (dimap (f <$>) (rmap ((g <$>) <$>)) sf)