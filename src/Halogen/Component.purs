-- | This module defines a type of composable _components_.

module Halogen.Component 
  ( Component()
  , component
  , component'
  , runComponent
  , combine
  ) where
      
import Data.Void (Void(), absurd)
import Data.Either
import Data.Exists (Exists(), mkExists, runExists)
import Data.Bifunctor (Bifunctor, rmap)
import Data.Profunctor (Profunctor, dimap)

import Halogen.HTML (HTMLRepr)
import Halogen.Signal (SF1(), mergeWith')
      
-- | This will be hidden inside the existential package `Component`.
newtype ComponentF p m node req res i = ComponentF (SF1 (Either i req) (node p (m (Either i res))))
      
-- | A component.
-- | 
-- | The type parameters are, in order:
-- |
-- | - `p`, the type of _placeholders_
-- | - `m`, the monad used to track effects required by external requests
-- | - `node`, the type of node used to render content
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
newtype Component p m node req res = Component (Exists (ComponentF p m node req res))

-- | Create a component by providing a signal function.
-- |
-- | The signal function should consume external requests and produce DOM nodes. The DOM
-- | nodes in turn will create (monadic) external requests.
-- |
-- | See the `Halogen.Signal` documentation.
component :: forall p m node req res i. (Bifunctor node, Functor m) => SF1 req (node p (m res)) -> Component p m node req res
component sf = component' (dimap f (rmap (g <$>)) sf)
  where
  f :: Either Void req -> req
  f = either absurd id
  
  g :: res -> Either Void res
  g = Right

-- | A variant of `component` which creates a component with some internal, hidden input type.
component' :: forall p m node req res i. SF1 (Either i req) (node p (m (Either i res))) -> Component p m node req res
component' sf = Component (mkExists (ComponentF sf))

-- | Unpack a component.
-- |
-- | The rank-2 type ensures that the hidden message type must be used abstractly.
runComponent :: forall p m node req res r. (forall i. SF1 (Either i req) (node p (m (Either i res))) -> r) -> Component p m node req res -> r
runComponent f (Component e) = runExists (\(ComponentF sf) -> f sf) e

-- | Combine two components.
combine :: forall p m node req1 req2 res1 res2. 
             (Bifunctor node, Functor m) =>
             (forall a. node p a -> node p a -> node p a) -> 
             Component p m node req1 res1 -> 
             Component p m node req2 res2 -> 
             Component p m node (Either req1 req2) (Either res1 res2)
combine f = runComponent \sf1 -> runComponent \sf2 -> component' (mergeWith' f1 f2 sf1 sf2)
  where
  f1 :: forall i1 i2. Either (Either i1 i2) (Either req1 req2) -> Either (Either i1 req1) (Either i2 req2)
  f1 (Left (Left i1)) = Left (Left i1)
  f1 (Left (Right i2)) = Right (Left i2)
  f1 (Right (Left req1)) = Left (Right req1)
  f1 (Right (Right req2)) = Right (Right req2)
      
  f2 :: forall i1 i2. node p (m (Either i1 res1)) -> node p (m (Either i2 res2)) -> node p (m (Either (Either i1 i2) (Either res1 res2)))
  f2 n1 n2 = rmap (f3 <$>) (f (rmap (Left <$>) n1) (rmap (Right <$>) n2))

  f3 :: forall i1 i2. Either (Either i1 res1) (Either i2 res2) -> Either (Either i1 i2) (Either res1 res2)
  f3 (Left (Left i1)) = Left (Left i1)
  f3 (Right (Left i1)) = Left (Right i1)
  f3 (Left (Right res1)) = Right (Left res1)
  f3 (Right (Right res2)) = Right (Right res2)

instance profunctorComponent :: (Bifunctor node, Functor m) => Profunctor (Component p m node) where
  dimap f g = runComponent \sf -> component' (dimap (f <$>) (rmap ((g <$>) <$>)) sf)