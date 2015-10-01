module Halogen.Component
  ( ComponentP()
  , Component()
  , Render()
  , Eval()
  , EvalP()
  , Peek()
  , PeekP()
  , renderComponent
  , queryComponent
  , component
  , component'
  , ChildState()
  , createChild
  , createChild'
  , ParentComponent()
  , ParentComponentP()
  , InstalledComponent()
  , InstalledState()
  , installedState
  , ChildF(..)
  , QueryF()
  , mkQuery
  , mkQuery'
  , liftQuery
  , query
  , query'
  , install
  , install'
  ) where

import Prelude

import Control.Apply ((<*))
import Control.Bind ((=<<), (<=<), (>=>))
import Control.Coroutine (await)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Control.Monad.Free (Free(), foldFree, liftF, mapF)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (State(), runState)
import Control.Monad.Trans (lift)
import Control.Plus (Plus, empty)
import qualified Control.Monad.State.Class as CMS
import qualified Control.Monad.State.Trans as CMS

import Data.Bifunctor (bimap, lmap, rmap)
import Data.Const (Const())
import Data.Either (Either(..), either)
import Data.Functor.Coproduct (Coproduct(..), coproduct, left, right)
import Data.Maybe (Maybe(..), maybe, fromMaybe')
import Data.NaturalTransformation (Natural())
import Data.Tuple (Tuple(..), snd)
import Data.Void (Void())
import qualified Data.Either.Unsafe as U
import qualified Data.Map as M
import qualified Data.Maybe.Unsafe as U

import Halogen.Component.ChildPath (ChildPath(), injState, injQuery, injSlot, prjState, prjQuery, prjSlot)
import Halogen.HTML.Core (HTML(..), fillSlot)
import Halogen.Query (HalogenF(), get, modify)
import Halogen.Query.StateF (StateF(), mapState)
import Halogen.Query.SubscribeF (SubscribeF(), subscribeN, remapSubscribe, hoistSubscribe)

-- | Data type for Halogen components.
-- | - `s` - the type of the component state
-- | - `f` - the component's query algebra
-- | - `g` - the monad handling the component's non-state effects
-- | - `o` - the type of values observable via `peek`, used to allow parent
-- |         components to see queries their children have acted upon.
-- | - `p` - the type of slots within the component, used to specify locations
-- |         at which child components can be installed.
newtype ComponentP s f g o p = Component
  { render :: State s (HTML p (f Unit))
  , eval   :: Eval f s f g
  , peek   :: PeekP s f g o
  }

-- | A type alias for Halogen components where `peek` is not used.
type Component s f g = ComponentP s f g (Const Void)

instance functorComponent :: Functor (ComponentP s f g o) where
  map f (Component c) = Component { render: lmap f <$> c.render, eval: c.eval, peek: c.peek }

-- | A type alias for a component `render` function.
type Render s f p = s -> HTML p (f Unit)

-- | A type alias for a component `eval` function that takes a value from the
-- | component's query algebra and returns a `Free` monad of the Halogen
-- | component algebra.
type Eval i s f g = Natural i (Free (HalogenF s f g))

-- | A convenience variation on `Eval` for parent components.
type EvalP i s s' f f' g p p' = Eval i s f (QueryF s s' f f' g p p')

-- | A type alias for a component `peek` function that observes inputs to child
-- | components.
type Peek s s' f f' g p p' = PeekP s f (QueryF s s' f f' g p p') (ChildF p f')

-- | A lower level form of the `Peek` type synonym, used internally.
type PeekP s f g o = forall a. o a -> Free (HalogenF s f g) Unit

-- | Runs a component's `render` function with the specified state, returning
-- | the generated `HTML` and new state.
renderComponent :: forall s f g o p. ComponentP s f g o p -> s -> Tuple (HTML p (f Unit)) s
renderComponent (Component c) = runState c.render

-- | Runs a compnent's `query` function with the specified query input and
-- | returns the pending computation as a `Free` monad.
queryComponent :: forall s f g o p. ComponentP s f g o p -> Eval f s f g
queryComponent (Component c) = c.eval

-- | Runs a parent component's `peek` function.
peekComponent :: forall s f g o p. ComponentP s f g o p -> PeekP s f g o
peekComponent (Component c) = c.peek

-- | Builds a new [`Component`](#component) from a [`Render`](#render) and
-- | [`Eval`](#eval) function.
component :: forall s f g p. Render s f p -> Eval f s f g -> Component s f g p
component r q = Component { render: CMS.gets r, eval: q, peek: const (pure unit) }

-- | Builds a new [`ComponentP`](#componentp) from a [`Render`](#render),
-- | [`Eval`](#eval), and [`Peek`](#peek) function. This is used in cases where
-- | defining a parent component that needs to observe inputs to its children.
component' :: forall s s' f f' g p p'. Render s f p -> EvalP f s s' f f' g p p' -> Peek s s' f f' g p p' -> ParentComponentP s s' f f' g p p'
component' r q p = Component { render: CMS.gets r, eval: q, peek: p }

-- | A type synonym for a component combined with its state. This is used when
-- | installing components into slots.
type ChildState s f g p = Tuple (Component s f g p) s

-- | Creates a `ChildState` for a component.
createChild :: forall s f g p. Component s f g p -> s -> ChildState s f g p
createChild c s = Tuple c s

-- | Creates a `ChildState` for a component that is being installed into a
-- | parent with multiple different types of child component.
createChild' :: forall s s' f f' g p p' q. (Functor g) => ChildPath s s' f f' p p' -> Component s f g q -> s -> ChildState s' f' g q
createChild' i c s = Tuple (transform i c) (injState i s)

-- | A type alias used to simplify the type signature for a `Component s f g p`
-- | that is intended to have components of type `Component s' f' g p'`
-- | installed into it.
type ParentComponent s s' f f' g p p' = Component s f (QueryF s s' f f' g p p') p

-- | A type alias similar to `ParentComponent`, but for components that `peek`
-- | on their children.
type ParentComponentP s s' f f' g p p' = ComponentP s f (QueryF s s' f f' g p p') (ChildF p f') p

-- | A type alias use to simplify the type signature for a `Component s f g p`
-- | that has had components of type `Component s' f' g p'` installed into it.
type InstalledComponent s s' f f' g p p' = Component (InstalledState s s' f f' g p p') (Coproduct f (ChildF p f')) g p'

-- | The type used by component containers for their state where `s` is the
-- | state local to the container, `p` is the type of slot used by the
-- | container, and the remaining parameters are the type variables for the
-- | child components.
type InstalledState s s' f f' g p p' =
  { parent   :: s
  , children :: M.Map p (ChildState s' f' g p')
  , memo     :: M.Map p (HTML p' (Coproduct f (ChildF p f') Unit))
  }

-- | Creates an initial `InstalledState` value for a component container based
-- | on a state value for the container.
installedState :: forall s s' f f' g p p'. (Ord p) => s -> InstalledState s s' f f' g p p'
installedState = { parent: _, children: M.empty, memo: M.empty }

-- | An intermediate algebra that parent components "produce" from their `eval`
-- | and `peek` functions. This takes the place of `g` when compared to a leaf
-- | (non-parent) component.
type QueryF s s' f f' g p p' = Free (HalogenF (InstalledState s s' f f' g p p') (ChildF p f') g)

-- | An intermediate algebra used to associate values from a child component's
-- | algebra with the slot the component was installed into.
data ChildF p f i = ChildF p (f i)

instance functorChildF :: (Functor f) => Functor (ChildF p f) where
  map f (ChildF p fi) = ChildF p (f <$> fi)

-- | Queries a child component, for use within a parent component's `eval` or
-- | `peek` function.
query :: forall s s' f f' g p p' i. (Functor g, Ord p)
      => p
      -> f' i
      -> Free (HalogenF s f (QueryF s s' f f' g p p')) (Maybe i)
query p q = liftQuery (mkQuery p q)

-- | A version of [`query`](#query) for use when a parent component has multiple
-- | types of child component.
query' :: forall s s' s'' f f' f'' g p p' p'' i. (Functor g, Ord p')
       => ChildPath s s' f f' p p'
       -> p
       -> f i
       -> Free (HalogenF s'' f'' (QueryF s'' s' f'' f' g p' p'')) (Maybe i)
query' i p q = liftQuery (mkQuery' i p q)

-- | Creates a query for a child component where `p` is the slot the component
-- | was installed into and `f' i` in the input query.
-- |
-- | If a component is not found for the slot the result of the query
-- | will be `Nothing`.
mkQuery :: forall s s' f f' p p' g i. (Functor g, Ord p)
      => p
      -> f' i
      -> QueryF s s' f f' g p p' (Maybe i)
mkQuery p q = do
  st <- get
  case M.lookup p st.children of
    Nothing -> pure Nothing
    Just (Tuple c _) -> Just <$> mapF (coproduct (left <<< mapStateFChild p) (right <<< coproduct (left <<< remapSubscribe (ChildF p)) right)) (queryComponent c q)

-- | A version of [`mkQuery`](#mkQuery) for use when a parent component has
-- | multiple types of child component.
mkQuery' :: forall s s' s'' f f' f'' g p p' p'' i. (Functor g, Ord p')
         => ChildPath s s' f f' p p'
         -> p
         -> f i
         -> QueryF s'' s' f'' f' g p' p'' (Maybe i)
mkQuery' i p q = mkQuery (injSlot i p) (injQuery i q)

-- | Lifts a value in the `QueryF` algebra into the monad used by a component's
-- | `eval` function.
liftQuery :: forall s s' f f' g p p'. (Functor g)
          => EvalP (QueryF s s' f f' g p p') s s' f f' g p p'
liftQuery qf = liftF (right (right qf))

-- | Installs children into a parent component by using a function that produces
-- | `ChildState` values for a given slot.
install :: forall s s' f f' g p p'. (Plus g, Ord p)
        => ParentComponent s s' f f' g p p'
        -> (p -> ChildState s' f' g p')
        -> InstalledComponent s s' f f' g p p'
install c f = Component { render: render c f, eval: eval, peek: const (pure unit) }
  where
  eval :: Eval (Coproduct f (ChildF p f')) (InstalledState s s' f f' g p p') (Coproduct f (ChildF p f')) g
  eval = coproduct (queryParent c) queryChild

-- | A version of [`install`](#install) for use with parent components that
-- | `peek` on their children.
install' :: forall s s' f f' g p p'. (Plus g, Ord p)
        => ParentComponentP s s' f f' g p p'
        -> (p -> ChildState s' f' g p')
        -> InstalledComponent s s' f f' g p p'
install' c f = Component { render: render c f, eval: eval, peek: const (pure unit) }
  where
  eval :: Eval (Coproduct f (ChildF p f')) (InstalledState s s' f f' g p p') (Coproduct f (ChildF p f')) g
  eval = coproduct (queryParent c) (\q -> queryChild q <* peek q)

  peek :: PeekP (InstalledState s s' f f' g p p') (Coproduct f (ChildF p f')) g (ChildF p f')
  peek q =
    let runSubscribeF' = runSubscribeF (queryParent c)
    in foldFree (coproduct mergeParentStateF (coproduct runSubscribeF' liftChildF)) (peekComponent c q)

mapStateFParent :: forall s s' f f' g p p'. Natural (StateF s) (StateF (InstalledState s s' f f' g p p'))
mapStateFParent = mapState (_.parent) (\f st -> st { parent = f st.parent })

mapStateFChild :: forall s s' f f' g p p'. (Ord p) => p -> Natural (StateF s') (StateF (InstalledState s s' f f' g p p'))
mapStateFChild p = mapState (\st -> U.fromJust $ snd <$> M.lookup p st.children)
                            (\f st -> { parent: st.parent, children: M.update (Just <<< rmap f) p st.children, memo: st.memo })

render :: forall s s' f f' g o p p'. (Ord p)
       => ComponentP s f (QueryF s s' f f' g p p') o p
       -> (p -> ChildState s' f' g p')
       -> State (InstalledState s s' f f' g p p') (HTML p' ((Coproduct f (ChildF p f')) Unit))
render c f = do
    st <- CMS.get
    case renderComponent c st.parent of
      Tuple html s -> do
        -- Empty the state so that we don't keep children that are no longer
        -- being rendered...
        CMS.put { parent: s, children: M.empty :: M.Map p (ChildState s' f' g p'), memo: M.empty :: M.Map p (HTML p' (Coproduct f (ChildF p f') Unit)) }
        -- ...but then pass through the old state so we can lookup child
        -- components that are being re-rendered
        fillSlot (renderChild st) left html

  where

  renderChild :: InstalledState s s' f f' g p p'
              -> p
              -> State (InstalledState s s' f f' g p p') (HTML p' ((Coproduct f (ChildF p f')) Unit))
  renderChild st p =
    let childState = M.lookup p st.children
    in case M.lookup p st.memo of
      Just html -> do
        CMS.modify (\st' -> { parent: st'.parent, children: M.alter (const childState) p st'.children, memo: M.insert p html st'.memo } :: InstalledState s s' f f' g p p')
        pure html
      Nothing -> renderChild' p $ fromMaybe' (\_ -> f p) childState

  renderChild' :: p
               -> ChildState s' f' g p'
               -> State (InstalledState s s' f f' g p p') (HTML p' ((Coproduct f (ChildF p f')) Unit))
  renderChild' p (Tuple c s) = case renderComponent c s of
    Tuple html s' -> do
      CMS.modify (\st -> { parent: st.parent, children: M.insert p (Tuple c s') st.children, memo: st.memo } :: InstalledState s s' f f' g p p')
      pure $ right <<< ChildF p <$> html

queryParent :: forall s s' f f' g o p p' q. (Functor g)
            => ComponentP s f (QueryF s s' f f' g p p') o q
            -> Eval f (InstalledState s s' f f' g p p') (Coproduct f (ChildF p f')) g
queryParent c q = foldFree (coproduct mergeParentStateF (coproduct (runSubscribeF (queryParent c)) liftChildF)) (queryComponent c q)

mergeParentStateF :: forall s s' f f' g p p'. Eval (StateF s) (InstalledState s s' f f' g p p') (Coproduct f (ChildF p f')) g
mergeParentStateF = liftF <<< left <<< mapStateFParent

runSubscribeF :: forall s s' f f' g p p'. (Functor g)
              => Eval f (InstalledState s s' f f' g p p') (Coproduct f (ChildF p f')) g
              -> Eval (SubscribeF f (Free (HalogenF (InstalledState s s' f f' g p p') (ChildF p f') g))) (InstalledState s s' f f' g p p') (Coproduct f (ChildF p f')) g
runSubscribeF queryParent' = subscribeN (forever $ lift <<< queryParent' =<< await) <<< hoistSubscribe liftChildF

liftChildF :: forall s s' f f' g p p'. (Functor g)
           => Eval (Free (HalogenF (InstalledState s s' f f' g p p') (ChildF p f') g)) (InstalledState s s' f f' g p p') (Coproduct f (ChildF p f')) g
liftChildF = mapF (coproduct left (right <<< coproduct (left <<< remapSubscribe right) right))

queryChild :: forall s s' f f' g p p'. (Plus g, Ord p)
           => Eval (ChildF p f') (InstalledState s s' f f' g p p') (Coproduct f (ChildF p f')) g
queryChild (ChildF p q) = do
  modify (\st -> { parent: st.parent, children: st.children, memo: M.delete p st.memo })
  mapF (coproduct left (right <<< coproduct (left <<< remapSubscribe right) right)) (mkQuery p q) >>= maybe empty' pure
  where
  empty' :: forall s f g a. (Plus g) => Free (HalogenF s f g) a
  empty' = liftF (right (right empty))

adaptState :: forall s t m a. (Monad m) => (s -> t) -> (t -> s) -> CMS.StateT s m a -> CMS.StateT t m a
adaptState st ts (CMS.StateT f) = CMS.StateT \state -> f (ts state) >>= \(Tuple a s) -> pure $ Tuple a (st s)

transform' :: forall s s' f f' g o o' p p'. (Functor g)
           => (s -> s')
           -> (s' -> s)
           -> Natural f f'
           -> Natural f' f
           -> Natural o' o
           -> (p -> p')
           -> ComponentP s f g o p
           -> ComponentP s' f' g o' p'
transform' sTo sFrom fTo fFrom oFrom tp (Component c) =
  Component { render: bimap tp fTo <$> adaptState sTo sFrom c.render
            , eval: mapF natHF <<< c.eval <<< fFrom
            , peek: mapF natHF <<< c.peek <<< oFrom
            }
  where
  natHF :: Natural (HalogenF s f g) (HalogenF s' f' g)
  natHF = coproduct (left <<< mapState sFrom (\f s -> sTo (f (sFrom s))))
                    (right <<< coproduct (left <<< remapSubscribe fTo) right)

transform :: forall s s' f f' g p p' q. (Functor g)
          => ChildPath s s' f f' p p'
          -> Component s f g q
          -> Component s' f' g q
transform i = transform' (injState i) (U.fromJust <<< prjState i) (injQuery i) (U.fromJust <<< prjQuery i) id id
