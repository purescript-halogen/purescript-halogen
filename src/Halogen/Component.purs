module Halogen.Component
  ( ComponentP()
  , Component()
  , Render()
  , Eval()
  , component
  , RenderParent()
  , SlotConstructor(..)
  , EvalParent()
  , Peek()
  , parentComponent
  , parentComponent'
  , InstalledState()
  , installedState
  , ChildF(..)
  , QueryF()
  , mkQuery
  , mkQuery'
  , liftQuery
  , query
  , query'
  , interpret
  , transformChild
  , renderComponent
  , queryComponent
  , RenderP()
  , PeekP()
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
import Halogen.Query (HalogenF(), get, modify, hoistHalogenF)
import Halogen.Query.StateF (StateF(), mapState)
import Halogen.Query.SubscribeF (SubscribeF(), subscribeN, remapSubscribe, hoistSubscribe)

-- | Data type for Halogen components.
-- | - `s` - the component's state
-- | - `f` - the component's query algebra
-- | - `g` - a functor integrated into the component's query algebra that allows
-- |         embedding of external DSLs or handling of effects.
-- | - `o` - the type of values observable via `peek`, used to allow parent
-- |         components to see queries their children have acted upon.
-- | - `p` - the type of slot addresses within the component - these values
-- |         allow queries to child components to be specifically addressed.
newtype ComponentP s f g o p = Component
  { render :: State s (HTML p (f Unit))
  , eval   :: Eval f s f g
  , peek   :: PeekP o s f g
  }

instance functorComponent :: Functor (ComponentP s f g o) where
  map f (Component c) = Component { render: lmap f <$> c.render, eval: c.eval, peek: c.peek }

-- | A low level constructor for building components.
component' :: forall s f g o p. RenderP s f p -> Eval f s f g -> PeekP o s f g -> ComponentP s f g o p
component' r e p = Component { render: CMS.gets r, eval: e, peek: p }

-- | A type alias for self-contained Halogen components.
type Component s f g = ComponentP s f g (Const Void) Void

-- | A low level form of the `Render` and `RenderParent` synonyms, used
-- | internally.
type RenderP s f p = s -> HTML p (f Unit)

-- | A type alias for a component `render` function - takes the component's
-- | current state and returns a `HTML` value.
type Render s f = RenderP s f Void

-- | A type alias for a component `eval` function - takes a functorial value `i`
-- | and returns a `Free` of the Halogen component algebra.
-- |
-- | Usually `i` will be the same type as `f`, but sometimes it is useful to be
-- | able to break up an `Eval` function into different parts, in which case
-- | `i`'s type may differ.
type Eval i s f g = Natural i (Free (HalogenF s f g))

-- | Builds a self-contained component with no possible children.
component :: forall s f g. Render s f -> Eval f s f g -> Component s f g
component r e = component' r e (const (pure unit))

-- | A variation on `Render` for parent components - the function follows the
-- | same form but the type representation is different.
type RenderParent s s' f f' g p = RenderP s f (SlotConstructor s' f' g p)

-- | The type used for slots in the HTML rendered by parent components.
data SlotConstructor s' f' g p = SlotConstructor p (Unit -> { component :: Component s' f' g, initialState :: s' })

-- | A variation on `Eval` for parent components - the function follows the
-- | same form but the type representation is different.
type EvalParent i s s' f f' g p = Eval i s f (QueryF s s' f f' g p)

-- | A low level form of the `Peek` type synonym, used internally.
type PeekP i s f g = forall a. i a -> Free (HalogenF s f g) Unit

-- | A type alias for a component `peek` function that observes inputs to child
-- | components.
type Peek i s s' f f' g p = PeekP i s f (QueryF s s' f f' g p)

-- | Builds a component that may contain child components.
parentComponent :: forall s s' f f' g p
                 . (Plus g, Ord p)
                => RenderParent s s' f f' g p
                -> EvalParent f s s' f f' g p
                -> Component (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
parentComponent r e = Component { render: render r, eval: eval, peek: const (pure unit) }
  where
  eval :: Eval (Coproduct f (ChildF p f')) (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
  eval = coproduct (queryParent e) queryChild

-- | Builds a component that may contain child components and additionally
-- | supports the `peek` operation to allow the parent to observe queries that
-- | descendant components have processed.
parentComponent' :: forall s s' f f' g p
                  . (Plus g, Ord p)
                 => RenderParent s s' f f' g p
                 -> EvalParent f s s' f f' g p
                 -> Peek (ChildF p f') s s' f f' g p
                 -> Component (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
parentComponent' r e p = Component { render: render r, eval: eval, peek: const (pure unit) }
  where
  eval :: Eval (Coproduct f (ChildF p f')) (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
  eval = coproduct (queryParent e) (\q -> queryChild q <* peek q)

  peek :: PeekP (ChildF p f') (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
  peek =
    let runSubscribeF' = runSubscribeF (queryParent e)
    in foldFree (coproduct mergeParentStateF (coproduct runSubscribeF' liftChildF)) <<< p

-- | The type used by component containers for their state where `s` is the
-- | state local to the container, `p` is the type of slot used by the
-- | container, and the remaining parameters are the type variables for the
-- | child components.
type InstalledState s s' f f' g p =
  { parent   :: s
  , children :: M.Map p (Tuple (Component s' f' g) s')
  , memo     :: M.Map p (HTML Void (Coproduct f (ChildF p f') Unit))
  }

-- | Lifts a state value into an `InstalledState` value. Useful when providing
-- | an initial state value for a parent component.
installedState :: forall s s' f f' g p. (Ord p) => s -> InstalledState s s' f f' g p
installedState = { parent: _, children: M.empty, memo: M.empty }

-- | An intermediate algebra that parent components "produce" from their `eval`
-- | and `peek` functions. This takes the place of `g` when compared to a leaf
-- | (non-parent) component.
type QueryF s s' f f' g p = Free (HalogenF (InstalledState s s' f f' g p) (ChildF p f') g)

-- | An intermediate algebra used to associate values from a child component's
-- | algebra with the slot the component was installed into.
data ChildF p f i = ChildF p (f i)

instance functorChildF :: (Functor f) => Functor (ChildF p f) where
  map f (ChildF p fi) = ChildF p (f <$> fi)

-- | Queries a child component, for use within a parent component's `eval` or
-- | `peek` function.
query :: forall s s' f f' g p i. (Functor g, Ord p)
      => p
      -> f' i
      -> Free (HalogenF s f (QueryF s s' f f' g p)) (Maybe i)
query p q = liftQuery (mkQuery p q)

-- | A version of [`query`](#query) for use when a parent component has multiple
-- | types of child component.
query' :: forall s s' s'' f f' f'' g p p' i. (Functor g, Ord p')
       => ChildPath s s' f f' p p'
       -> p
       -> f i
       -> Free (HalogenF s'' f'' (QueryF s'' s' f'' f' g p')) (Maybe i)
query' i p q = liftQuery (mkQuery' i p q)

-- | Creates a query for a child component where `p` is the slot the component
-- | was installed into and `f' i` in the input query.
-- |
-- | If a component is not found for the slot the result of the query
-- | will be `Nothing`.
mkQuery :: forall s s' f f' p g i. (Functor g, Ord p)
      => p
      -> f' i
      -> QueryF s s' f f' g p (Maybe i)
mkQuery p q = do
  st <- get
  case M.lookup p st.children of
    Nothing -> pure Nothing
    Just (Tuple c _) -> Just <$> mapF (coproduct (left <<< mapStateFChild p) (right <<< coproduct (left <<< remapSubscribe (ChildF p)) right)) (queryComponent c q)

-- | A version of [`mkQuery`](#mkQuery) for use when a parent component has
-- | multiple types of child component.
mkQuery' :: forall s s' s'' f f' f'' g p p' i. (Functor g, Ord p')
         => ChildPath s s' f f' p p'
         -> p
         -> f i
         -> QueryF s'' s' f'' f' g p' (Maybe i)
mkQuery' i p q = mkQuery (injSlot i p) (injQuery i q)

-- | Lifts a value in the `QueryF` algebra into the monad used by a component's
-- | `eval` function.
liftQuery :: forall s s' f f' g p. (Functor g)
          => EvalParent (QueryF s s' f f' g p) s s' f f' g p
liftQuery qf = liftF (right (right qf))

mapStateFParent :: forall s s' f f' g p. Natural (StateF s) (StateF (InstalledState s s' f f' g p))
mapStateFParent = mapState (_.parent) (\f st -> st { parent = f st.parent })

mapStateFChild :: forall s s' f f' g p. (Ord p) => p -> Natural (StateF s') (StateF (InstalledState s s' f f' g p))
mapStateFChild p = mapState (\st -> U.fromJust $ snd <$> M.lookup p st.children)
                            (\f st -> { parent: st.parent, children: M.update (Just <<< rmap f) p st.children, memo: st.memo })

render :: forall s s' f f' g p. (Ord p)
       => (s -> HTML (SlotConstructor s' f' g p) (f Unit))
       -> State (InstalledState s s' f f' g p) (HTML Void ((Coproduct f (ChildF p f')) Unit))
render rc = do
    st <- CMS.get
    let html = rc st.parent
    -- Empty the state so that we don't keep children that are no longer
    -- being rendered...
    CMS.put { parent: st.parent, children: M.empty :: M.Map p (Tuple (Component s' f' g) s'), memo: M.empty :: M.Map p (HTML Void (Coproduct f (ChildF p f') Unit)) }
    -- ...but then pass through the old state so we can lookup child
    -- components that are being re-rendered
    fillSlot (renderChild st) left html

  where

  renderChild :: InstalledState s s' f f' g p
              -> SlotConstructor s' f' g p
              -> State (InstalledState s s' f f' g p) (HTML Void ((Coproduct f (ChildF p f')) Unit))
  renderChild st (SlotConstructor p def) =
    let childState = M.lookup p st.children
    in case M.lookup p st.memo of
      Just html -> do
        CMS.modify (\st' -> { parent: st'.parent, children: M.alter (const childState) p st'.children, memo: M.insert p html st'.memo } :: InstalledState s s' f f' g p)
        pure html
      Nothing -> case childState of
        Just (Tuple c s) -> renderChild' p c s
        Nothing ->
          let def' = def unit
          in renderChild' p def'.component def'.initialState

  renderChild' :: p
               -> Component s' f' g
               -> s'
               -> State (InstalledState s s' f f' g p) (HTML Void ((Coproduct f (ChildF p f')) Unit))
  renderChild' p c s = case renderComponent c s of
    Tuple html s' -> do
      CMS.modify (\st -> { parent: st.parent, children: M.insert p (Tuple c s') st.children, memo: st.memo } :: InstalledState s s' f f' g p)
      pure $ right <<< ChildF p <$> html

queryParent :: forall s s' f f' g p. (Functor g)
            => EvalParent f s s' f f' g p
            -> Eval f (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
queryParent f q = foldFree (coproduct mergeParentStateF (coproduct (runSubscribeF (queryParent f)) liftChildF)) (f q)

mergeParentStateF :: forall s s' f f' g p. Eval (StateF s) (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
mergeParentStateF = liftF <<< left <<< mapStateFParent

runSubscribeF :: forall s s' f f' g p. (Functor g)
              => Eval f (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
              -> Eval (SubscribeF f (Free (HalogenF (InstalledState s s' f f' g p) (ChildF p f') g))) (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
runSubscribeF queryParent' = subscribeN (forever $ lift <<< queryParent' =<< await) <<< hoistSubscribe liftChildF

liftChildF :: forall s s' f f' g p. (Functor g)
           => Eval (Free (HalogenF (InstalledState s s' f f' g p) (ChildF p f') g)) (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
liftChildF = mapF (coproduct left (right <<< coproduct (left <<< remapSubscribe right) right))

queryChild :: forall s s' f f' g p. (Plus g, Ord p)
           => Eval (ChildF p f') (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
queryChild (ChildF p q) = do
  modify (\st -> { parent: st.parent, children: st.children, memo: M.delete p st.memo })
  mapF (coproduct left (right <<< coproduct (left <<< remapSubscribe right) right)) (mkQuery p q) >>= maybe empty' pure
  where
  empty' :: forall s f g a. (Plus g) => Free (HalogenF s f g) a
  empty' = liftF (right (right empty))

adaptState :: forall s t m a. (Monad m) => (s -> t) -> (t -> s) -> CMS.StateT s m a -> CMS.StateT t m a
adaptState st ts (CMS.StateT f) = CMS.StateT \state -> f (ts state) >>= \(Tuple a s) -> pure $ Tuple a (st s)

transform' :: forall s s' f f' g
            . (Functor g)
           => (s -> s')
           -> (s' -> s)
           -> Natural f f'
           -> Natural f' f
           -> Component s f g
           -> Component s' f' g
transform' sTo sFrom fTo fFrom (Component c) =
  Component { render: map fTo <$> adaptState sTo sFrom c.render
            , eval: mapF natHF <<< c.eval <<< fFrom
            , peek: mapF natHF <<< c.peek
            }
  where
  natHF :: Natural (HalogenF s f g) (HalogenF s' f' g)
  natHF = coproduct (left <<< mapState sFrom (\f s -> sTo (f (sFrom s))))
                    (right <<< coproduct (left <<< remapSubscribe fTo) right)

-- | Transforms a `Component`'s types using a `ChildPath` definition.
transformChild :: forall s s' f f' g p p'
                . (Functor g)
               => ChildPath s s' f f' p p'
               -> Component s f g
               -> Component s' f' g
transformChild i = transform' (injState i)
                              (U.fromJust <<< prjState i)
                              (injQuery i)
                              (U.fromJust <<< prjQuery i)

-- | Changes the component's `g` type. A use case for this would be to interpret
-- | some `Free` monad as `Aff` so the component can be used with `runUI`.
interpret :: forall s f g g' o p
           . (Functor g')
          => Natural g g'
          -> ComponentP s f g o p
          -> ComponentP s f g' o p
interpret nat (Component c) =
  Component { render: c.render
            , eval: mapF (hoistHalogenF nat) <<< c.eval
            , peek: mapF (hoistHalogenF nat) <<< c.peek
            }

-- | Runs a component's `render` function with the specified state, returning
-- | the generated `HTML` and new state.
renderComponent :: forall s f g o p. ComponentP s f g o p -> s -> Tuple (HTML p (f Unit)) s
renderComponent (Component c) = runState c.render

-- | Runs a compnent's `query` function with the specified query input and
-- | returns the pending computation as a `Free` monad.
queryComponent :: forall s f g o p. ComponentP s f g o p -> Eval f s f g
queryComponent (Component c) = c.eval
