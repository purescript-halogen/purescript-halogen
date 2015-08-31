module Halogen.Component
  ( ComponentP()
  , Component()
  , Render()
  , Eval()
  , Peek()
  , renderComponent
  , queryComponent
  , component
  , component'
  , liftEff'
  , ComponentStateP()
  , ComponentState()
  , InstalledStateP()
  , InstalledState()
  , ParentComponentP()
  , ParentComponent()
  , InstalledComponentP()
  , InstalledComponent()
  , installedState
  , ChildF(..)
  , QueryFP()
  , QueryF()
  , query
  , liftQuery
  , install
  , installL
  , installR
  , install'
  , installL'
  , installR'
  ) where

import Prelude

import Control.Apply ((<*))
import Control.Bind ((=<<))
import Control.Coroutine (await)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Control.Monad.Free (Free(), foldFree, liftF, mapF)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (State(), runState)
import Control.Monad.Trans (lift)
import Control.Plus (Plus, empty)
import qualified Control.Monad.State.Class as CMS

import Data.Bifunctor (lmap, rmap)
import Data.Const (Const())
import Data.Either (Either(..), either)
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Maybe (Maybe(..), maybe, fromMaybe')
import Data.NaturalTransformation (Natural())
import Data.Tuple (Tuple(..), snd)
import Data.Void (Void())
import qualified Data.Map as M
import qualified Data.Maybe.Unsafe as U

import Halogen.HTML.Core (HTML(..), substPlaceholder)
import Halogen.Query (HalogenF(), get)
import Halogen.Query.StateF (StateF(), mapState)
import Halogen.Query.SubscribeF (SubscribeF(), subscribeN, remapSubscribe, hoistSubscribe)

-- | Data type for Halogen components.
-- | - `s` - the type of the component state
-- | - `f` - the component's query algebra
-- | - `g` - the monad handling the component's non-state effects
-- | - `o` - the type of values observable via `peek`, used to allow parent
-- |         components to see queries their children have acted upon.
-- | - `p` - the type of placeholders within the component, used to specify
-- |         "holes" in which child components can be installed.
newtype ComponentP s f g o p = Component
  { render :: State s (HTML p (f Unit))
  , eval   :: Eval f s f g
  , peek   :: Peek s f g o
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

-- | A type alias for a component `peek` function that observes inputs to child
-- | components.
type Peek s f g o = forall a. o a -> Free (HalogenF s f g) Unit

-- | Runs a component's `render` function with the specified state, returning
-- | the generated `HTML` and new state.
renderComponent :: forall s f g o p. ComponentP s f g o p -> s -> Tuple (HTML p (f Unit)) s
renderComponent (Component c) = runState c.render

-- | Runs a compnent's `query` function with the specified query input and
-- | returns the pending computation as a `Free` monad.
queryComponent :: forall s f g o p. ComponentP s f g o p -> Eval f s f g
queryComponent (Component c) = c.eval

peekComponent :: forall s f g o p. ComponentP s f g o p -> Peek s f g o
peekComponent (Component c) = c.peek

-- | Builds a new [`ComponentP`](#componentp) from a [`Render`](#render),
-- | [`Eval`](#eval), and [`Peek`](#peek) function. This is used in cases where
-- | defining a parent component that needs to observe inputs to its children.
component' :: forall s f g o p. Render s f p -> Eval f s f g -> Peek s f g o -> ComponentP s f g o p
component' r q p = Component { render: CMS.gets r, eval: q, peek: p }

-- | Builds a new [`Component`](#component) from a [`Render`](#render) and
-- | [`Eval`](#eval) function.
component :: forall s f g p. Render s f p -> Eval f s f g -> Component s f g p
component r q = component' r q (const $ pure unit)

-- | Interacting with the DOM will usually be done via `Eff`, but we tend to
-- | operate in `Aff` when using Halogen, so this function helps lift an `Eff`
-- | action into an `Aff` or any other `MonadEff`-instance-providing-type.
-- |
-- | This is identical to `liftFI <<< liftEff`, but typed in such a way that
-- | code using `liftEff'` won't require decorating its usage with explicit
-- | type signatures.
liftEff' :: forall eff a s f g. (MonadEff eff g, Functor g) => Eff eff a -> Free (HalogenF s f g) a
liftEff' = liftF <<< right <<< right <<< liftEff

-- | A type synonym for a component combined with its state. Used when
-- | installing components to a component with initial state for a placeholder.
type ComponentStateP s f g o p = Tuple (ComponentP s f g o p) s
type ComponentState s f g p = ComponentStateP s f g (Const Void) p

-- | The type used by component containers for their state where `s` is the
-- | state local to the container, `p` is the type of placeholder used by the
-- | container, and the remaining parameters are the type variables for the
-- | child components.
type InstalledStateP s s' f' g o' p p' =
  { parent   :: s
  , children :: M.Map p (ComponentStateP s' f' g o' p')
  }
type InstalledState s s' f g p p' = InstalledStateP s s' f g (Const Void) p p'

-- | A type alias used to simplify the type signature for a `Component s f g p`
-- | that is intended to have components of type `Component s' f' g p'`
-- | installed into it.
type ParentComponentP s s' f f' g o o' p p' = ComponentP s f (QueryFP s s' f' g o' p p') o p
type ParentComponent s s' f f' g o' p p' = ParentComponentP s s' f f' g (Const Void) o' p p'

-- | A type alias use to simplify the type signature for a `Component s f g p`
-- | that has had components of type `Component s' f' g p'` installed into it.
type InstalledComponentP s s' f f' g o o' p p' = ComponentP (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g o p'
type InstalledComponent s s' f f' g o' p p' = InstalledComponentP s s' f f' g (Const Void) o' p p'

-- | Creates an initial `InstalledState` value for a component container based
-- | on a state value for the container.
installedState :: forall s s' f' g o' p p'. (Ord p) => s -> InstalledStateP s s' f' g o' p p'
installedState = { parent: _, children: M.empty }

mapStateFParent :: forall s s' f f' g o p p'. Natural (StateF s) (StateF (InstalledStateP s s' f' g o p p'))
mapStateFParent = mapState (_.parent) (\f st -> { parent: f st.parent, children: st.children })

mapStateFChild :: forall s s' f f' g o p p'. (Ord p) => p -> Natural (StateF s') (StateF (InstalledStateP s s' f' g o p p'))
mapStateFChild p = mapState (\st -> U.fromJust $ snd <$> M.lookup p st.children)
                            (\f st -> { parent: st.parent, children: M.update (Just <<< rmap f) p st.children })

-- | An intermediate algebra that component containers "produce" (use as their
-- | `g` type variable).
type QueryFP s s' f' g o' p p' = Free (HalogenF (InstalledStateP s s' f' g o' p p') (ChildF p f') g)
type QueryF s s' f' g p p' = QueryFP s s' f' g (Const Void) p p'

-- | An intermediate algebra used to associate values from a child component's
-- | algebra with the child component's placeholder when querying.
data ChildF p f i = ChildF p (f i)

instance functorChildF :: (Functor f) => Functor (ChildF p f) where
  map f (ChildF p fi) = ChildF p (f <$> fi)

-- | Creates a query for a child component where `p` is the placeholder
-- | addressing the component and `f' i` in the input query.
-- |
-- | If a component is not found for the placeholder the result of the query
-- | will be `Nothing`.
query :: forall s s' f' p p' o g i. (Functor g, Ord p)
      => p
      -> f' i
      -> QueryFP s s' f' g o p p' (Maybe i)
query p q = do
  st <- get
  case M.lookup p st.children of
    Nothing -> pure Nothing
    Just (Tuple c _) -> Just <$> mapF (coproduct (left <<< mapStateFChild p) (right <<< coproduct (left <<< remapSubscribe (ChildF p)) right)) (queryComponent c q)

-- | Lifts a value in the `QueryF` algebra into the monad used by a component's
-- | `eval` function.
liftQuery :: forall s s' f f' g o' p p'. (Functor g)
          => Eval (QueryFP s s' f' g o' p p') s f (QueryFP s s' f' g o' p p')
liftQuery qf = liftF (right (right qf))

installer :: forall s s' f f' g o' p p' q q'. (Plus g, Ord p)
          => (q -> Either p q')
          -> (p' -> q')
          -> Component s f (QueryFP s s' f' g o' p p') q
          -> (p -> ComponentStateP s' f' g o' p')
          -> Component (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g q'
installer fromQ toQ' c f = Component { render: render', eval: eval, peek: const (pure unit) }
  where

  render' :: State (InstalledStateP s s' f' g o' p p') (HTML q' ((Coproduct f (ChildF p f')) Unit))
  render' = render fromQ toQ' c f

  eval :: Eval (Coproduct f (ChildF p f')) (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g
  eval = coproduct (queryParent c) queryChild

install :: forall s s' f f' g o' p p'. (Plus g, Ord p)
       => ParentComponent s s' f f' g o' p p'
       -> (p -> ComponentStateP s' f' g o' p')
       -> InstalledComponent s s' f f' g o' p p'
install = installer Left id

installL :: forall s s' f f' g o' pl pr p'. (Plus g, Ord pl)
         => Component s f (QueryFP s s' f' g o' pl p') (Either pl pr)
         -> (pl -> ComponentStateP s' f' g o' p')
         -> Component (InstalledStateP s s' f' g o' pl p') (Coproduct f (ChildF pl f')) g (Either p' pr)
installL = installer (either Left (Right <<< Right)) Left

installR :: forall s s' f f' g o' pl pr p'. (Plus g, Ord pr)
         => Component s f (QueryFP s s' f' g o' pr p') (Either pl pr)
         -> (pr -> ComponentStateP s' f' g o' p')
         -> Component (InstalledStateP s s' f' g o' pr p') (Coproduct f (ChildF pr f')) g (Either pl p')
installR = installer (either (Right <<< Left) Left) Right

installer' :: forall s s' f f' g o' p p' q q'. (Plus g, Ord p)
           => (q -> Either p q')
           -> (p' -> q')
           -> ComponentP s f (QueryFP s s' f' g o' p p') (ChildF p f') q
           -> (p -> ComponentStateP s' f' g o' p')
           -> ComponentP (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g (ChildF p f') q'
installer' fromQ toQ' c f = Component { render: render', eval: eval, peek: peek }
  where

  render' :: State (InstalledStateP s s' f' g o' p p') (HTML q' ((Coproduct f (ChildF p f')) Unit))
  render' = render fromQ toQ' c f

  eval :: Eval (Coproduct f (ChildF p f')) (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g
  eval = coproduct (queryParent c) (\q -> queryChild q <* peek q)

  peek :: Peek (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g (ChildF p f')
  peek q =
    let runSubscribeF' = runSubscribeF (queryParent c)
    in foldFree (coproduct mergeParentStateF (coproduct runSubscribeF' liftChildF)) (peekComponent c q)

install' :: forall s s' f f' g o' p p'. (Plus g, Ord p)
        => ParentComponentP s s' f f' g (ChildF p f') o' p p'
        -> (p -> ComponentStateP s' f' g o' p')
        -> InstalledComponentP s s' f f' g (ChildF p f') o' p p'
install' = installer' Left id

installL' :: forall s s' f f' g o' pl pr p'. (Plus g, Ord pl)
          => ComponentP s f (QueryFP s s' f' g o' pl p') (ChildF pl f') (Either pl pr)
          -> (pl -> ComponentStateP s' f' g o' p')
          -> ComponentP (InstalledStateP s s' f' g o' pl p') (Coproduct f (ChildF pl f')) g (ChildF pl f') (Either p' pr)
installL' = installer' (either Left (Right <<< Right)) Left

installR' :: forall s s' f f' g o' pl pr p'. (Plus g, Ord pr)
          => ComponentP s f (QueryFP s s' f' g o' pr p') (ChildF pr f') (Either pl pr)
          -> (pr -> ComponentStateP s' f' g o' p')
          -> ComponentP (InstalledStateP s s' f' g o' pr p') (Coproduct f (ChildF pr f')) g (ChildF pr f') (Either pl p')
installR' = installer' (either (Right <<< Left) Left) Right

render :: forall s s' f f' g o o' p p' q q'. (Ord p)
       => (q -> Either p q')
       -> (p' -> q')
       -> ComponentP s f (QueryFP s s' f' g o' p p') o q
       -> (p -> ComponentStateP s' f' g o' p')
       -> State (InstalledStateP s s' f' g o' p p') (HTML q' ((Coproduct f (ChildF p f')) Unit))
render fromQ toQ' c f = do
    st <- CMS.get
    case renderComponent c st.parent of
      Tuple html s -> do
        -- Empty the state so that we don't keep children that are no longer
        -- being rendered...
        CMS.put { parent: s, children: M.empty :: M.Map p (ComponentStateP s' f' g o' p') }
        -- ...but then pass through the old state so we can lookup child
        -- components that are being re-rendered
        substPlaceholder (renderChild st) left html

  where

  renderChild :: InstalledStateP s s' f' g o' p p'
              -> q
              -> State (InstalledStateP s s' f' g o' p p') (HTML q' ((Coproduct f (ChildF p f')) Unit))
  renderChild st p = case fromQ p of
    Left p' -> renderChild' st p' $ fromMaybe' (\_ -> f p') (M.lookup p' st.children)
    Right p' -> pure $ Placeholder p'

  renderChild' :: InstalledStateP s s' f' g o' p p'
               -> p
               -> ComponentStateP s' f' g o' p'
               -> State (InstalledStateP s s' f' g o' p p') (HTML q' ((Coproduct f (ChildF p f')) Unit))
  renderChild' st p (Tuple c s) = case renderComponent c s of
    Tuple html s' -> do
      CMS.modify (\st -> st { children = M.insert p (Tuple c s') st.children })
      pure $ lmap toQ' $ right <<< ChildF p <$> html

queryParent :: forall s s' f f' g o o' p p' q. (Functor g)
            => ComponentP s f (QueryFP s s' f' g o' p p') o q
            -> Eval f (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g
queryParent c q = foldFree (coproduct mergeParentStateF (coproduct (runSubscribeF (queryParent c)) liftChildF)) (queryComponent c q)

mergeParentStateF :: forall s s' f f' g o' p p'. Eval (StateF s) (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g
mergeParentStateF = liftF <<< left <<< mapStateFParent

runSubscribeF :: forall s s' f f' g o' p p'. (Functor g)
              => Eval f (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g
              -> Eval (SubscribeF f (Free (HalogenF (InstalledStateP s s' f' g o' p p') (ChildF p f') g))) (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g
runSubscribeF queryParent' = subscribeN (forever $ lift <<< queryParent' =<< await) <<< hoistSubscribe liftChildF

liftChildF :: forall s s' f f' g o' p p'. (Functor g)
           => Eval (Free (HalogenF (InstalledStateP s s' f' g o' p p') (ChildF p f') g)) (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g
liftChildF = mapF (coproduct left (right <<< coproduct (left <<< remapSubscribe right) right))

queryChild :: forall s s' f f' g o' p p'. (Plus g, Ord p)
           => Eval (ChildF p f') (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g
queryChild (ChildF p q) = mapF (coproduct left (right <<< coproduct (left <<< remapSubscribe right) right)) (query p q) >>= maybe empty' pure

empty' :: forall f g h a. (Functor f, Functor g, Plus h) => Free (Coproduct f (Coproduct g h)) a
empty' = liftF $ right (right empty)
