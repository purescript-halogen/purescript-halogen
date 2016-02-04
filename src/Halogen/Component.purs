module Halogen.Component
  ( Component()
  , ComponentHTML()
  , ComponentSpec()
  , Render()
  , ComponentDSL()
  , Eval()
  , component
  , componentSpec
  , ParentHTML()
  , ParentQuery()
  , RenderParent()
  , SlotConstructor(..)
  , ParentDSL()
  , EvalParent()
  , Peek()
  , parentComponent
  , parentComponentSpec
  , parentComponent'
  , parentComponentSpec'
  , InstalledState()
  , installedState
  , ChildF(..)
  , QueryF()
  , mkQuery
  , mkQuery'
  , liftQuery
  , query
  , query'
  , transform
  , transformChild
  , interpret
  , renderComponent
  , queryComponent
  , initializeComponent
  , finalizeComponent
  ) where

import Prelude

import Control.Apply ((<*))
import Control.Bind ((=<<))
import Control.Monad.Free (Free(), foldFree, liftF, mapF)
import Control.Monad.Free.Trans as FT
import Control.Monad.State (State(), runState)
import Control.Monad.State.Trans as CMS
import Control.Monad.Writer.Trans (WriterT(), runWriterT)
import Control.Monad.Writer.Trans as CMW

import Data.Bifunctor (lmap, rmap)
import Data.Foldable (foldMap, for_)
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.Unsafe as U
import Data.NaturalTransformation (Natural())
import Data.Traversable (for)
import Data.Tuple (Tuple(..), snd)
import Data.Void (Void())

import Halogen.Component.ChildPath (ChildPath(), injState, injQuery, injSlot, prjState, prjQuery)
import Halogen.Component.Hook (Hook(..), Finalized(), finalized, mapFinalized, lmapHook, rmapHook)
import Halogen.HTML.Core (HTML(..), fillSlot)
import Halogen.Query (get, modify, liftH, Action())
import Halogen.Query.EventSource (EventSource(..), ParentEventSource(), runEventSource, fromParentEventSource)
import Halogen.Query.HalogenF (HalogenF(), HalogenFP(..), hoistHalogenF, transformHF)
import Halogen.Query.StateF (StateF(..), mapState)

-- | Data type for Halogen components.
-- | - `s` - the component's state
-- | - `f` - the component's query algebra
-- | - `g` - a functor integrated into the component's query algebra that allows
-- |         embedding of external DSLs or handling of effects.
newtype Component s f g = Component
  { render :: RenderM s f g (ComponentHTML f)
  , eval :: Eval f s f g
  , initializer :: Maybe (f Unit)
  , finalizers :: s -> Array (Finalized g)
  }

-- | The type for the internal render monad. Rendering returns HTML, a new
-- | state, and input hooks to run post-render.
type RenderM s f g = WriterT (Array (Hook f g)) (State s)

-- | The type for `HTML` rendered by a self-contained component.
type ComponentHTML f = HTML Void (f Unit)

-- | A type alias for a component `render` function - takes the component's
-- | current state and returns a `HTML` value.
type Render s f = s -> ComponentHTML f

-- | The DSL used in the `eval` function for self-contained components.
type ComponentDSL s f g = Free (HalogenF s f g)

-- | A type alias for a component `eval` function - takes a functorial value `i`
-- | and returns a `Free` of the Halogen component algebra.
-- |
-- | Usually `i` will be the same type as `f`, but sometimes it is useful to be
-- | able to break up an `Eval` function into different parts, in which case
-- | `i`'s type may differ.
type Eval i s f g = Natural i (ComponentDSL s f g)

-- | Builds a self-contained component with no possible children.
component :: forall s f g. Render s f -> Eval f s f g -> Component s f g
component r e = componentSpec { render: r, eval: e, initializer: Nothing, finalizer: Nothing }

-- | A full spec for a component, including lifecycle inputs.
type ComponentSpec s f g =
  { render :: Render s f
  , eval :: Eval f s f g
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

-- | Builds a self-contained component with no possible children that may have
-- | lifecycle hooks.
componentSpec :: forall s f g. ComponentSpec s f g -> Component s f g
componentSpec spec =
  Component
    { render: CMS.gets spec.render
    , eval: spec.eval
    , initializer: spec.initializer
    , finalizers: finalizers
    }
  where
  finalizers :: s -> Array (Finalized g)
  finalizers s =
    case spec.finalizer of
      Just i -> [ finalized spec.eval s i ]
      _      -> []

-- | The type for `HTML` rendered by a parent component.
type ParentHTML s' f f' g p = HTML (SlotConstructor s' f' g p) (f Unit)

-- | The type for nested queries.
type ParentQuery f f' p = Coproduct f (ChildF p f')

-- | A variation on `Render` for parent components - the function follows the
-- | same form but the type representation is different.
type RenderParent s s' f f' g p = s -> ParentHTML s' f f' g p

-- | The type used for slots in the HTML rendered by parent components.
data SlotConstructor s' f' g p = SlotConstructor p (Unit -> { component :: Component s' f' g, initialState :: s' })

-- | The DSL used in the `eval` and `peek` functions for parent components.
type ParentDSL s s' f f' g p = Free (HalogenFP ParentEventSource s f (QueryF s s' f f' g p))

-- | A variation on `Eval` for parent components - the function follows the
-- | same form but the type representation is different.
type EvalParent i s s' f f' g p = Natural i (ParentDSL s s' f f' g p)

-- | A type alias for a component `peek` function that observes inputs to child
-- | components.
type Peek i s s' f f' g p = forall a. i a -> ParentDSL s s' f f' g p Unit

-- | Builds a component that may contain child components.
parentComponent
  :: forall s s' f f' g p
   . (Functor g, Ord p)
  => RenderParent s s' f f' g p
  -> EvalParent f s s' f f' g p
  -> Component (InstalledState s s' f f' g p) (ParentQuery f f' p) g
parentComponent r e = parentComponentSpec { render: r, eval: e, initializer: Nothing, finalizer: Nothing }

-- | A full spec for a parent component, including lifecycle inputs.
type ParentComponentSpec s s' f f' g p =
  { render :: RenderParent s s' f f' g p
  , eval :: EvalParent f s s' f f' g p
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

-- | Builds a component that may contain child components and have lifcycle hooks.
parentComponentSpec
  :: forall s s' f f' g p
   . (Functor g, Ord p)
  => ParentComponentSpec s s' f f' g p
  -> Component (InstalledState s s' f f' g p) (ParentQuery f f' p) g
parentComponentSpec spec =
  Component
    { render: renderParent spec.render
    , eval: eval
    , initializer: left <$> spec.initializer
    , finalizers: parentFinalizers eval spec.finalizer
    }
  where
  eval :: Eval (ParentQuery f f' p) (InstalledState s s' f f' g p) (ParentQuery f f' p) g
  eval = coproduct (queryParent spec.eval) queryChild

-- | Builds a component that may contain child components and additionally
-- | supports the `peek` operation to allow the parent to observe queries that
-- | descendant components have processed.
parentComponent'
  :: forall s s' f f' g p
   . (Functor g, Ord p)
  => RenderParent s s' f f' g p
  -> EvalParent f s s' f f' g p
  -> Peek (ChildF p f') s s' f f' g p
  -> Component (InstalledState s s' f f' g p) (ParentQuery f f' p) g
parentComponent' r e p = parentComponentSpec' { render: r, eval: e, peek: p, initializer: Nothing, finalizer: Nothing }

-- | A full spec for a parent component, including lifecycle inputs and a
-- | `peek` operation.
type ParentComponentSpecP s s' f f' g p =
  { render :: RenderParent s s' f f' g p
  , eval :: EvalParent f s s' f f' g p
  , peek :: Peek (ChildF p f') s s' f f' g p
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

-- | Builds a component that may contain child components, peek, and
-- | lifecycle hooks.
parentComponentSpec'
  :: forall s s' f f' g p
   . (Functor g, Ord p)
  => ParentComponentSpecP s s' f f' g p
  -> Component (InstalledState s s' f f' g p) (ParentQuery f f' p) g
parentComponentSpec' spec = Component
  { render: renderParent spec.render
  , eval: eval
  , initializer: left <$> spec.initializer
  , finalizers: parentFinalizers eval spec.finalizer
  }
  where
  eval :: Eval (ParentQuery f f' p) (InstalledState s s' f f' g p) (ParentQuery f f' p) g
  eval = coproduct (queryParent spec.eval) \q -> queryChild q <* queryParent spec.peek q

parentFinalizers
  :: forall s s' f f' g p
   . Eval (ParentQuery f f' p) (InstalledState s s' f f' g p) (ParentQuery f f' p) g
  -> Maybe (f Unit)
  -> InstalledState s s' f f' g p
  -> Array (Finalized g)
parentFinalizers eval fin (InstalledState s) =
  foldMap childFin s.children <> parentFin
  where
  parentFin :: Array (Finalized g)
  parentFin =
    case fin of
      Just i -> [ finalized eval (InstalledState s { children = M.empty, memo = M.empty }) (left i) ]
      _      -> []

  childFin :: Tuple (Component s' f' g) s' -> Array (Finalized g)
  childFin (Tuple (Component c) s) = c.finalizers s

-- | The type used by component containers for their state where `s` is the
-- | state local to the container, `p` is the type of slot used by the
-- | container, and the remaining parameters are the type variables for the
-- | child components.
newtype InstalledState s s' f f' g p = InstalledState
  { parent :: s
  , children :: M.Map p (Tuple (Component s' f' g) s')
  , memo :: M.Map p (ComponentHTML (ParentQuery f f' p))
  }

-- | Lifts a state value into an `InstalledState` value. Useful when providing
-- | an initial state value for a parent component.
installedState :: forall s s' f f' g p. (Ord p) => s -> InstalledState s s' f f' g p
installedState st = InstalledState { parent: st, children: M.empty, memo: M.empty }

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
query
  :: forall s s' f f' g p i
   . (Functor g, Ord p)
  => p
  -> f' i
  -> Free (HalogenFP ParentEventSource s f (QueryF s s' f f' g p)) (Maybe i)
query p q = liftQuery (mkQuery p q)

-- | A version of [`query`](#query) for use when a parent component has multiple
-- | types of child component.
query'
  :: forall s s' s'' f f' f'' g p p' i
   . (Functor g, Ord p')
  => ChildPath s s' f f' p p'
  -> p
  -> f i
  -> Free (HalogenFP ParentEventSource s'' f'' (QueryF s'' s' f'' f' g p')) (Maybe i)
query' i p q = liftQuery (mkQuery' i p q)

-- | Creates a query for a child component where `p` is the slot the component
-- | was installed into and `f' i` in the input query.
-- |
-- | If a component is not found for the slot the result of the query
-- | will be `Nothing`.
mkQuery
  :: forall s s' f f' p g i
   . (Functor g, Ord p)
  => p
  -> f' i
  -> QueryF s s' f f' g p (Maybe i)
mkQuery p q = do
  InstalledState st <- get
  for (M.lookup p st.children) \(Tuple c _) ->
    mapF (transformHF (mapStateFChild p) (ChildF p) id) (queryComponent c q)

-- | A version of [`mkQuery`](#mkQuery) for use when a parent component has
-- | multiple types of child component.
mkQuery'
  :: forall s s' s'' f f' f'' g p p' i
   . (Functor g, Ord p')
  => ChildPath s s' f f' p p'
  -> p
  -> f i
  -> QueryF s'' s' f'' f' g p' (Maybe i)
mkQuery' i p q = mkQuery (injSlot i p) (injQuery i q)

-- | Lifts a value in the `QueryF` algebra into the monad used by a component's
-- | `eval` function.
liftQuery
  :: forall s s' f f' g p
   . (Functor g)
  => Natural (QueryF s s' f f' g p) (ParentDSL s s' f f' g p)
liftQuery = liftH

mapStateFParent :: forall s s' f f' g p. Natural (StateF s) (StateF (InstalledState s s' f f' g p))
mapStateFParent =
  mapState
    (\(InstalledState st) -> st.parent)
    (\f (InstalledState st) -> InstalledState
      { parent: f st.parent
      , children: st.children
      , memo: st.memo
      })

mapStateFChild :: forall s s' f f' g p. (Ord p) => p -> Natural (StateF s') (StateF (InstalledState s s' f f' g p))
mapStateFChild p =
  mapState
    (\(InstalledState st) -> U.fromJust $ snd <$> M.lookup p st.children)
    (\f (InstalledState st) -> InstalledState
      { parent: st.parent
      , children: M.update (Just <<< rmap f) p st.children
      , memo: st.memo
      })

renderParent
  :: forall s s' f f' g p
   . (Ord p)
  => RenderParent s s' f f' g p
  -> RenderM (InstalledState s s' f f' g p) (ParentQuery f f' p) g (ComponentHTML (ParentQuery f f' p))
renderParent render = do
    InstalledState st <- CMS.get
    let html = render st.parent
    -- Empty the state so that we don't keep children that are no longer
    -- being rendered...
    CMS.put $ InstalledState
      { parent: st.parent
      , children: M.empty
      , memo: M.empty
      } :: InstalledState s s' f f' g p
    -- ...but then pass through the old state so we can lookup child
    -- components that are being re-rendered
    html' <- fillSlot (renderChild (InstalledState st)) left html
    (InstalledState st') :: InstalledState s s' f f' g p <- CMS.get
    for_ (M.toList st.children) \(Tuple k (Tuple c s)) ->
      if not (M.member k st'.children)
        then
          let hooks :: Array (Hook (ParentQuery f f' p) g)
              hooks = Finalized <$> finalizeComponent c s
          in CMW.tell hooks
        else
          pure unit
    pure html'

  where

  renderChild
    :: InstalledState s s' f f' g p
    -> SlotConstructor s' f' g p
    -> RenderM (InstalledState s s' f f' g p) (ParentQuery f f' p) g (ComponentHTML (ParentQuery f f' p))
  renderChild (InstalledState st) (SlotConstructor p def) =
    let childState = M.lookup p st.children
    in case M.lookup p st.memo of
      Just html -> do
        CMS.modify \(InstalledState st') -> InstalledState
          { parent: st'.parent
          , children: M.alter (const childState) p st'.children
          , memo: M.insert p html st'.memo
          } :: InstalledState s s' f f' g p
        pure html
      Nothing -> case childState of
        Just (Tuple c s) -> renderChild' p c s
        Nothing -> do
          let def' = def unit
          case initializeComponent def'.component of
            Nothing -> pure unit
            Just init ->
              let hook :: Hook (Coproduct f (ChildF p f')) g
                  hook = PostRender (right (ChildF p init))
              in CMW.tell [ hook ]
          renderChild' p def'.component def'.initialState

  renderChild'
    :: p
    -> Component s' f' g
    -> s'
    -> RenderM (InstalledState s s' f f' g p) (ParentQuery f f' p) g (ComponentHTML (ParentQuery f f' p))
  renderChild' p c s = do
    let r = renderComponent c s
        html = adapt <$> r.html

        adapt :: Natural f' (Coproduct f (ChildF p f'))
        adapt a = right (ChildF p a)

        hooks :: Array (Hook (Coproduct f (ChildF p f')) g)
        hooks = lmapHook adapt <$> r.hooks

    CMW.tell hooks
    CMS.modify \(InstalledState st) -> InstalledState
      { parent: st.parent
      , children: M.insert p (Tuple c r.state) st.children
      , memo: M.insert p html st.memo
      } :: InstalledState s s' f f' g p
    pure html

queryParent
  :: forall s s' f f' g p a q r. (Functor g)
  => (q a -> ParentDSL s s' f f' g p r)
  -> q a
  -> ComponentDSL (InstalledState s s' f f' g p) (ParentQuery f f' p) g r
queryParent f =
  f >>> foldFree \h ->
    case h of
      StateHF q -> mergeParentStateF q
      SubscribeHF es next ->
        liftF $ SubscribeHF (EventSource (FT.interpret (lmap left) (runEventSource (fromParentEventSource es)))) next
      QueryHF q -> liftChildF q
      HaltHF -> liftF HaltHF

mergeParentStateF :: forall s s' f f' g p. Natural (StateF s) (ComponentDSL (InstalledState s s' f f' g p) (ParentQuery f f' p) g)
mergeParentStateF = liftF <<< StateHF <<< mapStateFParent

liftChildF
  :: forall s s' f f' g p
   . (Functor g)
  => Natural (Free (HalogenF (InstalledState s s' f f' g p) (ChildF p f') g)) (ComponentDSL (InstalledState s s' f f' g p) (ParentQuery f f' p) g)
liftChildF = mapF (transformHF id right id)

queryChild
  :: forall s s' f f' g p
   . (Functor g, Ord p)
  => Natural (ChildF p f') (ComponentDSL (InstalledState s s' f f' g p) (ParentQuery f f' p) g)
queryChild (ChildF p q) = do
  modify \(InstalledState st) -> InstalledState
    { parent: st.parent
    , children: st.children
    , memo: M.delete p st.memo
    }
  mapF (transformHF id right id) (mkQuery p q)
    >>= maybe (liftF HaltHF) pure

-- | Transforms a `Component`'s types using partial mapping functions.
-- |
-- | If the initial state provided to the component fails the transformation an
-- | empty component will be rendered. If either of the transformations fail the
-- | component will "halt" (evaluate to `empty`), so care must be taken when
-- | handling transformed components to ensure they receive the intended query
-- | values and initial state type.
-- |
-- | Halogen itself will never cause a `transform`ed component to halt; this
-- | situation will only arise when the initial state is incorrect or a bad
-- | externally constructed query is passed to the component.
transform
  :: forall s s' f f' g
   . (Functor g)
  => (s -> s')
  -> (s' -> Maybe s)
  -> (forall a. f a -> f' a)
  -> (forall a. f' a -> Maybe (f a))
  -> Component s f g
  -> Component s' f' g
transform reviewS previewS reviewQ previewQ (Component c) =
  Component
    { render: maybe (pure $ Text "") render' <<< previewS =<< CMS.get
    , eval: maybe (liftF HaltHF) (foldFree go <<< c.eval) <<< previewQ
    , initializer: reviewQ <$> c.initializer
    , finalizers: maybe [] c.finalizers <<< previewS
    }
  where

  render' :: s -> RenderM s' f' g (HTML Void (f' Unit))
  render' st =
    CMW.WriterT $ CMS.StateT \_ ->
      case runState (runWriterT c.render) st of
        Tuple (Tuple html hooks) st' ->
          pure $ Tuple (Tuple (rmap reviewQ html) (lmapHook reviewQ <$> hooks)) (reviewS st')

  go :: Natural (HalogenF s f g) (Free (HalogenF s' f' g))
  go (StateHF (Get k)) =
    liftF <<< maybe HaltHF (\st' -> StateHF (Get (k <<< const st'))) <<< previewS =<< get
  go (StateHF (Modify f next)) = liftF $ StateHF (Modify (modifyState f) next)
  go (SubscribeHF es next) = liftF $ SubscribeHF (EventSource (FT.interpret (lmap reviewQ) (runEventSource es))) next
  go (QueryHF q) = liftF $ QueryHF q
  go HaltHF = liftF HaltHF

  modifyState :: (s -> s) -> s' -> s'
  modifyState f s' = maybe s' (reviewS <<< f) (previewS s')

-- | Transforms a `Component`'s types using a `ChildPath` definition.
transformChild
  :: forall s s' f f' g p p'
   . (Functor g)
  => ChildPath s s' f f' p p'
  -> Component s f g
  -> Component s' f' g
transformChild i = transform (injState i) (prjState i) (injQuery i) (prjQuery i)

-- | Changes the component's `g` type. A use case for this would be to interpret
-- | some `Free` monad as `Aff` so the component can be used with `runUI`.
interpret
  :: forall s f g g'
   . (Functor g')
  => Natural g g'
  -> Component s f g
  -> Component s f g'
interpret nat (Component c) =
  Component
    { render: render'
    , eval: mapF (hoistHalogenF nat) <<< c.eval
    , initializer: c.initializer
    , finalizers: map (mapFinalized nat) <$> c.finalizers
    }
  where
  render' :: RenderM s f g' (ComponentHTML f)
  render' =
    CMW.WriterT $ CMS.StateT \s ->
      case runState (runWriterT c.render) s of
        Tuple (Tuple html hooks) s' ->
          pure $ Tuple (Tuple html (rmapHook nat <$> hooks)) s'

-- | Runs a component's `render` function with the specified state, returning
-- | the generated `HTML` and new state.
renderComponent
  :: forall s f g
   . Component s f g
  -> s
  -> { html :: HTML Void (f Unit)
     , hooks :: Array (Hook f g)
     , state :: s
     }
renderComponent (Component c) s =
  case runState (runWriterT c.render) s of
    Tuple (Tuple html hooks) state -> { html: html, hooks: hooks, state: state }

-- | Runs a compnent's `query` function with the specified query input and
-- | returns the pending computation as a `Free` monad.
queryComponent :: forall s f g. Component s f g -> Eval f s f g
queryComponent (Component c) = c.eval

initializeComponent :: forall s f g. Component s f g -> Maybe (f Unit)
initializeComponent (Component c) = c.initializer

finalizeComponent :: forall s f g. Component s f g -> s -> Array (Finalized g)
finalizeComponent (Component c) = c.finalizers
