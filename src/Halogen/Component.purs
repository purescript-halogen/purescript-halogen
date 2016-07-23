module Halogen.Component
  ( Component
  , ComponentRendered
  , ComponentHTML
  , ComponentDSL
  , RawComponentSpec
  , RawComponent
  , ComponentSpec
  , component
  , RawLifecycleComponentSpec
  , LifecycleComponentSpec
  , lifecycleComponent
  , ParentRendered
  , ParentHTML
  , RawSlotConstructor(..)
  , SlotConstructor
  , ParentQuery
  , RawParentDSL
  , ParentDSL
  , RawParentComponentSpec
  , ParentComponentSpec
  , parentComponent
  , RawLifecycleParentComponentSpec
  , LifecycleParentComponentSpec
  , lifecycleParentComponent
  , RawParentState
  , ParentState
  , parentState
  , ChildF(..)
  , runChildF
  , RawQueryF
  , QueryF
  , mkQuery
  , mkQuery'
  , mkQueries
  , mkQueries'
  , liftQuery
  , query
  , query'
  , queryAll
  , queryAll'
  , childSlots
  , transform
  , transformChild
  , interpret
  , renderComponent
  , queryComponent
  , initializeComponent
  , finalizeComponent
  ) where

import Prelude
import Control.Monad.Free.Trans as FT
import Data.List as L
import Data.Map as M
import Control.Monad.Free (Free, foldFree, liftF, hoistFree)
import Data.Array (cons) as A
import Data.Bifunctor (lmap)
import Data.Foldable (foldMap, for_)
import Data.Functor.Coproduct (Coproduct, coproduct, left, right)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..), maybe, fromJust)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Halogen.Component.ChildPath (ChildPath(..), injState, injQuery, injSlot, prjState, prjQuery, prjSlot)
import Halogen.Component.Hook (Hook(..), Finalized, finalized, mapFinalized, lmapHook, rmapHook)
import Halogen.Component.Tree (Tree, mkTree, thunkTree)
import Halogen.HTML.Core (HTML)
import Halogen.Query (get, liftH)
import Halogen.Query.EventSource (EventSource(..), ParentEventSource, runEventSource, fromParentEventSource)
import Halogen.Query.HalogenF (HalogenF, HalogenFP(..), RenderPending(..), hoistHalogenF, transformHF)
import Halogen.Query.StateF (StateF(..), mapState)
import Halogen.RenderDSL (class RenderDSL, mapTree, installChildren, initialTree, emptyTree)
import Partial.Unsafe (unsafePartial)

type RenderResult h s f g =
  { state :: s
  , hooks :: Array (Hook f g)
  , tree  :: Tree h f Unit
  }

type Component s f g = RawComponent HTML s f g
type ComponentHTML f = ComponentRendered HTML f
type ParentState s s' f f' g p = RawParentState HTML s s' f f' g p
type ChildState s' f f' g p = RawChildState HTML s' f f' g p
type LifecycleComponentSpec s f g = RawLifecycleComponentSpec HTML s f g
type ComponentSpec s f g = RawComponentSpec HTML s f g
type ParentHTML s' f f' g p = ParentRendered HTML s' f f' g p
type SlotConstructor s' f' g p = RawSlotConstructor HTML s' f' g p
type ParentDSL s s' f f' g p = RawParentDSL HTML s s' f f' g p

emptyResult :: forall h s f g. RenderDSL h => s -> RenderResult h s f g
emptyResult state =
  { state
  , hooks: []
  , tree: emptyTree
  }

-- | Data type for Halogen components.
-- | - `s` - the component's state
-- | - `f` - the component's query algebra
-- | - `g` - a functor integrated into the component's query algebra that allows
-- |         embedding of external DSLs or handling of effects.
newtype RawComponent h s f g = RawComponent
  { render :: s -> RenderResult h s f g
  , eval :: f ~> (ComponentDSL s f g)
  , initializer :: Maybe (f Unit)
  , finalizers :: s -> Array (Finalized g)
  }

-- | The type for `HTML` rendered by a self-contained component.
type ComponentRendered h f = h Void (f Unit)

-- | The DSL used in the `eval` function for self-contained components.
type ComponentDSL s f g = Free (HalogenF s f g)

-- | A spec for a HTML component.
type RawComponentSpec h s f g =
  { render :: s -> ComponentRendered h f
  , eval :: f ~> ComponentDSL s f g
  }

-- | Builds a self-contained component with no possible children.
component :: forall h s f g. (RenderDSL h) => RawComponentSpec h s f g -> RawComponent h s f g
component spec =
  lifecycleComponent
    { render: spec.render
    , eval: spec.eval
    , initializer: Nothing
    , finalizer: Nothing
    }

-- | A spec for a component, including lifecycle inputs.
type RawLifecycleComponentSpec h s f g =
  { render :: s -> ComponentRendered h f
  , eval :: f ~> ComponentDSL s f g
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }


-- | Builds a self-contained component with lifecycle inputs and no possible
-- | children.
lifecycleComponent :: forall h s f g. RenderDSL h => RawLifecycleComponentSpec h s f g -> RawComponent h s f g
lifecycleComponent spec =
    RawComponent { render: \s -> { state: s, hooks: [], tree: renderTree (spec.render s) }
    , eval: spec.eval
    , initializer: spec.initializer
    , finalizers: \s -> maybe [] (\i -> [finalized spec.eval s i]) spec.finalizer
    }
  where
  renderTree :: ComponentRendered h f -> Tree h f Unit
  renderTree html = initialTree html

-- | The type for `HTML` rendered by a parent component.
type ParentRendered h s' f f' g p = h (RawSlotConstructor h s' f' g p) (f Unit)

-- | The type used for slots in the HTML rendered by parent components.
data RawSlotConstructor h s' f' g p = RawSlotConstructor p (Unit -> { component :: RawComponent h s' f' g, initialState :: s' })

-- | The type for nested queries.
type ParentQuery f f' p = Coproduct f (ChildF p f')

-- | The DSL used in the `eval` and `peek` functions for parent components.
type RawParentDSL h s s' f f' g p = Free (HalogenFP ParentEventSource s f (RawQueryF h s s' f f' g p))

type RawParentComponentSpec h s s' f f' g p =
  { render :: s -> ParentRendered h s' f f' g p
  , eval :: f ~> RawParentDSL h s s' f f' g p
  , peek :: forall x. Maybe (ChildF p f' x -> RawParentDSL h s s' f f' g p Unit)
  }

-- | A full spec for a parent component.
type ParentComponentSpec s s' f f' g p = RawParentComponentSpec HTML s s' f f' g p

-- | Builds a component that may contain child components.
parentComponent
  :: forall h s s' f f' g p
   . (Functor g, Ord p, RenderDSL h)
  => RawParentComponentSpec h s s' f f' g p
  -> RawComponent h (RawParentState h s s' f f' g p) (ParentQuery f f' p) g
parentComponent spec =
  lifecycleParentComponent
    { render: spec.render
    , eval: spec.eval
    , peek: spec.peek
    , initializer: Nothing
    , finalizer: Nothing
    }

type RawLifecycleParentComponentSpec h s s' f f' g p =
  { render :: s -> ParentRendered h s' f f' g p
  , eval :: f ~> RawParentDSL h s s' f f' g p
  , peek :: forall x. Maybe (ChildF p f' x -> RawParentDSL h s s' f f' g p Unit)
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

-- | A full spec for a parent component, including lifecycle inputs.
type LifecycleParentComponentSpec s s' f f' g p = RawLifecycleParentComponentSpec HTML s s' f f' g p

-- | Builds a component with lifecycle inputs that may contain child components.
lifecycleParentComponent
  :: forall h s s' f f' g p
   . (Functor g, Ord p, RenderDSL h)
  => RawLifecycleParentComponentSpec h s s' f f' g p
  -> RawComponent h (RawParentState h s s' f f' g p) (ParentQuery f f' p) g
lifecycleParentComponent spec =
    RawComponent
    { render: renderParent spec.render
    , eval: eval
    , initializer: left <$> spec.initializer
    , finalizers: parentFinalizers eval spec.finalizer
    }
  where
  eval :: ParentQuery f f' p ~> ComponentDSL (RawParentState h s s' f f' g p) (ParentQuery f f' p) g
  eval = coproduct (queryParent spec.eval) case spec.peek of
    Nothing -> queryChild
    Just peek -> \q -> queryChild q <* queryParent peek q

parentFinalizers
  :: forall h s s' f f' g p
   . (ParentQuery f f' p ~> ComponentDSL (RawParentState h s s' f f' g p) (ParentQuery f f' p) g)
  -> Maybe (f Unit)
  -> RawParentState h s s' f f' g p
  -> Array (Finalized g)
parentFinalizers eval fin (ParentState s) =
  foldMap childFin s.children <> foldMap parentFin fin
  where
  parentFin :: f Unit -> Array (Finalized g)
  parentFin i = [ finalized eval (parentState s.parent) (left i) ]

  childFin :: RawChildState h s' f f' g p -> Array (Finalized g)
  childFin child = finalizeComponent child.component child.state

-- | The type used by component containers for their state where `s` is the
-- | state local to the container, `p` is the type of slot used by the
-- | container, and the remaining parameters are the type variables for the
-- | child components.
newtype RawParentState h s s' f f' g p = ParentState
  { parent :: s
  , children :: M.Map p (RawChildState h s' f f' g p)
  }

type RawChildState h s' f f' g p =
    { component :: RawComponent h s' f' g
    , state :: s'
    , memo :: Maybe (Tree h (ParentQuery f f' p) p)
    }

-- | Lifts a state value into an `ParentState` value. Useful when providing
-- | an initial state value for a parent component.
parentState :: forall h s s' f f' g p. s -> RawParentState h s s' f f' g p
parentState st = ParentState { parent: st, children: M.empty }

type RawQueryF h s s' f f' g p = Free (HalogenF (RawParentState h s s' f f' g p) (ChildF p f') g)
-- | An intermediate algebra that parent components "produce" from their `eval`
-- | and `peek` functions. This takes the place of `g` when compared to a leaf
-- | (non-parent) component.
type QueryF s s' f f' g p = RawQueryF HTML s s' f f' g p

-- | An intermediate algebra used to associate values from a child component's
-- | algebra with the slot the component was installed into.
data ChildF p f i = ChildF p (f i)

-- | Extracts the query part from a ChildF value, discarding the slot value.
runChildF :: forall p f i. ChildF p f i -> f i
runChildF (ChildF _ q) = q

instance functorChildF :: Functor f => Functor (ChildF p f) where
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

-- | Queries every child component that is currently installed. For use within
-- | a parent component's `eval` or `peek` function.
queryAll
  :: forall s s' f f' g p i
   . (Functor g, Ord p)
  => f' i
  -> Free (HalogenFP ParentEventSource s f (QueryF s s' f f' g p)) (M.Map p i)
queryAll q = liftQuery (mkQueries q)

-- | Returns slots of all currently installed child components.
childSlots
  :: forall s s' f f' p g
  . (Functor g, Ord p)
  => QueryF s s' f f' g p (L.List p)
childSlots = do
  ParentState st <- get
  pure (M.keys st.children)

-- | A version of [`queryAll](#queryAll) for use when a parent component has
-- | multiple types of child component.
queryAll'
  :: forall s s' s'' f f' f'' g p p' i
   . (Functor g, Ord p, Ord p')
  => ChildPath s s' f f' p p'
  -> f i
  -> Free (HalogenFP ParentEventSource s'' f'' (QueryF s'' s' f'' f' g p')) (M.Map p i)
queryAll' i q = liftQuery (mkQueries' i q)

bracketQuery :: forall s f g a. ComponentDSL s f g a -> ComponentDSL s f g a
bracketQuery f = do
  rp <- liftF (RenderPendingHF id)
  case rp of
    Just Pending -> liftF (RenderHF Nothing unit)
    _ -> pure unit
  res <- f
  rp' <- liftF (RenderPendingHF id)
  for_ rp \_ -> liftF (RenderHF (Just Deferred) unit)
  pure res

-- | Creates a query for a child component where `p` is the slot the component
-- | was installed into and `f' i` in the input query.
-- |
-- | If a component is not found for the slot the result of the query
-- | will be `Nothing`.
mkQuery
  :: forall h s s' f f' p g i
   . (Functor g, Ord p)
  => p
  -> f' i
  -> RawQueryF h s s' f f' g p (Maybe i)
mkQuery p q = bracketQuery do
  ParentState st <- get
  for (M.lookup p st.children) \child ->
    hoistFree (transformHF (mapStateFChild p) (ChildF p) id) (queryComponent child.component q)

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

-- | Creates a query for every child component that is currently installed.
mkQueries
  :: forall s s' f f' p g i
   . (Functor g, Ord p)
  => f' i
  -> QueryF s s' f f' g p (M.Map p i)
mkQueries = mkQueries' (ChildPath id id id)

-- | A version of [`mkQueries](#mkQueries) for use when a parent component has
-- | multiple types of child component.
mkQueries'
  :: forall s s' s'' f f' f'' g p p' i
   . (Functor g, Ord p', Ord p)
  => ChildPath s s' f f' p p'
  -> f i
  -> QueryF s'' s' f'' f' g p' (M.Map p i)
mkQueries' i q = bracketQuery do
  ParentState st <- get
  M.fromList <<< L.catMaybes <$> traverse mkChildQuery (M.toList st.children)
  where
  mkChildQuery (Tuple p' child) =
    for (prjSlot i p')
        \p -> Tuple p <$> hoistFree (transformHF (mapStateFChild p') (ChildF p') id)
                               (queryComponent child.component (injQuery i q))

-- | Lifts a value in the `QueryF` algebra into the monad used by a component's
-- | `eval` function.
liftQuery
  :: forall s s' f f' g p
   . QueryF s s' f f' g p
  ~> ParentDSL s s' f f' g p
liftQuery = liftH

mapStateFParent :: forall h s s' f f' g p. StateF s ~> StateF (RawParentState h s s' f f' g p)
mapStateFParent =
  mapState
    (\(ParentState st) -> st.parent)
    (\f (ParentState st) -> ParentState
      { parent: f st.parent
      , children: st.children
      })

mapStateFChild :: forall h s s' f f' g p. Ord p => p -> StateF s' ~> StateF (RawParentState h s s' f f' g p)
mapStateFChild p =
  mapState
    (\(ParentState st) -> unsafePartial fromJust $ _.state <$> M.lookup p st.children)
    (\f (ParentState st) -> ParentState
      { parent: st.parent
      , children: M.update (\child -> Just { component: child.component, state: f child.state, memo: Nothing }) p st.children
      })


renderParent
  :: forall h s s' f f' g p
   . (Ord p, RenderDSL h)
  => (s -> ParentRendered h s' f f' g p)
  -> (RawParentState h s s' f f' g p)
  -> RenderResult h (RawParentState h s s' f f' g p) (ParentQuery f f' p) g
renderParent render (ParentState curr) =
  case installChildren installChild left (render curr.parent) init of
    Tuple html acc ->
      { state: ParentState { parent: curr.parent, children: acc.children }
      , hooks: foldMap finalizeChild acc.removed <> acc.hooks
      , tree:  mkTree $ defer \_ -> html
      }

  where

  init =
    { children: M.empty
    , removed: curr.children
    , hooks: []
    }

  finalizeChild child =
    map Finalized $ finalizeComponent child.component child.state

  installChild slot (RawSlotConstructor p def) { hooks, removed, children } =
    case M.lookup p curr.children of
      Just child' ->
        renderChild Nothing child'

      Nothing ->
        let def' = def unit
            hook = PostRender <$> initializeComponent def'.component
        in
            renderChild hook
              { component: def'.component
              , state: def'.initialState
              , memo: Nothing
              }
    where
    update hs c =
      { children: M.insert p c children
      , removed: M.delete p removed
      , hooks: hooks <> hs
      }

    renderChild _ c@{ memo : Just tree } =
      Tuple (slot $ thunkTree tree) (update [] c)

    renderChild hook c =
      let r = renderComponent c.component c.state
          tree = mapTree adapt (const p) r.tree
          hooks' = lmapHook adapt <$> maybe r.hooks (flip A.cons r.hooks) hook

          adapt :: f' ~> Coproduct f (ChildF p f')
          adapt a = right (ChildF p a)
      in
          Tuple (slot tree) $ update hooks'
            { component: c.component
            , state: r.state
            , memo: Just tree
            }

queryParent
  :: forall h s s' f f' g p a q r
   . (Functor g)
  => (q a -> RawParentDSL h s s' f f' g p r)
  -> q a
  -> ComponentDSL (RawParentState h s s' f f' g p) (ParentQuery f f' p) g r
queryParent f =
  f >>> foldFree \h ->
    case h of
      StateHF q -> mergeParentStateF q
      SubscribeHF es next ->
        liftF $ SubscribeHF (EventSource (FT.interpret (lmap left) (runEventSource (fromParentEventSource es)))) next
      QueryHF q -> liftChildF q
      RenderHF p a -> liftF $ RenderHF p a
      RenderPendingHF k -> liftF $ RenderPendingHF k
      HaltHF -> liftF HaltHF

mergeParentStateF :: forall h s s' f f' g p. StateF s ~> ComponentDSL (RawParentState h s s' f f' g p) (ParentQuery f f' p) g
mergeParentStateF = liftF <<< StateHF <<< mapStateFParent

liftChildF
  :: forall h s s' f f' g p
   . (Functor g)
  => Free (HalogenF (RawParentState h s s' f f' g p) (ChildF p f') g)
  ~> ComponentDSL (RawParentState h s s' f f' g p) (ParentQuery f f' p) g
liftChildF = hoistFree (transformHF id right id)

queryChild
  :: forall h s s' f f' g p
   . (Functor g, Ord p)
  => ChildF p f'
  ~> ComponentDSL (RawParentState h s s' f f' g p) (ParentQuery f f' p) g
queryChild (ChildF p q) =
  hoistFree (transformHF id right id) (mkQuery p q)
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
  :: forall h s s' f f' g
   . (Functor g, RenderDSL h)
  => (s -> s')
  -> (s' -> Maybe s)
  -> (f ~> f')
  -> (forall a. f' a -> Maybe (f a))
  -> RawComponent h s f g
  -> RawComponent h s' f' g
transform reviewS previewS reviewQ previewQ (RawComponent c) =
    RawComponent
    { render: \st -> maybe (emptyResult st) render' (previewS st)
    , eval: maybe (liftF HaltHF) (foldFree go <<< c.eval) <<< previewQ
    , initializer: reviewQ <$> c.initializer
    , finalizers: maybe [] c.finalizers <<< previewS
    }
  where

  render' :: s -> RenderResult h s' f' g
  render' st =
    case c.render st of
      { state, hooks, tree } ->
        { state: reviewS state
        , hooks: lmapHook reviewQ <$> hooks
        , tree: mapTree reviewQ id tree
        }

  go :: HalogenF s f g ~> Free (HalogenF s' f' g)
  go (StateHF (Get k)) =
    liftF <<< maybe HaltHF (\st' -> StateHF (Get (k <<< const st'))) <<< previewS =<< get
  go (StateHF (Modify f next)) = liftF $ StateHF (Modify (modifyState f) next)
  go (SubscribeHF es next) = liftF $ SubscribeHF (EventSource (FT.interpret (lmap reviewQ) (runEventSource es))) next
  go (QueryHF q) = liftF $ QueryHF q
  go (RenderHF p a) = liftF $ RenderHF p a
  go (RenderPendingHF k) = liftF $ RenderPendingHF k
  go HaltHF = liftF HaltHF

  modifyState :: (s -> s) -> s' -> s'
  modifyState f s' = maybe s' (reviewS <<< f) (previewS s')

-- | Transforms a `Component`'s types using a `ChildPath` definition.
transformChild
  :: forall h s s' f f' g p p'
   . (Functor g, RenderDSL h)
  => ChildPath s s' f f' p p'
  -> RawComponent h s f g
  -> RawComponent h s' f' g
transformChild i = transform (injState i) (prjState i) (injQuery i) (prjQuery i)

-- | Changes the component's `g` type. A use case for this would be to interpret
-- | some `Free` monad as `Aff` so the component can be used with `runUI`.
interpret
  :: forall s f g g'
   . Functor g'
  => g ~> g'
  -> Component s f g
  -> Component s f g'
interpret nat (RawComponent c) = RawComponent
    { render: render'
    , eval: hoistFree (hoistHalogenF nat) <<< c.eval
    , initializer: c.initializer
    , finalizers: map (mapFinalized nat) <$> c.finalizers
    }
  where
  render' :: s -> RenderResult HTML s f g'
  render' st =
    case c.render st of
      { state, hooks, tree } ->
        { state
        , hooks: rmapHook nat <$> hooks
        , tree
        }

-- | Runs a component's `render` function with the specified state, returning
-- | the generated `HTML` and new state.
renderComponent
  :: forall h s f g
   . RawComponent h s f g
  -> s
  -> RenderResult h s f g
renderComponent (RawComponent c) = c.render

-- | Runs a compnent's `query` function with the specified query input and
-- | returns the pending computation as a `Free` monad.
queryComponent :: forall h s f g. RawComponent h s f g -> f ~> ComponentDSL s f g
queryComponent (RawComponent c) = c.eval

initializeComponent :: forall h s f g. RawComponent h s f g -> Maybe (f Unit)
initializeComponent (RawComponent c) = c.initializer

finalizeComponent :: forall h s f g. RawComponent h s f g -> s -> Array (Finalized g)
finalizeComponent (RawComponent c) = c.finalizers
