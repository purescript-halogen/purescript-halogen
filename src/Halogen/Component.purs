module Halogen.Component
  ( Component()
  , ComponentF()
  , ComponentFC()
  , Render()
  , RenderF()
  , RenderFC()
  , Eval()
  , component
  , componentF
  , componentFC
  , renderComponent
  , queryComponent
  , ComponentState()
  , InstalledState()
  , ChildF()
  , installL
  , installR
  , installAll
  , QueryT()
  -- , query
  ) where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Free (Free(), FreeC(), bindF, bindFC, liftF, mapF)
import Control.Monad.State (State(), runState)
import Control.Monad.State.Trans (StateT())
import Control.Monad.Trans (MonadTrans, lift)
import Control.Plus (Plus, empty)
import qualified Control.Monad.State.Class as CMS

import Data.Bifunctor (lmap)
import Data.Coyoneda (Natural())
import Data.Either (Either(..), either)
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Void (Void())

import Halogen.HTML.Core (HTML(..), install)
import Halogen.Query.StateF (StateF(..), get)

import qualified Data.Map as M

newtype Component s f g p = Component
  { render :: State s (HTML p (f Unit))
  , query  :: Natural f (Free (Coproduct (StateF s) g))
  }

type ComponentF s f = Component s (Free f)
type ComponentFC s f = Component s (FreeC f)

type Render s p f = s -> HTML p (f Unit)
type RenderF s p f = s -> HTML p (Free f Unit)
type RenderFC s p f = s -> HTML p (FreeC f Unit)

type Eval f s g = Natural f (Free (Coproduct (StateF s) g))

renderComponent :: forall s f g p. Component s f g p -> s -> Tuple (HTML p (f Unit)) s
renderComponent (Component c) = runState c.render

queryComponent :: forall s f g p i. Component s f g p -> f i -> Free (Coproduct (StateF s) g) i
queryComponent (Component c) q = c.query q

instance functorComponent :: Functor (Component s f g) where
  map f (Component c) =
    Component { render: lmap f <$> c.render
              , query: c.query
              }

component :: forall s f g p. Render s p f -> Eval f s g -> Component s f g p
component r q = Component { render: CMS.gets r, query: q }

componentF :: forall s f g p. (Functor f, Functor g) => RenderF s p f -> Eval f s g -> ComponentF s f g p
componentF r e = component r (\fa -> bindF fa e)

componentFC :: forall s f g p. (Functor g) => RenderFC s p f -> Eval f s g -> ComponentFC s f g p
componentFC r e = component r (\fa -> bindFC fa e)

type ComponentState s f g p = Tuple s (Component s f g p)

type InstalledState s s' f' p p' g =
  { parent   :: s
  , children :: M.Map p (ComponentState s' f' g p')
  }

data ChildF p f i = ChildF p (f i)

instance functorChildF :: (Functor f) => Functor (ChildF p f) where
  map f (ChildF p fi) = ChildF p (f <$> fi)

installer :: forall s f g p s' f' p' q q'. (Ord p, Monad g, Plus g)
          => (q -> Either p q')
          -> (p' -> q')
          -> Component s f (QueryT s s' f' p p' g) q
          -> (p -> ComponentState s' f' g p')
          -> Component (InstalledState s s' f' p p' g) (Coproduct f (ChildF p f')) g q'
installer fromQ toQ' c f = Component { render: render, query: query }
  where

  render :: State (InstalledState s s' f' p p' g) (HTML q' ((Coproduct f (ChildF p f')) Unit))
  render = do
    st <- CMS.get :: _ (InstalledState s s' f' p p' g)
    case renderComponent c st.parent of
      Tuple html s -> do
        -- Empty the state so that we don't keep children that are no longer
        -- being rendered...
        CMS.put { parent: s, children: M.empty :: M.Map p (ComponentState s' f' g p') }
        -- ...but then pass through the old state so we can lookup child
        -- components that are being re-rendered
        install (renderChild st) left html

  renderChild :: InstalledState s s' f' p p' g
              -> q
              -> State (InstalledState s s' f' p p' g) (HTML q' ((Coproduct f (ChildF p f')) Unit))
  renderChild st p = case fromQ p of
    Left p' -> renderChild' st p' $ case M.lookup p' st.children of
      Just sc -> sc
      Nothing -> f p'
    Right p' -> pure $ Placeholder p'

  renderChild' :: InstalledState s s' f' p p' g
               -> p
               -> ComponentState s' f' g p'
               -> State (InstalledState s s' f' p p' g) (HTML q' ((Coproduct f (ChildF p f')) Unit))
  renderChild' st p (Tuple s c) = case renderComponent c s of
    Tuple html s' -> do
      CMS.put st { children = M.insert p (Tuple s' c) st.children }
      pure $ lmap toQ' $ right <<< ChildF p <$> html

  query :: Eval (Coproduct f (ChildF p f')) (InstalledState s s' f' p p' g) g
  query = coproduct (const empty') queryChild

  queryChild :: Eval (ChildF p f') (InstalledState s s' f' p p' g) g
  queryChild (ChildF p q) = do
    st <- get :: _ (InstalledState s s' f' p p' g)
    case M.lookup p st.children of
      Nothing -> empty'
      Just (Tuple s' c') -> mapF (coproduct (left <<< remapStateAction s' c') right) (queryComponent c' q)
      where
      remapStateAction :: s' -> Component s' f' g p' -> Natural (StateF s') (StateF (InstalledState s s' f' p p' g))
      remapStateAction s' c' (Get k) = Get (\_ -> k s')
      remapStateAction s' c' (Modify f next) = Modify (\st -> { parent: st.parent, children: M.insert p (Tuple (f s') c') st.children }) next

empty' :: forall f g a. (Functor f, Plus g) => Free (Coproduct f g) a
empty' = liftF (right empty :: Coproduct f g a)

installL :: forall s f g pl pr s' f' p'. (Ord pl, Monad g, Plus g)
         => Component s f (QueryT s s' f' pl p' g) (Either pl pr)
         -> (pl -> ComponentState s' f' g p')
         -> Component (InstalledState s s' f' pl p' g) (Coproduct f (ChildF pl f')) g (Either p' pr)
installL = installer (either Left (Right <<< Right)) Left

installR :: forall s f g pl pr s' f' p'. (Ord pr, Monad g, Plus g)
         => Component s f (QueryT s s' f' pr p' g) (Either pl pr)
         -> (pr -> ComponentState s' f' g p')
         -> Component (InstalledState s s' f' pr p' g) (Coproduct f (ChildF pr f')) g (Either pl p')
installR = installer (either (Right <<< Left) Left) Right

installAll :: forall s f g p s' f' p'. (Ord p, Monad g, Plus g)
           => Component s f (QueryT s s' f' p p' g) p
           -> (p -> ComponentState s' f' g p')
           -> Component (InstalledState s s' f' p p' g) (Coproduct f (ChildF p f')) g p'
installAll = installer Left id

newtype QueryT s s' f' p p' g a = QueryT (StateT (InstalledState s s' f' p p' g) g a)

runQueryT :: forall s s' f' p p' g a. QueryT s s' f' p p' g a -> StateT (InstalledState s s' f' p p' g) g a
runQueryT (QueryT a) = a

-- query :: forall s s' f' p p' g. (Monad g, Ord p) => p -> (forall i. f' i -> QueryT s s' f' p p' g (Maybe i))
-- query p q = QueryT do
--   st <- get :: _ (InstalledState s s' f' p p' g)
--   case M.lookup p st.children of
--     Nothing -> pure Nothing
--     Just (Tuple s c) -> do
--       Tuple i s' <- lift $ queryComponent c q s
--       put st { children = M.insert p (Tuple s' c) st.children }
--       pure (Just i)

instance functorQueryT :: (Monad g) => Functor (QueryT s s' f' p p' g) where
  map f (QueryT a) = QueryT (map f a)

instance applyQueryT :: (Monad g) => Apply (QueryT s s' f' p p' g) where
  apply (QueryT f) (QueryT a) = QueryT (apply f a)

instance applicativeQueryT :: (Monad g) => Applicative (QueryT s s' f' p p' g) where
  pure = QueryT <<< pure

instance bindQueryT :: (Monad g) => Bind (QueryT s s' f' p p' g) where
  bind (QueryT a) f = QueryT (a >>= runQueryT <<< f)

instance monadQueryT :: (Monad g) => Monad (QueryT s s' f' p p' g)

instance monadStateQueryT :: (Monad g) => CMS.MonadState s (QueryT s s' f' p p' g) where
  state f = QueryT do
    st <- CMS.get
    let s' = f st.parent
    CMS.put st { parent = snd s' }
    pure $ fst s'

instance monadTransQueryT :: MonadTrans (QueryT s s' f' p p') where
  lift = QueryT <<< lift
