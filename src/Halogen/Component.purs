module Halogen.Component where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Ref (REF(), Ref(), newRef, writeRef, readRef)
import Control.Monad.Free (Free(), FreeC())
import Control.Monad.State (State(), runState)
import Control.Monad.State.Class (MonadState, get, gets, put)
import Control.Monad.State.Trans (StateT(), runStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Plus (Plus, empty)

import Data.Bifunctor (lmap)
import Data.DOM.Simple.Types (HTMLElement())
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Void (Void())

import DOM (DOM())

import Halogen.HTML.Renderer.VirtualDOM (renderHTML)
import Halogen.HTML.Core (HTML(), install)
import Halogen.Internal.VirtualDOM (VTree(), createElement, diff, patch)

import qualified Data.Map as M

newtype Component s f g p = Component
  { render :: State s (HTML p (f Unit))
  , query  :: forall i. f i -> StateT s g i
  }

type ComponentF s f = Component s (Free f)
type ComponentFC s f = Component s (FreeC f)

instance functorComponent :: Functor (Component s f g) where
  map f (Component c) =
    Component { render: lmap f <$> c.render
              , query: c.query
              }

component :: forall s f g p. (s -> HTML p (f Unit)) -> (forall i. f i -> StateT s g i) -> Component s f g p
component r q = Component { render: renderPure r, query: q }

renderPure :: forall s p i. (s -> (HTML p i)) -> State s (HTML p i)
renderPure = gets

-- installR :: forall s f g pl pr s' f' p'. (Ord pr, Plus g)
--          => Component s f (QueryT s s' f' pr p' g) (Either pl pr)
--          -> (pr -> Tuple s' (Component s' f' g p'))
--          -> Component (InstalledState s s' f' pl p' g) (Coproduct f (ChildF pr f')) g (Either pl p')
-- installR (Component c) f = todo

-- installL :: forall s f g pl pr s' f' p'. (Ord pl, Plus g)
--          => Component s f (QueryT s s' f' pl p' g) (Either pl pr)
--          -> (pl -> Tuple s' (Component s' f' g p'))
--          -> Component (InstalledState s s' f' pr p' g) (Coproduct f (ChildF pl f')) g (Either pr p')
-- installL (Component c) f = todo

installAll :: forall s f g p s' f' p'. (Ord p, Functor f, Functor f', Monad g, Plus g)
           => Component s f (QueryT s s' f' p p' g) p
           -> (p -> Tuple s' (Component s' f' g p'))
           -> Component (InstalledState s s' f' p p' g) (Coproduct f (ChildF p f')) g p'
installAll (Component c) f = Component { render: render, query: query }
  where

  render :: State (InstalledState s s' f' p p' g) (HTML p' ((Coproduct f (ChildF p f')) Unit))
  render = do
    st <- get
    case runState c.render st.parent of
      Tuple html s -> do
        put st { parent = s, children = M.empty :: M.Map p (ComponentState s' f' g p') }
        install (renderChild st) left html

  renderChild :: (InstalledState s s' f' p p' g)
              -> p
              -> State (InstalledState s s' f' p p' g) (HTML p' ((Coproduct f (ChildF p f')) Unit))
  renderChild st p = renderChild' st p $ case M.lookup p st.children of
    Just sc -> sc
    Nothing -> f p

  renderChild' :: (InstalledState s s' f' p p' g)
               -> p
               -> ComponentState s' f' g p'
               -> State (InstalledState s s' f' p p' g) (HTML p' ((Coproduct f (ChildF p f')) Unit))
  renderChild' st p (Tuple s comp@(Component c)) = case runState c.render s of
    Tuple html s' -> do
      put st { children = M.insert p (Tuple s' comp) st.children }
      pure $ right <<< ChildF p <$> html

  query :: forall i. (Coproduct f (ChildF p f')) i
                  -> StateT (InstalledState s s' f' p p' g) g i
  query fi = coproduct (pure empty) queryChild fi

  queryChild :: forall i. ChildF p f' i
                       -> StateT (InstalledState s s' f' p p' g) g i
  queryChild (ChildF p q) = do
    st <- get :: _ (InstalledState s s' f' p p' g)
    case M.lookup p st.children of
      Nothing -> empty
      Just (Tuple s comp@(Component c)) -> do
        Tuple i s' <- lift $ runStateT (c.query q) s
        put st { children = M.insert p (Tuple s' comp) st.children }
        pure i

data ChildF p f i = ChildF p (f i)

instance functorChildF :: (Functor f) => Functor (ChildF p f) where
  map f (ChildF p fi) = ChildF p (f <$> fi)

type ComponentState s f g p = Tuple s (Component s f g p)

type InstalledState s s' f' p p' g =
  { parent   :: s
  , children :: M.Map p (ComponentState s' f' g p')
  }

newtype QueryT s s' f' p p' g a = QueryT (StateT (InstalledState s s' f' p p' g) g a)

runQueryT :: forall s s' f' p p' g a. QueryT s s' f' p p' g a -> StateT (InstalledState s s' f' p p' g) g a
runQueryT (QueryT a) = a

query :: forall s s' f' p p' g. (Monad g, Ord p) => p -> (forall i. f' i -> QueryT s s' f' p p' g (Maybe i))
query p q = QueryT do
  st <- get :: _ (InstalledState s s' f' p p' g)
  case M.lookup p st.children of
    Nothing -> pure Nothing
    Just (Tuple s comp@(Component c)) -> do
      Tuple i s' <- lift $ runStateT (c.query q) s
      put st { children = M.insert p (Tuple s' comp) st.children }
      pure (Just i)

instance functorQueryT :: (Monad g) => Functor (QueryT s s' f' p p' g) where
  map f (QueryT a) = QueryT (map f a)

instance applyQueryT :: (Monad g) => Apply (QueryT s s' f' p p' g) where
  apply (QueryT f) (QueryT a) = QueryT (apply f a)

instance applicativeQueryT :: (Monad g) => Applicative (QueryT s s' f' p p' g) where
  pure = QueryT <<< pure

instance bindQueryT :: (Monad g) => Bind (QueryT s s' f' p p' g) where
  bind (QueryT a) f = QueryT (a >>= runQueryT <<< f)

instance monadQueryT :: (Monad g) => Monad (QueryT s s' f' p p' g)

instance monadStateQueryT :: (Monad g) => MonadState s (QueryT s s' f' p p' g) where
  state f = QueryT do
    st <- get
    let s' = f st.parent
    put st { parent = snd s' }
    pure $ fst s'

instance monadTransQueryT :: MonadTrans (QueryT s s' f' p p') where
  lift = QueryT <<< lift
