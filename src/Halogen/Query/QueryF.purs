module Halogen.Query.QueryF
  ( QueryF(..)
  , HHH
  , query
  , interpret
  ) where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Map as Map
import Data.Functor.Coproduct (Coproduct, left)
import Data.Maybe (Maybe(..))
import Halogen.Component.Types (Component)
import Halogen.Query.HalogenF (HalogenF(..))

-- -- | An intermediate algebra used to associate values from a child component's
-- -- | algebra with the slot the component was installed into.
-- data ChildF p f i = ChildF p (f i)
--
-- -- | Extracts the query part from a ChildF value, discarding the slot value.
-- runChildF :: forall p f i. ChildF p f i -> f i
-- runChildF (ChildF _ q) = q
--
-- instance functorChildF :: Functor f => Functor (ChildF p f) where
--   map f (ChildF p fi) = ChildF p (f <$> fi)

data QueryF f g p a
  = GetChildren (Map.Map p (Component f g) -> a)
  | RunQuery ((Component f g -> f a -> g a) -> g a)

type HHH s f f' g p = Free (HalogenF s (Coproduct (QueryF f' g p) (Coproduct f f')) g)

getChildren :: forall s f f' g p. HHH s f f' g p (Map.Map p (Component f' g))
getChildren = liftF $ QueryFHF $ left $ GetChildren id

getChild :: forall s f f' g p. Ord p => p -> HHH s f f' g p (Maybe (Component f' g))
getChild p = Map.lookup p <$> getChildren

runQuery :: forall s f f' g p a. Applicative g => f' a -> Component f' g -> HHH s f f' g p a
runQuery q c = do
  x <- liftF $ QueryFHF $ left $ RunQuery \k -> k c q
  liftF $ QueryGHF (pure x)

query :: forall s f f' g p a. (Applicative g, Ord p) => p -> f' a -> HHH s f f' g p (Maybe a)
query p q =
  getChild p >>= case _ of
    Nothing -> pure Nothing
    Just comp -> Just <$> runQuery q comp

  -- , query
  -- , query'
  -- , queryAll
  -- , queryAll'
  -- , childSlots

interpret :: forall f g p. Applicative g => (Component f g -> f ~> g) -> QueryF f g p ~> g
interpret driver = case _ of
  GetChildren k -> pure $ k Map.empty
  RunQuery k -> k driver
