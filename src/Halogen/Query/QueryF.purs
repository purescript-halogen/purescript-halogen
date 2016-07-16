module Halogen.Query.QueryF
  ( QueryF(..)
  , ParentF
  , ComponentF
  , getChildren
  , getChild
  , runQuery
  , query
  , queryAll
  , childSlots
  ) where

import Prelude

import Control.Monad.Free (Free, liftF)

import Data.Const (Const)
import Data.Functor.Coproduct (Coproduct, left)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.Traversable (traverse)

import Halogen.Component.Types (Component)
import Halogen.Query.HalogenF (HalogenF(..))

type ParentF f f' g p = Coproduct (QueryF f' g p) (Coproduct f f')
type ComponentF f g = ParentF f (Const Void) g Void

data QueryF f g p a
  = GetChildren (Map.Map p (Component f g) -> a)
  | RunQuery ((Component f g -> f ~> g) -> g a)

instance functorQueryF :: Functor g => Functor (QueryF f g p) where
  map f = case _ of
    GetChildren k -> GetChildren (map f k)
    RunQuery k -> RunQuery (map f <<< k)

getChildren
  :: forall s f f' g p
   . Free (HalogenF s (ParentF f f' g p) g) (Map.Map p (Component f' g))
getChildren = liftF $ QueryFHF $ left $ GetChildren id

getChild
  :: forall s f f' g p
   . Ord p
  => p
  -> Free (HalogenF s (ParentF f f' g p) g) (Maybe (Component f' g))
getChild p = Map.lookup p <$> getChildren

runQuery
  :: forall s f f' g p a
   . Applicative g
  => f' a
  -> Component f' g
  -> Free (HalogenF s (ParentF f f' g p) g) a
runQuery q c = do
  x <- liftF $ QueryFHF $ left $ RunQuery \k -> k c q
  liftF $ QueryGHF (pure x)

query
  :: forall s f f' g p a
   . (Applicative g, Ord p)
  => p
  -> f' a
  -> Free (HalogenF s (ParentF f f' g p) g) (Maybe a)
query p q =
  getChild p >>= case _ of
    Nothing -> pure Nothing
    Just comp -> Just <$> runQuery q comp

queryAll
  :: forall s f f' g p a
   . (Applicative g, Ord p)
  => p
  -> f' a
  -> Free (HalogenF s (ParentF f f' g p) g) (Map.Map p a)
queryAll p q = traverse (runQuery q) =<< getChildren

childSlots
  :: forall s f f' g p a
   . (Applicative g, Ord p)
  => p
  -> f' a
  -> Free (HalogenF s (ParentF f f' g p) g) (List p)
childSlots p q = Map.keys <$> getChildren
