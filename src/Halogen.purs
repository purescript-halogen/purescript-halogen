module Halogen 
  ( Spec()
  , render
  , foldState
  , mkSpec
  
  , embed
  , beside
  
  , runSpec
  ) where
    
import DOM

import Data.Array
import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Monoid

import Control.Monad.Eff
import Control.Monad.Eff.Ref

import VirtualDOM
import VirtualDOM.VTree

import Optic.Types
import Optic.Getter ((^.))
import Optic.Setter (set)
import Optic.Prism (matching)    
    
import Halogen.HTML
import Halogen.Props
import Halogen.VirtualDOM    
    
-- | A `Spec` defines a state machine which responds to inputs of type `i` and maintains a
-- | state of type `s`.
newtype Spec s i = Spec
  { render :: s -> HTML i
  , foldState :: s -> i -> s
  }

render :: forall s i. Spec s i -> s -> HTML i
render (Spec spec) s = spec.render s

foldState :: forall s i. Spec s i -> s -> i -> s
foldState (Spec spec) s i = spec.foldState s i

-- | Create a `Spec` by providing a `render` function, and an operation
-- | which updates the state given an input.
mkSpec :: forall s i. (s -> HTML i) -> (s -> i -> s) -> Spec s i
mkSpec render_ foldState_ = Spec { render: render_, foldState: foldState_ }

-- |
-- `embed` allows us to change the state and input types by using a `Lens` and a `Prism`.
-- 
-- We need to provide a "prototype" input of type `i1` because the `Prism` may not
-- correspond directly to a pair (i2 -> i1, i1 -> Maybe i2)`.
--
embed :: forall s1 s2 i1 i2. LensP s1 s2 -> PrismP i1 i2 -> i1 -> Spec s2 i2 -> Spec s1 i1
embed lens prism proto spec = mkSpec render_ foldState_
  where
  render_ :: s1 -> HTML i1
  render_ s1 = let s2 = s1 ^. lens
               in flip (set prism) proto <$> render spec s2

  foldState_ :: s1 -> i1 -> s1
  foldState_ s1 i1 = let s2 = s1 ^. lens
                     in case prism `matching` i1 of
                          Left _ -> s1
                          Right i2 -> set lens (foldState spec s2 i2) s1
                          
-- | Side-by-side in a `div` element
beside :: forall s1 s2 i1 i2. Spec s1 i1 -> Spec s2 i2 -> Spec (Tuple s1 s2) (Either i1 i2)
beside spec1 spec2 = mkSpec render_ foldState_
  where
  render_ :: Tuple s1 s2 -> HTML (Either i1 i2)
  render_ (Tuple s1 s2) =
    div' [ Left  <$> render spec1 s1
         , Right <$> render spec2 s2
         ]

  foldState_ :: Tuple s1 s2 -> Either i1 i2 -> Tuple s1 s2
  foldState_ (Tuple s1 s2) (Left i1)  = Tuple (foldState spec1 s1 i1) s2
  foldState_ (Tuple s1 s2) (Right i2) = Tuple s1 (foldState spec2 s2 i2)

-- | `runSpec` is responsible for taking a `Spec` and hooking up its event
-- | handlers to rerender the DOM. It maintains the state of the component
-- | using a `RefVal`.
runSpec :: forall s i eff. Spec s i -> s -> Eff (ref :: Ref, dom :: DOM | eff) Node
runSpec spec initialState = newRef Nothing >>= runSpec'
  where
  runSpec' :: RefVal _ -> Eff (ref :: Ref, dom :: DOM | eff) Node
  runSpec' ref = do
    let html  = render spec initialState
        vtree = renderHtml inputHandler html
        node  = createElement vtree
    writeRef ref $ Just { state: initialState, vtree: vtree, node: node }
    return node
    where
    renderState :: s -> Eff (ref :: Ref, dom :: DOM | eff) Unit
    renderState state = do
      Just { vtree: vtree, node: node } <- readRef ref
      let html   = render spec state
          vtree' = renderHtml inputHandler html
          diffs  = diff vtree vtree'
      node' <- patch diffs node
      writeRef ref $ Just { state: state, vtree: vtree', node: node' }

    inputHandler :: i -> Eff (ref :: Ref, dom :: DOM | eff) Unit
    inputHandler i = do
      Just { state: state, vtree: vtree } <- readRef ref
      renderState (foldState spec state i)
