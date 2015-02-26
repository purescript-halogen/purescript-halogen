module Halogen 
  ( Spec()
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
import Data.Exists

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
    
newtype SpecRecord s i = SpecRecord
  { render :: s -> HTML i
  , foldState :: s -> i -> s
  }
  
-- | A `Spec` defines a state machine which responds to inputs of type `i` and maintains a
-- | state of type `s`.

render :: forall s i. SpecRecord s i -> s -> HTML i
render (SpecRecord spec) s = spec.render s

foldState :: forall s i. SpecRecord s i -> s -> i -> s
foldState (SpecRecord spec) s i = spec.foldState s i

-- | A `Spec` defines a state machine which responds to inputs of some hidden type `i` and maintains a
-- | state of type `s`.
newtype Spec s = Spec (Exists (SpecRecord s))

-- | Create a `Spec` by providing a `render` function, and an operation
-- | which updates the state given an input.
-- |
-- | The type `i` is hidden in the return type.
mkSpec :: forall s i. (s -> HTML i) -> (s -> i -> s) -> Spec s
mkSpec render_ foldState_ = Spec $ mkExists $ SpecRecord { render: render_, foldState: foldState_ }

-- | `embed` allows us to enlarge the state types by using a `Lens`.
embed :: forall s1 s2. LensP s1 s2 -> Spec s2 -> Spec s1
embed lens (Spec spec) = runExists embed' spec
  where
  embed' :: forall i. SpecRecord s2 i -> Spec s1
  embed' rec = mkSpec render_ foldState_ 
    where   
    render_ :: s1 -> HTML i
    render_ s1 = render rec (s1 ^. lens)
    
    foldState_ :: s1 -> i -> s1
    foldState_ s1 i = set lens (foldState rec (s1 ^. lens) i) s1
                          
-- | Side-by-side in a `div` element
beside :: forall s1 s2. Spec s1 -> Spec s2 -> Spec (Tuple s1 s2)
beside (Spec spec1) (Spec spec2) = runExists (\rec1 -> runExists (beside' rec1) spec2) spec1
  where
  beside' :: forall i1 i2. SpecRecord s1 i1 -> SpecRecord s2 i2 -> Spec (Tuple s1 s2)
  beside' rec1 rec2 = mkSpec render_ foldState_ 
    where
    render_ :: Tuple s1 s2 -> HTML (Either i1 i2)
    render_ (Tuple s1 s2) =
      div' [ Left  <$> render rec1 s1
           , Right <$> render rec2 s2
           ]
    
    foldState_ :: Tuple s1 s2 -> Either i1 i2 -> Tuple s1 s2
    foldState_ (Tuple s1 s2) (Left i1)  = Tuple (foldState rec1 s1 i1) s2
    foldState_ (Tuple s1 s2) (Right i2) = Tuple s1 (foldState rec2 s2 i2)

-- | `runSpec` is responsible for taking a `Spec` and hooking up its event
-- | handlers to rerender the DOM. It maintains the state of the component
-- | using a `RefVal`.
runSpec :: forall s eff. Spec s -> s -> Eff (ref :: Ref, dom :: DOM | eff) Node
runSpec (Spec spec) initialState = do
  ref <- newRef Nothing
  runExists (runSpec' ref) spec
  where
  runSpec' :: forall i. RefVal _ -> SpecRecord s i -> Eff (ref :: Ref, dom :: DOM | eff) Node
  runSpec' ref rec = do
    let html  = render rec initialState
        vtree = renderHtml inputHandler html
        node  = createElement vtree
    writeRef ref $ Just { state: initialState, vtree: vtree, node: node }
    return node
    where
    renderState :: s -> Eff (ref :: Ref, dom :: DOM | eff) Unit
    renderState state = do
      Just { vtree: vtree, node: node } <- readRef ref
      let html   = render rec state
          vtree' = renderHtml inputHandler html
          diffs  = diff vtree vtree'
      node' <- patch diffs node
      writeRef ref $ Just { state: state, vtree: vtree', node: node' }

    inputHandler :: i -> Eff (ref :: Ref, dom :: DOM | eff) Unit
    inputHandler i = do
      Just { state: state, vtree: vtree } <- readRef ref
      renderState (foldState rec state i)
