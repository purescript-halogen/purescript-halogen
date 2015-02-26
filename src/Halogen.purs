module Halogen 
  ( Spec()
  , mkSpec
  
  , Render()
  , FoldState()
  
  , Void()
  , Component()
  
  , embed
  , beside
  , append
  
  , runComponent
  ) where
    
import DOM

import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Monoid
import Data.Exists
import Data.Bifunctor

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

-- | A function which renders a component given an array of its rendered children and the current state
type Render s c i = s -> HTML c i

-- | A function which can respond to inputs by updating a state
type FoldState s i = s -> i -> s
    
-- | A component will consist of a rendering function and a function to update its state
newtype SpecRecord s c i = SpecRecord
  { render :: Render s c i
  , foldState :: FoldState s i
  }

render :: forall s c i. SpecRecord s c i -> s -> HTML c i
render (SpecRecord spec) s = spec.render s

foldState :: forall s c i. SpecRecord s c i -> s -> i -> s
foldState (SpecRecord spec) s i = spec.foldState s i

-- | A `Spec` defines a state machine which responds to inputs of some hidden type `i` and maintains a
-- | state of type `s`. 
-- |
-- | The type `c` is used to represent placeholders for child elements.
newtype Spec s c = Spec (Exists (SpecRecord s c))

-- | An empty type used to indicate the lack of any unpopulated child nodes
data Void

-- | A `Component` is a `Spec` with no unpopulated child nodes
type Component s = Spec s Void

-- | Create a `Spec` by providing a `render` function, and an operation
-- | which updates the state given an input.
-- |
-- | The type `i` is hidden in the return type.
mkSpec :: forall s c i. Render s c i -> FoldState s i -> Spec s c
mkSpec render_ foldState_ = Spec $ mkExists $ SpecRecord { render: render_, foldState: foldState_ }

-- | `embed` allows us to enlarge the state types by using a `Lens`.
embed :: forall s1 s2 c. LensP s1 s2 -> Spec s2 c -> Spec s1 c
embed lens (Spec spec) = runExists embed' spec
  where
  embed' :: forall i. SpecRecord s2 c i -> Spec s1 c
  embed' rec = mkSpec render_ foldState_ 
    where   
    render_ :: s1 -> HTML c i
    render_ s1 = render rec (s1 ^. lens)
    
    foldState_ :: s1 -> i -> s1
    foldState_ s1 i = set lens (foldState rec (s1 ^. lens) i) s1
                          
-- | Side-by-side in a `div` element
beside :: forall s1 s2. Spec s1 Void -> Spec s2 Void -> Spec (Tuple s1 s2) Void
beside (Spec spec1) (Spec spec2) = runExists (\rec1 -> runExists (beside' rec1) spec2) spec1
  where
  beside' :: forall i1 i2. SpecRecord s1 Void i1 -> SpecRecord s2 Void i2 -> Spec (Tuple s1 s2) Void
  beside' rec1 rec2 = mkSpec render_ foldState_ 
    where
    render_ :: Tuple s1 s2 -> HTML Void (Either i1 i2)
    render_ (Tuple s1 s2) =
      div' [ bimap id  Left  $ render rec1 s1
           , bimap id Right $ render rec2 s2
           ]
    
    foldState_ :: Tuple s1 s2 -> Either i1 i2 -> Tuple s1 s2
    foldState_ (Tuple s1 s2) (Left i1)  = Tuple (foldState rec1 s1 i1) s2
    foldState_ (Tuple s1 s2) (Right i2) = Tuple s1 (foldState rec2 s2 i2)

-- | Replace the leftmost child placeholder
append :: forall s c. Spec s (Maybe c) -> Spec s c -> Spec s c
append (Spec spec1) (Spec spec2) = runExists (\rec1 -> runExists (append' rec1) spec2) spec1
  where
  append' :: forall i1 i2. SpecRecord s (Maybe c) i1 -> SpecRecord s c i2 -> Spec s c
  append' rec1 rec2 = mkSpec render_ foldState_ 
    where
    render_ :: s -> HTML c (Either i1 i2)
    render_ s = render rec1 s `graft` render rec2 s
    
    foldState_ :: s -> Either i1 i2 -> s
    foldState_ s (Left i1)  = foldState rec1 s i1
    foldState_ s (Right i2) = foldState rec2 s i2

-- | `runComponent` is responsible for taking a `Component` and hooking up its event
-- | handlers to rerender the DOM. It maintains the state of the component
-- | using a `RefVal`.
runComponent :: forall s eff. Component s -> s -> Eff (ref :: Ref, dom :: DOM | eff) Node
runComponent (Spec spec) initialState = do
  ref <- newRef Nothing
  runExists (runSpec' ref) spec
  where
  runSpec' :: forall i. RefVal _ -> SpecRecord s Void i -> Eff (ref :: Ref, dom :: DOM | eff) Node
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
