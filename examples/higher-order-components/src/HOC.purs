module HOC where

import Prelude

import Data.Coyoneda (Coyoneda, unCoyoneda, liftCoyoneda)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM as HQ

data Query f i o a
  -- | Example query for the HOC
  = ToggleOn a
  | Set a

  -- | Contains a query of the inner component
  | Inner (Coyoneda f a)
  -- | Handle messages of te inner component
  | HandleInner o a
  -- | React to input to the HOC
  | InnerInput i a

type Slot f i o = H.Slot (Query f i o) o

-- | Lift a query from the inner component to a query of the HOC. Useful when
-- | querying a component thats "inside" this HOC.
liftQuery :: forall f i o a. f a -> Query f i o a
liftQuery = Inner <<< liftCoyoneda

type State i =
  -- | State of the HOC itself
  { on :: Boolean
  -- | Keep track of inputs that we pass through to the inner component
  , input :: i
  }

class CanSet f where
  set :: Boolean -> H.Action f

type ChildSlots f o =
  ( child :: H.Slot f o Unit
  )

_child = SProxy :: SProxy "child"

-- | Takes a component and wraps it to produce a new component with added
-- | functionality.
-- |
-- | For the sake of this example, we require that all components that this
-- | factory wraps implement the `CanSet` typeclass, i.e. something in them
-- | can be "set" by giving a boolean.
-- |
-- | `f`, `i`, `o` are the query, input and output types of the wrapped
-- | component.
factory
  :: forall f i o m
   . CanSet f
  => H.Component HH.HTML f i o m
  -> H.Component HH.HTML (Query f i o) i o m
factory innerComponent =
  H.component
    { initialState: { on: true, input: _ }
    , render
    , eval
    , receiver: \i -> Just $ InnerInput i unit
    }

  where

  render :: State i -> H.ComponentHTML (Query f i o) (ChildSlots f o) m
  render state =
    HH.div_
      [ HH.hr_
      , HH.p_
        [ HH.button
          [ HE.onClick (HE.input_ ToggleOn) ]
          [ HH.text "Toggle wrapper state" ]
        , HH.text $ " Wrapper state: " <> if state.on then "on" else "off"
        ]
      , HH.p_
        [ HH.button
          [ HE.onClick (HE.input_ Set) ]
          [ HH.text "Set inner component to off" ]
        ]
      , HH.p_
        [ HH.slot _child unit innerComponent state.input (HE.input HandleInner)
        ]
      , HH.hr_
      ]

  eval :: Query f i o ~> H.HalogenM (State i) (Query f i o) (ChildSlots f o) o m
  eval (ToggleOn next) = do
    H.modify $ \state -> state { on = not state.on }
    pure next
  eval (Set next) = do
    _ <- H.query _child unit $ H.action (set false)
    pure next
  eval (Inner iq) = iq # unCoyoneda \k q -> do
    result <- H.query _child unit q
    case result of
      Nothing ->
        HQ.halt "HOC inner component query failed (this should be impossible)"
      Just a -> pure (k a)
  eval (HandleInner o next) = do
    H.raise o
    pure next
  eval (InnerInput i next) = do
    H.modify $ _{ input = i }
    pure next
