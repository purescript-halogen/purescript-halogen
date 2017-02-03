module HOC where

import Prelude
import Data.Coyoneda (Coyoneda, unCoyoneda, liftCoyoneda)
import Data.Maybe (Maybe(Nothing, Just))

import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.HalogenM as HQ
import Halogen.HTML.Events as HE

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

-- | Lift a query from the inner component to a query of the HOC. Useful when
-- | querying a component thats "inside" this HOC.
liftQuery :: forall f i o a. f a -> Query f i o a
liftQuery = Inner <<< liftCoyoneda

type Slot = Unit

type State i =
  -- | State of the HOC itself
  { on :: Boolean
  -- | Keep track of inputs that we pass through to the inner component
  , input :: i
  }

class CanSet f where
  set :: Boolean -> H.Action f

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
  H.parentComponent
    { initialState: { on: true, input: _ }
    , render
    , eval
    , receiver: \i -> Just $ InnerInput i unit
    }

  where

  render :: State i -> H.ParentHTML (Query f i o) f Slot m
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
        [ HH.slot unit innerComponent state.input (HE.input HandleInner)
        ]
      , HH.hr_
      ]

  eval :: Query f i o ~> H.ParentDSL (State i) (Query f i o) f Slot o m
  eval (ToggleOn next) = do
    H.modify $ \state -> state { on = not state.on }
    pure next
  eval (Set next) = do
    H.query unit $ H.action (set false)
    pure next
  eval (Inner iq) = iq # unCoyoneda \k q -> do
    result <- H.query unit q
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
