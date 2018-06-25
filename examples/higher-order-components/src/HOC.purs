module Example.HOC.HOC where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Exception as Exn
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Action o
  = Toggle
  | Reset
  | HandleInner o

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
  => MonadError Exn.Error m
  => H.Component HH.HTML f i o m
  -> H.Component HH.HTML f i o m
factory innerComponent =
  H.mkComponent
    { initialState: { on: true, input: _ }
    , render
    , eval
    }

  where

  render :: State i -> H.ComponentHTML' (Action o) (ChildSlots f o) m
  render state =
    HH.div_
      [ HH.hr_
      , HH.p_
        [ HH.button
          [ HE.onClick (\_ -> Just Toggle) ]
          [ HH.text "Toggle wrapper state" ]
        , HH.text $ " Wrapper state: " <> if state.on then "on" else "off"
        ]
      , HH.p_
        [ HH.button
          [ HE.onClick (\_ -> Just Reset) ]
          [ HH.text "Set inner component to off" ]
        ]
      , HH.p_
        [ HH.slot _child unit innerComponent state.input (Just <<< HandleInner)
        ]
      , HH.hr_
      ]

  eval
    :: H.HalogenQ f (Action o) i
    ~> H.HalogenM' (State i) (Action o) (ChildSlots f o) o m
  eval = case _ of
    H.Initialize a -> pure a
    H.Finalize a -> pure a
    H.Receive i a -> H.modify_ (_ { input = i }) $> a
    H.Handle msg a ->
      a <$ case msg of
        Toggle -> H.modify_ (\state -> state { on = not state.on })
        Reset -> void $ H.query _child unit $ H.action (set false)
        HandleInner o -> H.raise o
    H.Request q ->
      H.query _child unit q >>= case _ of
        Nothing ->
          throwError (Exn.error "HOC inner component query failed (this should be impossible)")
        Just a ->
          pure a
