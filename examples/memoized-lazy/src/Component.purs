module Example.MemoizedLazy.Component where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Example.MemoizedLazy.Fibonacci (renderFib)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML as HP
import Halogen.HTML.Events as HE

type Slot p = H.Slot (Const Void) Void p

type Input = { fibInt :: Int }

type State = { color :: String, fibInt :: Int }

data Action = ToggleColor | HandleInput Input

color1 = "antiquewhite" :: String
color2 = "darkseagreen" :: String

normalComponent :: forall q o m. H.Component q Input o m
normalComponent =
  H.mkComponent
    { initialState
    , render: HH.memoized eq render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< HandleInput
        }
    }
  where
  render state =
    HH.p
      [ colorStyle state ]
      [ HH.p_ [ HH.text "I am not using 'memoized' or 'lazy', so I am slow." ]
      , HH.p_ [ HH.strong_ [ renderFib state.fibInt ] ]
      , toggleButton
      ]

memoizedComponent :: forall q o m. H.Component q Input o m
memoizedComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< HandleInput
        }
    }
  where
  renderFibMemoized = HH.memoized eq renderFib

  render state = do
    HH.p
      [ colorStyle state ]
      [ HH.p_ [ HH.text "I am using 'memoized' on 'renderFib' using an 'eq' comparison." ]
      , HH.p_ [ HH.strong_ [ renderFibMemoized state.fibInt ] ]
      , toggleButton
      ]

lazyComponent :: forall q o m. H.Component q Input o m
lazyComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< HandleInput
        }
    }
  where
  render state =
    HH.p
      [ colorStyle state ]
      [ HH.p_ [ HH.text "I am using 'lazy' on 'renderFib', which uses reference equality." ]
      , HH.p_ [ HH.strong_ [ HH.lazy renderFib state.fibInt ] ]
      , toggleButton
      ]

toggleButton :: forall w. HH.HTML w Action
toggleButton =
  HH.button
    [ HE.onClick \_ -> ToggleColor ]
    [ HH.text "Toggle" ]

colorStyle :: forall r i. State -> HH.IProp r i
colorStyle state =
  HP.attr (HH.AttrName "style") ("background-color: " <> state.color <> ";")

initialState :: Input -> State
initialState { fibInt } = { fibInt, color: color1 }

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleInput { fibInt } ->
    H.modify_ _ { fibInt = fibInt }
  ToggleColor -> do
    { color } <- H.get
    if color == color1 then
      H.modify_ _ { color = color2 }
    else
      H.modify_ _ { color = color1 }
