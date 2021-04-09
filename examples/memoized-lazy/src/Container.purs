module Example.MemoizedLazy.Container where

import Prelude

import Example.MemoizedLazy.Component as Component
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

data Action = Add Int

type State = { fibInt :: Int }

type ChildSlots =
  ( memoizedLazy :: Component.Slot Int
  )

_memoizedLazy = Proxy :: Proxy "memoizedLazy"

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { fibInt: 30 }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.p_
        [ HH.text "Make the Fibonacci function slower. Currently: "
        , HH.strong_ [ HH.text (show state.fibInt) ]
        , HH.button
            [ HE.onClick \_ -> Add 1 ]
            [ HH.text "Add 1" ]
        , HH.button
            [ HE.onClick \_ -> Add 3 ]
            [ HH.text "Add 3" ]
        , HH.button
            [ HE.onClick \_ -> Add 7 ]
            [ HH.text "Add 7" ]
        ]
    , HH.ul_
        [ HH.slot_ _memoizedLazy 1 Component.normalComponent state
        , HH.slot_ _memoizedLazy 2 Component.memoizedComponent state
        , HH.slot_ _memoizedLazy 3 Component.lazyComponent state
        ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Add n ->
    H.modify_ \state -> state { fibInt = state.fibInt + n }
