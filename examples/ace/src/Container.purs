module Example.Ace.Container where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Example.Ace.AceComponent as AceComponent
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

-- | The application state, which in this case just stores the current text in
-- | the editor.
type State = { text :: String }

-- | The query algebra for the app.
data Action
  = ClearText
  | HandleAceUpdate AceComponent.Output

type ChildSlots =
  ( ace :: AceComponent.Slot Unit
  )

_ace = Proxy :: Proxy "ace"

-- | The main UI component definition.
component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { text: "" }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render { text: text } =
  HH.div_
    [ HH.h1_
        [ HH.text "ace editor" ]
    , HH.div_
        [ HH.p_
            [ HH.button
                [ HE.onClick \_ -> ClearText ]
                [ HH.text "Clear" ]
            ]
        ]
    , HH.div_
        [ HH.slot _ace unit AceComponent.component unit HandleAceUpdate ]
    , HH.p_
        [ HH.text ("Current text: " <> text) ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  ClearText ->
    H.tell _ace unit (AceComponent.ChangeText "")
  HandleAceUpdate msg ->
    handleAceOuput msg

handleAceOuput :: forall o m. MonadAff m => AceComponent.Output -> H.HalogenM State Action ChildSlots o m Unit
handleAceOuput = case _ of
  AceComponent.TextChanged text ->
    H.modify_ (_ { text = text })
