module Example.RawHTML.Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.HTML as HH
import Halogen as H

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render: \_ -> template
  , eval: H.mkEval $ H.defaultEval { handleAction = \_ -> pure unit }
  }

template :: forall action m. H.ComponentHTML action (()) m
template = HH.rawHTML "<b>hello world</b>"


-- HH.div [ css "hero min-h-screen bg-base-200" ]
--   [ HH.div [ css "text-center hero-content" ]
--       [ HH.div [ css "max-w-md space-y-5" ]
--           [ HH.h1 [ css "text-5xl font-bold" ] [ HH.text "timey.app" ]
--           , HH.form [ css "space-y-5" ]
--               [ HH.div [ css "form-control" ]
--                   [ HH.input
--                       [ HP.type_ HP.InputText, HP.placeholder "e-mail", css "input input-bordered" ]
--                   ]
--               , HH.div [ css "form-control" ]
--                   [ HH.input
--                       [ HP.type_ HP.InputPassword, HP.placeholder "password", css "input input-bordered" ]
--                   ]
--               , HH.input [ HP.type_ HP.InputSubmit, HP.value "Login", css "btn" ]
--               ]
--           ]
--       ]
--   ]
