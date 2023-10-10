module Example.HocDirect.Button (component) where

import Prelude

import Data.Array (range)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Debug (spy)
import Effect.Aff (Aff, delay)
import Effect.Class.Console (logShow)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))


component :: forall q i o. H.Component q i o Aff
component =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render: \_ -> HH.slot_ (Proxy :: _ "hoc") unit (hoc parent) unit
    , eval: H.mkEval H.defaultEval
    }


hoc :: forall i o q. H.Component q Unit o Aff -> H.Component q i o Aff
hoc comp =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just unit
        }
    }
  where
  initialState _ = false

  render = spy "hoc st" >>> case _ of
    false -> HH.text "Loading..."
    _ -> HH.slot_ (Proxy :: _ "parent") unit comp unit

  handleAction _ = do 
    liftAff $ delay $ wrap 1000.0
    H.put true
    liftAff $ delay $ wrap 2000.0
    H.put false


parent :: forall i o q. H.Component q i o Aff
parent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just $ Left unit
        }
    }
  where
  initialState _ = 0

  render = case _ of
    1 -> HH.slot_ (Proxy :: _ "child1") unit child1 unit
    2 -> HH.div [] [ HH.slot_ (Proxy :: _ "child2") unit child2 unit, HH.text "other text with 2" ]
    3 -> HH.slot_ (Proxy :: _ "child3") unit child3 unit
    _ -> HH.div [ HE.onClick \_ -> Left unit ] [ HH.text "No child" ]

  handleAction = case _ of
    Right i -> H.put i
    Left _ -> void $ H.fork do
      for_ (range 1 4) \i -> do
        logShow { i }
        liftAff $ delay $ wrap 500.0
        H.put i

child1 :: forall i o m q. H.Component q i o m
child1 =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  initialState _ = unit

  render _ = HH.div_ [ HH.text "Child 1" ]

child2 :: forall i o m q. H.Component q i o m
child2 =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  initialState _ = unit

  render _ = HH.div_ [ HH.text "Child 2" ]

child3 :: forall i o m q. H.Component q i o m
child3 =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  initialState _ = unit

  render _ = HH.div_ [ HH.text "Child 3" ]

