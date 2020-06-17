module Test.TestDriver where

import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff.Driver as AD
import Halogen.HTML as HH

render :: forall a p i. a -> HH.HTML p i
render _ = HH.text ""

newtype TestRenderState s act ps o = TestRenderState Unit

runUI
  :: forall f i o
   . H.Component f i o Aff
  -> i
  -> Aff (H.HalogenIO f o Aff)
runUI = AD.runUI
  { render: \_ _ _ _ -> pure (TestRenderState unit)
  , renderChild: identity
  , removeChild: const (pure unit)
  , dispose: const (pure unit)
  }
