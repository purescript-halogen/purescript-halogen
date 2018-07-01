module Test.TestDriver where

import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff.Driver as AD

newtype TestRenderProduct p i = TestRenderProduct Unit

render :: forall a p i. a -> TestRenderProduct p i
render _ = TestRenderProduct unit

newtype TestRenderState s act ps o = TestRenderState Unit

runUI
  :: forall f i o
   . H.Component TestRenderProduct f i o Aff
  -> i
  -> Aff (H.HalogenIO f o Aff)
runUI = AD.runUI
  { render: \_ _ _ _ -> pure (TestRenderState unit)
  , renderChild: identity
  , removeChild: const (pure unit)
  , dispose: const (pure unit)
  }
