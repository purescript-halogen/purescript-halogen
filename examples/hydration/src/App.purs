module Example.Hydration.App (component) where

import Prelude
import Type.Proxy (Proxy(..))

import Data.Const (Const)
import Example.TextNodes.Elem as Example.TextNodes.Elem
import Example.TextNodes.Keyed as Example.TextNodes.Keyed
import Halogen as H
import Halogen.HTML as HH

type ChildSlots =
  ( elem :: H.Slot (Const Void) Void Unit
  , keyed :: H.Slot (Const Void) Void Unit
  )

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }

-- This test is used to check that components with empty text is not rerendered
-- How I checked this? I added trace and clicked on buttons

-- +++ b/src/Halogen/VDom/DOM.purs
--  patchText = EFn.mkEffectFn2 \state vdom → do
--    let { build, node, value: value1 } = state
--    case vdom of
-- -    Grafted g →
-- +    Grafted g → do
-- +      traceM "rerendered"
--        EFn.runEffectFn2 patchText state (runGraft g)
--      Text value2
-- -      | value1 == value2 →
-- +      | value1 == value2 → do
-- +          traceM "not rerendered"
--            pure $ mkStep $ Step node state patchText haltText
--        | otherwise → do
-- +          traceM "rerendered (otherwise)"
--            let nextState = { build, node, value: value2 }
--            EFn.runEffectFn2 Util.setTextContent value2 node
--            pure $ mkStep $ Step node nextState patchText haltText
--      _ → do
-- +      traceM "rerendered (other case)"
--        EFn.runEffectFn1 haltText state
--        EFn.runEffectFn1 build vdom

-- | the result was this picture https://imgur.com/26f053D

render :: forall m query. Unit -> H.ComponentHTML query ChildSlots m
render _ =
  HH.div_
    [ HH.slot (Proxy :: _ "elem") unit Example.TextNodes.Elem.component unit absurd
    , HH.slot (Proxy :: _ "keyed") unit Example.TextNodes.Keyed.component unit absurd
    ]
