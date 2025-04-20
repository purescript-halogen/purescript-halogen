module Halogen.VDom.StringRenderer.RenderComponent (renderComponent) where

import Data.Newtype as Newtype
import Halogen as Halogen
import Halogen.HTML as Halogen.HTML
import Halogen.Component as Halogen.Component
import Halogen.Query.Input as Halogen.Query
import Halogen.VDom (VDom) as HalogenVDom
import Halogen.VDom.DOM.Prop (Prop) as HalogenVDom
import Halogen.VDom.Thunk (runThunk) as HalogenVDom
import Halogen.VDom.DOM.StringRenderer as Halogen.VDom.DOM.StringRenderer

renderComponentImpl :: ∀ query input output m. input -> Halogen.Component query input output m -> String
renderComponentImpl input' =
  Halogen.unComponent
    ( let
        renderComponentSpec :: ∀ state action slots. Halogen.Component.ComponentSpec state query action slots input output m -> String
        renderComponentSpec componentSpec =
          let
            (state :: state) = componentSpec.initialState input'

            (html :: Halogen.HTML.HTML (Halogen.ComponentSlot slots m action) action) = componentSpec.render state

            (vdom :: HalogenVDom.VDom (Array (HalogenVDom.Prop (Halogen.Query.Input action))) (Halogen.ComponentSlot slots m action)) = Newtype.unwrap html
          in
            Halogen.VDom.DOM.StringRenderer.render renderWidget vdom
      in
        renderComponentSpec
    )

renderComponentSlotSpec :: ∀ query input output slots m action. Halogen.Component.ComponentSlotSpec query input output slots m action -> String
renderComponentSlotSpec componentSlotSpec = renderComponentImpl componentSlotSpec.input componentSlotSpec.component

renderWidget :: ∀ slots m action. Halogen.ComponentSlot slots m action -> String
renderWidget slot = case slot of
  Halogen.Component.ComponentSlot componentSlot -> Halogen.unComponentSlot renderComponentSlotSpec componentSlot
  Halogen.Component.ThunkSlot thunk ->
    let
      (html :: Halogen.HTML.HTML (Halogen.ComponentSlot slots m action) action) = HalogenVDom.runThunk thunk

      (vdom :: HalogenVDom.VDom (Array (HalogenVDom.Prop (Halogen.Query.Input action))) (Halogen.ComponentSlot slots m action)) = Newtype.unwrap html
    in
      Halogen.VDom.DOM.StringRenderer.render renderWidget vdom

renderComponent :: ∀ input query output m. Halogen.Component query input output m -> input -> String
renderComponent component input = renderComponentImpl input component
