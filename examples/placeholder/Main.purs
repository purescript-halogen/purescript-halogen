module Example.Placeholder where

import Data.Tuple

import Control.Monad.Eff

import DOM

import Halogen
import Halogen.Signal
import Halogen.Internal.VirtualDOM (VTree(), vtext)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

import qualified Halogen.Themes.Bootstrap3 as B

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node

data Placeholder = Placeholder String

view :: forall r. H.HTML Placeholder r
view = H.div (A.class_ B.container)
          [ H.h1_ [ H.text "placeholder" ]
          , H.placeholder (Placeholder "This text rendered by renderPlaceholder.")
          ]
  
-- | Convert placeholders into `VTrees`.
-- |
-- | In a real application, this function might render a third party component using `widget`.
renderPlaceholder :: Placeholder -> VTree
renderPlaceholder (Placeholder s) = vtext s  
  
main = do
  Tuple node _ <- runUIEff (pure view) renderPlaceholder (\_ _ -> return unit)
  appendToBody node
  
