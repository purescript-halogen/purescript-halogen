-- | This module provides convenience functions for creating _input groups_.

module Halogen.Themes.Bootstrap3.InputGroup where
    
import Data.Maybe
import Data.Foldable (foldMap)    
    
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
    
import qualified Halogen.Themes.Bootstrap3 as B

-- | Create an input group.
-- |
-- | An input group consists of a control with optional elements placed before and after.
inputGroup :: forall a i. Maybe (H.HTML a i) -> H.HTML a i -> Maybe (H.HTML a i) -> H.HTML a i
inputGroup before ctl after =
  H.div (A.class_ B.inputGroup)
        (foldMap addon before ++ [ctl] ++ foldMap addon after)
  where
  addon :: H.HTML a i -> [H.HTML a i]
  addon el = [ H.span (A.class_ (className el)) [el] ]
    where
    className (H.Element name _ _) | H.runTagName name == "button" = B.inputGroupBtn
    className _ = B.inputGroupAddon