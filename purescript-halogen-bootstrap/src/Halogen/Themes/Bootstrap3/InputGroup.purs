-- | This module provides convenience functions for creating _input groups_.

module Halogen.Themes.Bootstrap3.InputGroup where
    
import Data.Maybe
import Data.Foldable (foldMap)    
    
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
    
import qualified Halogen.Themes.Bootstrap3 as B

-- | Represents an input group add-on element
-- |
-- | We need to distinguish buttons from regular add-ons because of the 
-- | different CSS classes
data AddOn p i
  = RegularAddOn (H.HTML p i)
  | ButtonAddOn (H.HTML p i)

-- | Create an input group.
-- |
-- | An input group consists of a control with optional elements placed before and after.
inputGroup :: forall p i. Maybe (AddOn p i) -> H.HTML p i -> Maybe (AddOn p i) -> H.HTML p i
inputGroup before ctl after =
  H.div [A.class_ B.inputGroup]
        (foldMap addon before ++ [ctl] ++ foldMap addon after)
  where
  addon :: AddOn p i -> [H.HTML p i]
  addon (RegularAddOn el) = [ H.span [A.class_ B.inputGroupAddon] [el] ]
  addon (ButtonAddOn el) = [ H.span [A.class_ B.inputGroupBtn] [el] ]