-- | This module re-exports the types for the `HTML` DSL, and values for all
-- | supported HTML elements.
module Halogen.HTML
  ( module Halogen.HTML
  , module Halogen.HTML.Core
  , module Halogen.HTML.Elements
  , module Halogen.HTML.Properties
  ) where

import Prelude (Unit, (<<<))

import Data.Functor as F
import Data.Maybe (Maybe(..))

import Halogen.Component (Component, ParentHTML, mkComponentSlot, unComponent)
import Halogen.Component.ChildPath (ChildPath, injSlot, prjQuery, injQuery)
import Halogen.HTML.Core (class IsProp, AttrName(..), ClassName(..), HTML(..), Namespace(..), PropName(..), ElemName(..), text, handler)
import Halogen.HTML.Core as Core
import Halogen.HTML.Elements (Leaf, Node, withKeys, withKeys_, a, a_, abbr, abbr_, address, address_, area, article, article_, aside, aside_, audio, audio_, b, b_, base, bdi, bdi_, bdo, bdo_, blockquote, blockquote_, body, body_, br, br_, button, button_, canvas, caption, caption_, cite, cite_, code, code_, col, colgroup, colgroup_, command, datalist, datalist_, dd, dd_, del, del_, details, details_, dfn, dfn_, dialog, dialog_, div, div_, dl, dl_, dt, dt_, element, elementNS, em, em_, embed, embed_, fieldset, fieldset_, figcaption, figcaption_, figure, figure_, footer, footer_, form, form_, h1, h1_, h2, h2_, h3, h3_, h4, h4_, h5, h5_, h6, h6_, head, head_, header, header_, hr, hr_, html, html_, i, i_, iframe, img, input, ins, ins_, kbd, kbd_, label, label_, legend, legend_, li, li_, link, main, main_, map, map_, mark, mark_, menu, menu_, menuitem, menuitem_, meta, meter, meter_, nav, nav_, noscript, noscript_, object, object_, ol, ol_, optgroup, optgroup_, option, option_, output, output_, p, p_, param, pre, pre_, progress, progress_, q, q_, rp, rp_, rt, rt_, ruby, ruby_, samp, samp_, script, script_, section, section_, select, select_, small, small_, source, span, span_, strong, strong_, style, style_, sub, sub_, summary, summary_, sup, sup_, table, table_, tbody, tbody_, td, td_, textarea, tfoot, tfoot_, th, th_, thead, thead_, time, time_, title, title_, tr, tr_, track, u, u_, ul, ul_, var, var_, video, video_, wbr)
import Halogen.HTML.Properties (IProp, attr, attrNS, prop)

-- | Defines a slot for a child component. Takes:
-- | - the slot "address" value
-- | - the component for the slot
-- | - the input value to pass to the component
-- | - a function mapping outputs from the component to a query in the parent
slot
  :: forall f m p i o g
   . p
  -> Component HTML g i o m
  -> i
  -> (o -> Maybe (f Unit))
  -> ParentHTML f g p m
slot p component input outputQuery =
  let f = unComponent _.receiver component
  in Core.slot (mkComponentSlot p component input f outputQuery Just)

-- | Defines a slot for a child component when a parent has multiple types of
-- | child component. Takes:
-- | - the `ChildPath` for this particular child component type
-- | - the slot "address" value
-- | - the component for the slot
-- | - the input value to pass to the component
-- | - a function mapping outputs from the component to a query in the parent
slot'
  :: forall f g g' p p' i o m
   . ChildPath g g' p p'
  -> p
  -> Component HTML g i o m
  -> i
  -> (o -> Maybe (f Unit))
  -> ParentHTML f g' p' m
slot' i p component input outputQuery =
  let
    pq = prjQuery i
    f = F.map (injQuery i) <<< unComponent _.receiver component
  in
    Core.slot (mkComponentSlot (injSlot i p) component input f outputQuery pq)
