-- | This module re-exports the core types for the `HTML` DSL, and values for
-- | all supported HTML elements.
-- |
-- | Consider using the `Halogen.HTML.Indexed` variety of this module for
-- | better type safety.
module Halogen.HTML
  ( module Halogen.HTML
  , module Halogen.HTML.Core
  , module Halogen.HTML.Elements
  ) where

import Prelude (class Functor, (<$>))

import Data.Lazy (Lazy)
import Data.Maybe (Maybe)

import Halogen.Component (Component, transformChild, ComponentSlot, mkComponentSlot)
import Halogen.Component.ChildPath (ChildPath, injSlot)
import Halogen.HTML.Core (class IsProp, AttrName(..), ClassName(..), EventName(..), Namespace(..), PropName(..), TagName(..), HTML(..), HandlerF(..), Prop(..), PropF(..), attrName, className, element, eventName, fillSlot, handler, namespace, prop, propName, tagName, toPropString)
import Halogen.HTML.Elements (a, a_, abbr, abbr_, acronym, acronym_, address, address_, applet, applet_, area, article, article_, aside, aside_, audio, audio_, b, b_, base, basefont, basefont_, bdi, bdi_, bdo, bdo_, big, big_, blockquote, blockquote_, body, body_, br, br_, button, button_, canvas, caption, caption_, center, center_, cite, cite_, code, code_, col, colgroup, colgroup_, command, datalist, datalist_, dd, dd_, del, del_, details, details_, dfn, dfn_, dialog, dialog_, dir, dir_, div, div_, dl, dl_, dt, dt_, em, em_, embed, embed_, fieldset, fieldset_, figcaption, figcaption_, figure, figure_, font, font_, footer, footer_, form, form_, frame, frame_, frameset, frameset_, h1, h1_, h2, h2_, h3, h3_, h4, h4_, h5, h5_, h6, h6_, head, head_, header, header_, hr, hr_, html, html_, i, i_, iframe, img, input, ins, ins_, kbd, kbd_, keygen, label, label_, legend, legend_, li, li_, link, main, main_, map, map_, mark, mark_, menu, menu_, menuitem, menuitem_, meta, meter, meter_, nav, nav_, noframes, noframes_, noscript, noscript_, object, object_, ol, ol_, optgroup, optgroup_, option, option_, output, output_, p, p_, param, pre, pre_, progress, progress_, q, q_, rp, rp_, rt, rt_, ruby, ruby_, s, s_, samp, samp_, script, script_, section, section_, select, select_, small, small_, source, span, span_, strike, strike_, strong, strong_, style, style_, sub, sub_, summary, summary_, sup, sup_, table, table_, tbody, tbody_, td, td_, textarea, tfoot, tfoot_, th, th_, thead, thead_, time, time_, title, title_, tr, tr_, track, tt, tt_, u, u_, ul, ul_, var, var_, video, video_, wbr)

-- | Constructs a text node `HTML` value.
text :: forall p i. String -> HTML p i
text = Text

-- | Defines a slot for a child component. Takes a slot "address" value and a
-- | thunked constructor.
slot
  :: forall f m p o i
   . p
  -> Lazy (Component HTML f o m)
  -> (o -> Maybe i)
  -> HTML (ComponentSlot HTML f m p i) i
slot p ctor k = Slot (mkComponentSlot p ctor k)

-- | Defines a slot for a child component when a parent has multiple types of
-- | child component. Takes the `ChildPath` for the child component's type, a
-- | slot "address" value and a thunked constructor.
slot'
  :: forall f f' m p p' o i
   . Functor m
  => ChildPath f f' p p'
  -> p
  -> Lazy (Component HTML f o m)
  -> (o -> Maybe i)
  -> HTML (ComponentSlot HTML f' m p' i) i
slot' i p ctor k =
  Slot (mkComponentSlot (injSlot i p) (transformChild i <$> ctor) k)
