-- | The `EventHandler` monad, used to perform standard operations on HTML
-- | events.
module Halogen.HTML.Events.Handler
  ( EventHandler
  , EventUpdate
  , preventDefault
  , stopPropagation
  , stopImmediatePropagation
  , runEventHandler
  , unEventHandler
  ) where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Writer (Writer, runWriter)
import Control.Monad.Writer.Class (tell)

import Data.Foldable (for_)
import Data.Tuple (Tuple(..))

import DOM (DOM)

import Halogen.HTML.Events.Types (Event)

-- | This monad supports the following operations on events:
-- |
-- | - `preventDefault`
-- | - `stopPropagation`
-- | - `stopImmediatePropagation`
-- |
-- | It can be used as follows:
-- |
-- | ```purescript
-- | import Data.Functor (($>))
-- |
-- | H.a [ E.onClick \_ -> E.preventDefault $> clickHandler ]
-- |     [ H.text "Click here" ]
-- | ```
newtype EventHandler a = EventHandler (Writer (Array EventUpdate) a)

data EventUpdate
  = PreventDefault
  | StopPropagation
  | StopImmediatePropagation

unEventHandler :: forall a. EventHandler a -> Writer (Array EventUpdate) a
unEventHandler (EventHandler mw) = mw

-- | Call the `preventDefault` method on the current event.
preventDefault :: EventHandler Unit
preventDefault = EventHandler (tell [PreventDefault])

-- | Call the `stopPropagation` method on the current event.
stopPropagation :: EventHandler Unit
stopPropagation = EventHandler (tell [StopPropagation])

-- | Call the `stopImmediatePropagation` method on the current event.
stopImmediatePropagation :: EventHandler Unit
stopImmediatePropagation = EventHandler (tell [StopImmediatePropagation])

instance functorEventHandler :: Functor EventHandler where
  map f (EventHandler mw) = EventHandler (f <$> mw)

instance applyEventHandler :: Apply EventHandler where
  apply (EventHandler mw1) (EventHandler mw2) = EventHandler (mw1 <*> mw2)

instance applicativeEventHandler :: Applicative EventHandler where
  pure = EventHandler <<< pure

instance bindEventHandler :: Bind EventHandler where
  bind (EventHandler mw) f = EventHandler (mw >>= unEventHandler <<< f)

instance monadEventHandler :: Monad EventHandler

foreign import preventDefaultImpl :: forall eff fields. Event fields -> Eff (dom :: DOM | eff) Unit

foreign import stopPropagationImpl :: forall eff fields. Event fields -> Eff (dom :: DOM | eff) Unit

foreign import stopImmediatePropagationImpl :: forall eff fields. Event fields -> Eff (dom :: DOM | eff) Unit

-- | This function can be used to update an event and return the wrapped value
runEventHandler :: forall a fields m eff. (Monad m, MonadEff (dom :: DOM | eff) m) => Event fields -> EventHandler a -> m a
runEventHandler e (EventHandler mw) =
  case runWriter mw of
    Tuple a eus -> liftEff $ for_ eus (applyUpdate) *> pure a
  where
  applyUpdate :: EventUpdate -> Eff (dom :: DOM | eff) Unit
  applyUpdate PreventDefault            = preventDefaultImpl e
  applyUpdate StopPropagation           = stopPropagationImpl e
  applyUpdate StopImmediatePropagation  = stopImmediatePropagationImpl e
