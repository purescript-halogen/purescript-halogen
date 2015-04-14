-- | This module defines types for common DOM events

module Halogen.HTML.Events.Types where

import Data.DOM.Simple.Types

-- | This record synonym captures the properties which appear on every DOM event.
-- |
-- | The `fields` type parameter allows us to attach different types of additional
-- | properties to represent more specific types of events.
type Event fields = 
  { bubbles :: Boolean
  , cancelable :: Boolean
  , currentTarget :: HTMLElement
  , target :: HTMLElement
  , timeStamp :: Number
  , "type" :: String
  | fields
  }
  
-- | Identifies the additional fields which are available on mouse events.
type MouseEvent = 
  ( button :: Number
  , detail :: Number
  , relatedTarget :: HTMLElement
  , clientX :: Number
  , clientY :: Number
  , screenX	:: Number
  , screenY	:: Number
  , ctrlKey	:: Boolean
  , shiftKey :: Boolean
  , altKey :: Boolean
  , metaKey	:: Boolean
  , which :: Number
  )
  
-- | Identifies the additional fields which are available on keyboard events.
type KeyboardEvent = 
  ( charCode :: Number
  , keyCode :: Number
  , ctrlKey	:: Boolean
  , shiftKey :: Boolean
  , altKey :: Boolean
  , metaKey	:: Boolean
  , which :: Number
  )
  
-- | Identifies the additional fields which are available on focus events.
type FocusEvent = 
  ( relatedTarget :: HTMLElement
  )
