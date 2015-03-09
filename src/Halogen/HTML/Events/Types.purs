module Halogen.HTML.Events.Types where

import DOM

type Event fields = 
  { bubbles :: Boolean
  , cancelable :: Boolean
  , currentTarget :: Node
  , target :: Node
  , timeStamp :: Number
  , "type" :: String
  | fields
  }
  
type MouseEvent = 
  ( button :: Number
  , detail :: Number
  , relatedTarget :: Node
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

type KeyboardEvent = 
  ( charCode :: Number
  , keyCode :: Number
  , ctrlKey	:: Boolean
  , shiftKey :: Boolean
  , altKey :: Boolean
  , metaKey	:: Boolean
  , which :: Number
  )

type FocusEvent = 
  ( relatedTarget :: Node
  )