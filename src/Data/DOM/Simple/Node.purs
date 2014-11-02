module Data.DOM.Simple.Node where

import Data.DOM.Simple.Types
import Data.DOM.Simple.Events

class (EventTarget a) <= Node a
