module Data.DOM.Simple.Object where

import Data.DOM.Simple.Types
import Data.DOM.Simple.Unsafe.Object

class Object a

instance objectObject :: Object DOMObject

asObject :: forall o. (Object o) => o -> DOMObject
asObject = unsafeAsObject
