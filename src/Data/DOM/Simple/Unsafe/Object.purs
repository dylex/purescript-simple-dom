module Data.DOM.Simple.Unsafe.Object 
  ( unsafeAsObject
  , unsafeObjectProp
  ) where

import Data.Function
import Data.DOM.Simple.Types

foreign import unsafeAsObject """
  function unsafeAsObject(obj) {
    return obj;
  }""" :: forall o. o -> DOMObject

foreign import unsafeObjectPropFn """
  function unsafeObjectPropFn(obj, prop) {
    return function () {
      return obj[prop];
    };
  }""" :: forall eff o a. Fn2 o String (DOMEff eff a)

unsafeObjectProp :: forall eff o a. String -> o -> DOMEff eff a
unsafeObjectProp p o = runFn2 unsafeObjectPropFn o p
