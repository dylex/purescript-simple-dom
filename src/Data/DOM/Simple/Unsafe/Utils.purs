module Data.DOM.Simple.Unsafe.Utils 
  ( ensureFn
  , ensure
  , coerceBoolean
  , showImpl
  ) where

import Data.Function
import Data.Maybe

foreign import ensureFn """
  function ensureFn(nothing, just, v) {
    return v == null ? nothing : just(v);
  }""" :: forall a b. Fn3 b (a -> b) a b

ensure :: forall a. a -> Maybe a
ensure = runFn3 ensureFn Nothing Just

foreign import coerceBoolean """
  function coerceBoolean(x) {
    return !!x;
  }""" :: forall a. a -> Boolean

foreign import showImpl
  "function showImpl(v) {   \
  \  return function () {   \
  \    return v.toString(); \
  \  };                     \
  \}" :: forall a. a -> String
