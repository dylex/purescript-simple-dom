module Data.DOM.Simple.Unsafe.Events where

import Data.Function
import Data.DOM.Simple.Types

foreign import unsafeAddEventListener """
  function unsafeAddEventListener(src, targ, cb) {
    return function () {
      function listener(evt) {
        cb(evt)();
      };
      src.addEventListener(targ, listener, false);
      return listener;
    };
  }""" :: forall eff t e b. Fn3 b String (e -> DOMEff t Unit) (DOMEff eff (DOMEventListener e))

foreign import unsafeAsMouseEvent """
  function unsafeAsMouseEvent(obj) {
    return obj instanceof MouseEvent ? obj : null;
  }""" :: forall o e. o -> e

foreign import unsafeAsKeyboardEvent """
  function unsafeAsKeyboardEvent(obj) {
    return obj instanceof KeyboardEvent ? obj : null;
  }""" :: forall o e. o -> e

foreign import unsafeRemoveEventListener """
  function unsafeRemoveEventListener(src, targ, listener) {
    return function () {
      src.removeEventListener(targ, listener, false);
      return {};
    };
  }""" :: forall eff e b. Fn3 b String (DOMEventListener e) (DOMEff eff Unit)

foreign import unsafeStopPropagation
  "function unsafeStopPropagation(event) { \
  \  return function () {           \
  \    event.stopPropagation();      \
  \    return {};                   \
  \  }                              \
  \}" :: forall eff e. e -> DOMEff eff Unit

foreign import unsafePreventDefault
  "function unsafePreventDefault(event) { \
  \  return function () {           \
  \    event.preventDefault();      \
  \    return {};                   \
  \  }                              \
  \}" :: forall eff e. e -> DOMEff eff Unit

-- XXX Wallpaper over the fact that some browsers don't support
-- KeyboardEvent.key yet.  It's a hack, since it doesn't correctly
-- handle modifier keys, etc.
foreign import unsafeEventKey
  "function unsafeEventKey(event) {            \
  \  return function() {                       \
  \    return event.key === undefined          \
  \       ? String.fromCharCode(event.keyCode) \
  \       : event.key;                         \
  \  };                                        \
  \}" :: forall eff e. e -> DOMEff eff String
