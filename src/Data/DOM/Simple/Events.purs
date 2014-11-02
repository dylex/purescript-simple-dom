module Data.DOM.Simple.Events where

import Control.Monad
import Data.Function
import Data.Maybe

import Data.DOM.Simple.Types
import Data.DOM.Simple.Object
import Data.DOM.Simple.Window(document, globalWindow)
import Data.DOM.Simple.Ajax
import Data.DOM.Simple.Unsafe.Object
import Data.DOM.Simple.Unsafe.Events
import Data.DOM.Simple.Unsafe.Utils

class (Show t) <= EventType t where
  read :: String -> t

instance eventTypeString :: EventType String where
  read s = s

class (Object a) <= EventTarget a

{- Generic properties and methods available on all events. -}

class (Object e, EventType t) <= Event e t where
  asMouseEvent :: e -> Maybe DOMMouseEvent
  asKeyboardEvent :: e -> Maybe DOMKeyboardEvent

eventType :: forall eff e t. (EventType t, Event e t) => e -> DOMEff eff t
eventType e = read <$> unsafeObjectProp "type" e

addEventListener :: forall eff e t b. (EventType t, Event e t, EventTarget b) => t -> (e -> DOMEff eff Unit) -> b -> DOMEff eff (DOMEventListener e)
addEventListener t f a = runFn3 unsafeAddEventListener a (show t) f

removeEventListener :: forall eff e t b. (EventType t, Event e t, EventTarget b) => t -> DOMEventListener e -> b -> DOMEff eff Unit
removeEventListener t l a = runFn3 unsafeRemoveEventListener a (show t) l

eventTarget :: forall eff e. (Event e) => e -> DOMEff eff DOMEventTarget
eventTarget = unsafeObjectProp "target"

stopPropagation :: forall eff e. (Event e) => e -> DOMEff eff Unit
stopPropagation = unsafeStopPropagation

preventDefault :: forall eff e. (Event e) => e -> DOMEff eff Unit
preventDefault = unsafePreventDefault

instance eventObject :: Object DOMEvent

instance eventEvent :: Event DOMEvent String where
  asMouseEvent = ensure <<< unsafeAsMouseEvent
  asKeyboardEvent = ensure <<< unsafeAsKeyboardEvent

class (Event e t) <= UIEvent e t

{- Mouse Events -}

data MouseEventType = MouseMoveEvent | MouseOverEvent | MouseEnterEvent
                    | MouseOutEvent | MouseLeaveEvent

instance mouseEventTypeShow :: Show MouseEventType where
  show MouseMoveEvent   = "mousemove"
  show MouseOverEvent   = "mouseover"
  show MouseEnterEvent  = "mouseenter"
  show MouseOutEvent    = "mouseout"
  show MouseLeaveEvent  = "mouseleave"

instance mouseEventType :: EventType MouseEventType where
  read "mousemove"   = MouseMoveEvent
  read "mouseover"   = MouseOverEvent
  read "mouseenter"  = MouseEnterEvent
  read "mouseout"    = MouseOutEvent
  read "mouseleave"  = MouseLeaveEvent

instance mouseEventObject :: Object DOMMouseEvent

instance mouseEvent :: Event DOMMouseEvent MouseEventType where
  asMouseEvent e = Just e
  asKeyboardEvent _ = Nothing

instance mouseUIEvent :: UIEvent DOMMouseEvent MouseEventType

screenX :: forall eff e. DOMMouseEvent -> DOMEff eff Number
screenX = unsafeObjectProp "screenX"

screenY :: forall eff e. DOMMouseEvent -> DOMEff eff Number
screenY = unsafeObjectProp "screenY"


{- Keyboard Events -}

data KeyboardEventType = KeydownEvent | KeypressEvent | KeyupEvent

instance keyboardEventTypeShow :: Show KeyboardEventType where
  show KeydownEvent     = "keydown"
  show KeypressEvent    = "keypress"
  show KeyupEvent       = "keyup"

instance keyboardEventType :: EventType KeyboardEventType where
  read "keydown"  = KeydownEvent
  read "keypress" = KeypressEvent
  read "keyup"    = KeyupEvent

instance keyboardEventObject :: Object DOMKeyboardEvent

instance keyboardEvent :: Event DOMKeyboardEvent KeyboardEventType where
  asKeyboardEvent e = Just e
  asMouseEvent _ = Nothing

instance keyboardUIEvent :: UIEvent DOMKeyboardEvent KeyboardEventType where

data KeyLocation = KeyLocationStandard | KeyLocationLeft | KeyLocationRight | KeyLocationNumpad

toKeyLocation :: Number -> KeyLocation
toKeyLocation 0 = KeyLocationStandard
toKeyLocation 1 = KeyLocationLeft
toKeyLocation 2 = KeyLocationRight
toKeyLocation 3 = KeyLocationNumpad

key         :: forall eff. DOMKeyboardEvent -> DOMEff eff String
key = unsafeEventKey

keyCode     :: forall eff. DOMKeyboardEvent -> DOMEff eff Number
keyCode = unsafeObjectProp "keyCode"

keyLocation :: forall eff. DOMKeyboardEvent -> DOMEff eff KeyLocation
keyLocation ev = toKeyLocation <$> unsafeObjectProp "keyLocation" ev

altKey      :: forall eff. DOMKeyboardEvent -> DOMEff eff Boolean
altKey e = coerceBoolean <$> unsafeObjectProp "altKey" e

ctrlKey     :: forall eff. DOMKeyboardEvent -> DOMEff eff Boolean
ctrlKey e = coerceBoolean <$> unsafeObjectProp "ctrlKey" e

metaKey     :: forall eff. DOMKeyboardEvent -> DOMEff eff Boolean
metaKey e = coerceBoolean <$> unsafeObjectProp "metaKey" e

shiftKey    :: forall eff. DOMKeyboardEvent -> DOMEff eff Boolean
shiftKey e = coerceBoolean <$> unsafeObjectProp "shiftKey" e

{- UI Events -}

-- XXX this is slightly ham-handed, since
-- <http://www.w3.org/TR/DOM-Level-3-Events/#interface-UIEvent>
-- specifies that only some kinds of elements are valid targets for
-- each of these events.  Might make to refactor more carefully based
-- on what targets can accept what handlers.

data UIEventType = LoadEvent | UnloadEvent | AbortEvent
                 | ErrorEvent | SelectEvent | ResizeEvent
                 | ScrollEvent

instance uiEventTypeShow :: Show UIEventType where
  show LoadEvent    = "load"
  show UnloadEvent  = "unload"
  show AbortEvent   = "abort"
  show ErrorEvent   = "error"
  show SelectEvent  = "select"
  show ResizeEvent  = "resize"
  show ScrollEvent  = "scroll"

instance uiEventType :: EventType UIEventType where
  read "load"     = LoadEvent
  read "unload"   = UnloadEvent
  read "abort"    = AbortEvent
  read "error"    = ErrorEvent
  read "select"   = SelectEvent
  read "resize"   = ResizeEvent
  read "scroll"   = ScrollEvent

instance uiEventObject :: Object DOMUIEvent

instance uiEvent :: Event DOMUIEvent UIEventType where
  asKeyboardEvent _ = Nothing
  asMouseEvent _ = Nothing

instance uiUIEvent :: UIEvent DOMUIEvent UIEventType

-- XXX this should really be returning an HTMLAbstractView...
view   :: forall eff e. (UIEvent e) => e -> DOMEff eff HTMLWindow
view = unsafeObjectProp "view"

detail :: forall eff e. (UIEvent e) => e -> DOMEff eff Number
detail = unsafeObjectProp "detail"

{-
instance eventTargetXMLHttpRequest :: EventTarget XMLHttpRequest where
  addEventListener = unsafeAddEventListener
  removeEventListener = unsafeRemoveEventListener
-}

{-
ready :: forall t ta. (DOMEff t Unit) -> (DOMEff ta Unit)
ready cb = document globalWindow >>= (addEventListener "DOMContentLoaded" $ \_ -> cb)
-}
