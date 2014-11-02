module Data.DOM.Simple.Types where

import Control.Monad.Eff

foreign import data DOM :: !
type DOMEff eff = Eff (dom :: DOM | eff)

foreign import data DOMObject :: *
foreign import data DOMEventTarget :: *
foreign import data DOMEvent :: *
foreign import data DOMUIEvent :: *
foreign import data DOMMouseEvent :: *
foreign import data DOMKeyboardEvent :: *
foreign import data DOMEventListener :: * -> *

foreign import data HTMLElement       :: *
foreign import data HTMLDocument      :: *
foreign import data HTMLWindow        :: *
foreign import data XMLHttpRequest    :: *
foreign import data DOMLocation       :: *
foreign import data JavascriptContext :: *
foreign import data Timeout           :: *
