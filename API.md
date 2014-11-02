# Module Documentation

## Module Data.DOM.Simple.Ajax

### Types

    data ArrayBuffer :: *

    data ArrayBufferView :: *

    data Blob :: *

    data FormData :: *

    data HttpData a where
      NoData :: HttpData a
      TextData :: String -> HttpData a
      ArrayBufferData :: ArrayBuffer -> HttpData a
      ArrayBufferViewData :: ArrayBufferView -> HttpData a
      BlobData :: Blob -> HttpData a
      FormData :: FormData -> HttpData a
      DocumentData :: HTMLDocument -> HttpData a
      JsonData :: a -> HttpData a

    data HttpMethod where
      GET :: HttpMethod
      POST :: HttpMethod
      PUT :: HttpMethod
      DELETE :: HttpMethod
      PATCH :: HttpMethod
      HEAD :: HttpMethod
      OPTIONS :: HttpMethod
      JSONP :: HttpMethod
      HttpMethod :: String -> HttpMethod

    data ReadyState where
      Unsent :: ReadyState
      Opened :: ReadyState
      HeadersReceived :: ReadyState
      Loading :: ReadyState
      Done :: ReadyState

    data ResponseType where
      Default :: ResponseType
      ArrayBuffer :: ResponseType
      Blob :: ResponseType
      Document :: ResponseType
      Json :: ResponseType
      Text :: ResponseType
      MozBlob :: ResponseType
      MozChunkedText :: ResponseType
      MozChunkedArrayBuffer :: ResponseType

    type Url  = String


### Type Class Instances

    instance showHttpMethod :: Show HttpMethod

    instance showResponseType :: Show ResponseType


### Values

    getAllResponseHeaders :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) String

    getResponseHeader :: forall eff. String -> XMLHttpRequest -> Eff (dom :: DOM | eff) (Maybe String)

    makeXMLHttpRequest :: forall eff. Eff (dom :: DOM | eff) XMLHttpRequest

    onReadyStateChange :: forall eff e. Eff e Unit -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit

    open :: forall eff. HttpMethod -> Url -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit

    overrideMimeType :: forall eff. String -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit

    readyState :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) ReadyState

    response :: forall eff a. XMLHttpRequest -> Eff (dom :: DOM | eff) (HttpData a)

    responseText :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) String

    responseType :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) ResponseType

    send :: forall eff a. HttpData a -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit

    setRequestHeader :: forall eff. String -> String -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit

    setResponseType :: forall eff. ResponseType -> XMLHttpRequest -> Eff (dom :: DOM | eff) Unit

    status :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) Number

    statusText :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) String


## Module Data.DOM.Simple.Document

### Type Classes

    class Document b where
      title :: forall eff. b -> Eff (dom :: DOM | eff) String
      setTitle :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
      body :: forall eff. b -> Eff (dom :: DOM | eff) HTMLElement
      setBody :: forall eff. HTMLElement -> b -> Eff (dom :: DOM | eff) Unit
      documentElement :: forall eff. b -> Eff (dom :: DOM | eff) HTMLElement


### Type Class Instances

    instance htmlDocument :: Document HTMLDocument

    instance htmlDocumentElement :: Element HTMLDocument

    instance showHtmlDocument :: Show HTMLDocument


## Module Data.DOM.Simple.Element

### Type Classes

    class Element b where
      getElementById :: forall eff. String -> b -> Eff (dom :: DOM | eff) (Maybe HTMLElement)
      getElementsByClassName :: forall eff. String -> b -> Eff (dom :: DOM | eff) [HTMLElement]
      getElementsByName :: forall eff. String -> b -> Eff (dom :: DOM | eff) [HTMLElement]
      querySelector :: forall eff. String -> b -> Eff (dom :: DOM | eff) (Maybe HTMLElement)
      querySelectorAll :: forall eff. String -> b -> Eff (dom :: DOM | eff) [HTMLElement]
      getAttribute :: forall eff. String -> b -> Eff (dom :: DOM | eff) String
      setAttribute :: forall eff. String -> String -> b -> Eff (dom :: DOM | eff) Unit
      hasAttribute :: forall eff. String -> b -> Eff (dom :: DOM | eff) Boolean
      removeAttribute :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
      children :: forall eff. b -> Eff (dom :: DOM | eff) [HTMLElement]
      innerHTML :: forall eff. b -> Eff (dom :: DOM | eff) String
      setInnerHTML :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
      textContent :: forall eff. b -> Eff (dom :: DOM | eff) String
      setTextContent :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
      contentWindow :: forall eff. b -> Eff (dom :: DOM | eff) HTMLWindow
      classRemove :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
      classAdd :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
      classToggle :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
      classContains :: forall eff. String -> b -> Eff (dom :: DOM | eff) Boolean


### Type Class Instances

    instance htmlElement :: Element HTMLElement

    instance showHtmlElement :: Show HTMLElement


### Values

    blur :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit

    click :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit

    focus :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit

    setAttributes :: forall eff a. (Element a) => [T.Tuple String String] -> a -> Eff (dom :: DOM | eff) Unit


## Module Data.DOM.Simple.Encode

### Values

    decodeURI :: String -> String

    decodeURIComponent :: String -> String

    encodeURI :: String -> String

    encodeURIComponent :: String -> String

    paramatize :: forall a. a -> String

    toJsonString :: forall eff a. a -> Eff (dom :: DOM | eff) String


## Module Data.DOM.Simple.Events

### Types

    data KeyLocation where
      KeyLocationStandard :: KeyLocation
      KeyLocationLeft :: KeyLocation
      KeyLocationRight :: KeyLocation
      KeyLocationNumpad :: KeyLocation

    data KeyboardEventType where
      KeydownEvent :: KeyboardEventType
      KeypressEvent :: KeyboardEventType
      KeyupEvent :: KeyboardEventType

    data MouseEventType where
      MouseMoveEvent :: MouseEventType
      MouseOverEvent :: MouseEventType
      MouseEnterEvent :: MouseEventType
      MouseOutEvent :: MouseEventType
      MouseLeaveEvent :: MouseEventType

    data UIEventType where
      LoadEvent :: UIEventType
      UnloadEvent :: UIEventType
      AbortEvent :: UIEventType
      ErrorEvent :: UIEventType
      SelectEvent :: UIEventType
      ResizeEvent :: UIEventType
      ScrollEvent :: UIEventType


### Type Classes

    class (Object e, EventType t) <= Event e t where
      asMouseEvent :: e -> Maybe DOMMouseEvent
      asKeyboardEvent :: e -> Maybe DOMKeyboardEvent

    class (Object a) <= EventTarget a where

    class (Show t) <= EventType t where
      read :: String -> t

    class (Event e t) <= UIEvent e t where


### Type Class Instances

    instance eventEvent :: Event DOMEvent String

    instance eventObject :: Object DOMEvent

    instance eventTypeString :: EventType String

    instance keyboardEvent :: Event DOMKeyboardEvent KeyboardEventType

    instance keyboardEventObject :: Object DOMKeyboardEvent

    instance keyboardEventType :: EventType KeyboardEventType

    instance keyboardEventTypeShow :: Show KeyboardEventType

    instance keyboardUIEvent :: UIEvent DOMKeyboardEvent KeyboardEventType

    instance mouseEvent :: Event DOMMouseEvent MouseEventType

    instance mouseEventObject :: Object DOMMouseEvent

    instance mouseEventType :: EventType MouseEventType

    instance mouseEventTypeShow :: Show MouseEventType

    instance mouseUIEvent :: UIEvent DOMMouseEvent MouseEventType

    instance uiEvent :: Event DOMUIEvent UIEventType

    instance uiEventObject :: Object DOMUIEvent

    instance uiEventType :: EventType UIEventType

    instance uiEventTypeShow :: Show UIEventType

    instance uiUIEvent :: UIEvent DOMUIEvent UIEventType


### Values

    addEventListener :: forall eff e t b. (EventType t, Event e t, EventTarget b) => t -> (e -> DOMEff eff Unit) -> b -> DOMEff eff (DOMEventListener e)

    altKey :: forall eff. DOMKeyboardEvent -> DOMEff eff Boolean

    ctrlKey :: forall eff. DOMKeyboardEvent -> DOMEff eff Boolean

    detail :: forall eff e. (UIEvent e) => e -> DOMEff eff Number

    eventTarget :: forall eff e. (Event e) => e -> DOMEff eff DOMEventTarget

    eventType :: forall eff e t. (EventType t, Event e t) => e -> DOMEff eff t

    key :: forall eff. DOMKeyboardEvent -> DOMEff eff String

    keyCode :: forall eff. DOMKeyboardEvent -> DOMEff eff Number

    keyLocation :: forall eff. DOMKeyboardEvent -> DOMEff eff KeyLocation

    metaKey :: forall eff. DOMKeyboardEvent -> DOMEff eff Boolean

    preventDefault :: forall eff e. (Event e) => e -> DOMEff eff Unit

    removeEventListener :: forall eff e t b. (EventType t, Event e t, EventTarget b) => t -> DOMEventListener e -> b -> DOMEff eff Unit

    screenX :: forall eff e. DOMMouseEvent -> DOMEff eff Number

    screenY :: forall eff e. DOMMouseEvent -> DOMEff eff Number

    shiftKey :: forall eff. DOMKeyboardEvent -> DOMEff eff Boolean

    stopPropagation :: forall eff e. (Event e) => e -> DOMEff eff Unit

    toKeyLocation :: Number -> KeyLocation

    view :: forall eff e. (UIEvent e) => e -> DOMEff eff HTMLWindow


## Module Data.DOM.Simple.Node

### Type Classes

    class (EventTarget a) <= Node a where


## Module Data.DOM.Simple.Object

### Type Classes

    class Object a where


### Type Class Instances

    instance objectObject :: Object DOMObject


### Values

    asObject :: forall o. (Object o) => o -> DOMObject


## Module Data.DOM.Simple.Sugar

### Type Classes

    class DOMArrows b where
      (#<-) :: forall eff. b -> Tuple String String -> Eff (dom :: DOM | eff) Unit
      (<-#) :: forall eff. b -> String -> Eff (dom :: DOM | eff) String
      (<-?) :: forall eff. b -> String -> Eff (dom :: DOM | eff) (Maybe HTMLElement)
      (%<-) :: forall eff. b -> String -> Eff (dom :: DOM | eff) Unit
      (@<-) :: forall eff. b -> String -> Eff (dom :: DOM | eff) Unit


### Type Class Instances

    instance arrowsEffHTMLElement :: (Element a) => DOMArrows (Eff eff a)

    instance arrowsHTMLElement :: (Element a) => DOMArrows a

    instance arrowsMaybeHTMLElement :: (Element a) => DOMArrows (Maybe a)


## Module Data.DOM.Simple.Types

### Types

    data DOM :: !

    type DOMEff eff = Eff (dom :: DOM | eff)

    data DOMEvent :: *

    data DOMEventListener :: * -> *

    data DOMEventTarget :: *

    data DOMKeyboardEvent :: *

    data DOMLocation :: *

    data DOMMouseEvent :: *

    data DOMObject :: *

    data DOMUIEvent :: *

    data HTMLDocument :: *

    data HTMLElement :: *

    data HTMLWindow :: *

    data JavascriptContext :: *

    data Timeout :: *

    data XMLHttpRequest :: *


## Module Data.DOM.Simple.Unsafe.Ajax

### Values

    unsafeGetResponseHeader :: forall eff a. Fn2 XMLHttpRequest String (Eff (dom :: DOM | eff) String)

    unsafeOnReadyStateChange :: forall eff e. Fn2 XMLHttpRequest (Eff e Unit) (Eff (dom :: DOM | eff) Unit)

    unsafeOpen :: forall eff. Fn3 XMLHttpRequest String String (Eff (dom :: DOM | eff) Unit)

    unsafeReadyState :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) Number

    unsafeResponse :: forall eff a. XMLHttpRequest -> Eff (dom :: DOM | eff) a

    unsafeResponseType :: forall eff. XMLHttpRequest -> Eff (dom :: DOM | eff) String

    unsafeSend :: forall eff. Fn1 XMLHttpRequest (Eff (dom :: DOM | eff) Unit)

    unsafeSendWithPayload :: forall eff a. Fn2 XMLHttpRequest a (Eff (dom :: DOM | eff) Unit)

    unsafeSetResponseType :: forall eff. Fn2 XMLHttpRequest String (Eff (dom :: DOM | eff) Unit)


## Module Data.DOM.Simple.Unsafe.Document

### Values

    unsafeBody :: forall eff a. a -> Eff (dom :: DOM | eff) HTMLElement

    unsafeSetBody :: forall eff a. HTMLElement -> a -> Eff (dom :: DOM | eff) Unit

    unsafeSetTitle :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit

    unsafeTitle :: forall eff a. a -> Eff (dom :: DOM | eff) String


## Module Data.DOM.Simple.Unsafe.Element

### Values

    unsafeBlur :: forall eff a. a -> Eff (dom :: DOM | eff) Unit

    unsafeChildren :: forall eff a. a -> Eff (dom :: DOM | eff) [HTMLElement]

    unsafeClassAdd :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit

    unsafeClassContains :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Boolean

    unsafeClassRemove :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit

    unsafeClassToggle :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit

    unsafeClick :: forall eff a. a -> Eff (dom :: DOM | eff) Unit

    unsafeContentWindow :: forall eff a. a -> Eff (dom :: DOM | eff) HTMLWindow

    unsafeFocus :: forall eff a. a -> Eff (dom :: DOM | eff) Unit

    unsafeGetAttribute :: forall eff a. String -> a -> Eff (dom :: DOM | eff) String

    unsafeGetElementById :: forall eff a. String -> a -> Eff (dom :: DOM | eff) HTMLElement

    unsafeGetElementsByClassName :: forall eff a. String -> a -> Eff (dom :: DOM | eff) [HTMLElement]

    unsafeGetElementsByName :: forall eff a. String -> a -> Eff (dom :: DOM | eff) [HTMLElement]

    unsafeHasAttribute :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Boolean

    unsafeInnerHTML :: forall eff a. a -> Eff (dom :: DOM | eff) String

    unsafeQuerySelector :: forall eff a. String -> a -> Eff (dom :: DOM | eff) HTMLElement

    unsafeQuerySelectorAll :: forall eff a. String -> a -> Eff (dom :: DOM | eff) [HTMLElement]

    unsafeRemoveAttribute :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit

    unsafeSetAttribute :: forall eff a. String -> String -> a -> Eff (dom :: DOM | eff) Unit

    unsafeSetInnerHTML :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit

    unsafeSetTextContent :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit

    unsafeTextContent :: forall eff a. a -> Eff (dom :: DOM | eff) String


## Module Data.DOM.Simple.Unsafe.Events

### Values

    unsafeAddEventListener :: forall eff t e b. Fn3 b String (e -> DOMEff t Unit) (DOMEff eff (DOMEventListener e))

    unsafeAsKeyboardEvent :: forall o e. o -> e

    unsafeAsMouseEvent :: forall o e. o -> e

    unsafeEventKey :: forall eff e. e -> DOMEff eff String

    unsafePreventDefault :: forall eff e. e -> DOMEff eff Unit

    unsafeRemoveEventListener :: forall eff e b. Fn3 b String (DOMEventListener e) (DOMEff eff Unit)

    unsafeStopPropagation :: forall eff e. e -> DOMEff eff Unit


## Module Data.DOM.Simple.Unsafe.Object

### Values

    unsafeAsObject :: forall o. o -> DOMObject

    unsafeObjectProp :: forall eff o a. String -> o -> DOMEff eff a


## Module Data.DOM.Simple.Unsafe.Sugar

### Values

    dirtyKindDomRecast :: forall eff effn a. (Element a) => Eff eff a -> Eff (dom :: DOM | effn) a


## Module Data.DOM.Simple.Unsafe.Utils

### Values

    coerceBoolean :: forall a. a -> Boolean

    ensure :: forall a. a -> Maybe a

    ensureFn :: forall a b. Fn3 b (a -> b) a b

    showImpl :: forall a. a -> String


## Module Data.DOM.Simple.Unsafe.Window

### Values

    unsafeClearTimeout :: forall eff b. b -> Timeout -> Eff (dom :: DOM | eff) Unit

    unsafeDocument :: forall eff a. a -> Eff (dom :: DOM | eff) HTMLDocument

    unsafeGetLocation :: forall eff a. a -> Eff (dom :: DOM | eff) String

    unsafeGetSearchLocation :: forall eff a. a -> Eff (dom :: DOM | eff) String

    unsafeInnerHeight :: forall eff b. b -> Eff (dom :: DOM | eff) Number

    unsafeInnerWidth :: forall eff b. b -> Eff (dom :: DOM | eff) Number

    unsafeLocation :: forall eff a. a -> Eff (dom :: DOM | eff) DOMLocation

    unsafeSetInterval :: forall eff b. b -> Number -> Eff eff Unit -> Eff (dom :: DOM | eff) Timeout

    unsafeSetLocation :: forall eff a. String -> a -> Eff (dom :: DOM | eff) Unit

    unsafeSetTimeout :: forall eff b. b -> Number -> Eff eff Unit -> Eff (dom :: DOM | eff) Timeout


## Module Data.DOM.Simple.Window

### Type Classes

    class Location b where
      getLocation :: forall eff. b -> Eff (dom :: DOM | eff) String
      setLocation :: forall eff. String -> b -> Eff (dom :: DOM | eff) Unit
      search :: forall eff. b -> Eff (dom :: DOM | eff) String

    class Window b where
      document :: forall eff. b -> Eff (dom :: DOM | eff) HTMLDocument
      location :: forall eff. b -> Eff (dom :: DOM | eff) DOMLocation
      setTimeout :: forall eff. b -> Number -> Eff eff Unit -> Eff (dom :: DOM | eff) Timeout
      setInterval :: forall eff. b -> Number -> Eff eff Unit -> Eff (dom :: DOM | eff) Timeout
      clearTimeout :: forall eff. b -> Timeout -> Eff (dom :: DOM | eff) Unit
      innerWidth :: forall eff. b -> Eff (dom :: DOM | eff) Number
      innerHeight :: forall eff. b -> Eff (dom :: DOM | eff) Number


### Type Class Instances

    instance domLocation :: Location DOMLocation

    instance htmlWindow :: Window HTMLWindow


### Values

    getLocationValue :: String -> String -> Maybe String

    globalWindow :: HTMLWindow