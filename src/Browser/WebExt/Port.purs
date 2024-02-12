module Browser.WebExt.Port (Port, postMessage, onMessageAddListener) where

import Prelude

import Browser.WebExt.Listener (Listener)
import Browser.WebExt.Message (Message)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

foreign import data Port :: Type

foreign import postMessageImpl :: EffectFn2 Port Message Unit
foreign import onMessageAddListenerImpl :: EffectFn2 Port (Listener Message) Unit

postMessage ∷ Port → Message → Effect Unit
postMessage = runEffectFn2 postMessageImpl

onMessageAddListener ∷ Port -> Listener Message → Effect Unit
onMessageAddListener = runEffectFn2 onMessageAddListenerImpl
