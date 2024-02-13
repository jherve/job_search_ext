module Browser.WebExt.Port (Port, postMessage, onMessageAddListener, onDisconnectAddListener) where

import Prelude

import Browser.WebExt.Listener (Listener)
import Browser.WebExt.Message (Message)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

type Port = {name :: String, error :: String}

foreign import postMessageImpl :: EffectFn2 Port Message Unit
foreign import onMessageAddListenerImpl :: EffectFn2 Port (Listener Message) Unit
foreign import onDisconnectAddListenerImpl :: EffectFn2 Port (Listener Port) Unit

postMessage ∷ Port → Message → Effect Unit
postMessage = runEffectFn2 postMessageImpl

onMessageAddListener ∷ Port -> Listener Message → Effect Unit
onMessageAddListener = runEffectFn2 onMessageAddListenerImpl

onDisconnectAddListener ∷ Port -> Listener Port → Effect Unit
onDisconnectAddListener = runEffectFn2 onDisconnectAddListenerImpl