module Browser.WebExt.Runtime (
  sendMessage,
  onMessageAddListener
) where

import Prelude

import Browser.WebExt.Listener (Listener)
import Browser.WebExt.Message (Message)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Promise (Promise)

foreign import onMessageAddListenerImpl :: EffectFn1 (Listener Message) Unit
foreign import runtimeSendMessageImpl :: EffectFn1 Message (Promise Message)

onMessageAddListener ∷ Listener Message → Effect Unit
onMessageAddListener = runEffectFn1 onMessageAddListenerImpl

sendMessage ∷ Message → Effect (Promise Message)
sendMessage = runEffectFn1 runtimeSendMessageImpl
