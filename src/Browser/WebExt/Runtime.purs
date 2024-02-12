module Browser.WebExt.Runtime (
  Application,
  sendMessage,
  onMessageAddListener,
  connectNative
) where

import Prelude

import Browser.WebExt.Listener (Listener)
import Browser.WebExt.Message (Message)
import Browser.WebExt.Port (Port)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Promise (Promise)

type Application = String

foreign import onMessageAddListenerImpl :: EffectFn1 (Listener Message) Unit
foreign import runtimeSendMessageImpl :: EffectFn1 Message (Promise Message)
foreign import connectNativeImpl :: EffectFn1 Application Port

onMessageAddListener ∷ Listener Message → Effect Unit
onMessageAddListener = runEffectFn1 onMessageAddListenerImpl

sendMessage ∷ Message → Effect (Promise Message)
sendMessage = runEffectFn1 runtimeSendMessageImpl

connectNative ∷ Application → Effect Port
connectNative = runEffectFn1 connectNativeImpl
