module Browser.WebExt.Runtime (
  Tab,
  TabId,
  Message,
  Listener,
  tabsSendMessage,
  runtimeSendMessage,
  onClickedAddListener,
  onMessageAddListener,
  mkListener
) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2)
import Promise (Promise)

type Tab = { id :: Int, index :: Int }
type TabId = Int
type Message = String

type Listener a = EffectFn1 a Unit

foreign import onClickedAddListenerImpl :: EffectFn1 (Listener Tab) Unit
foreign import tabsSendMessageImpl :: EffectFn2 TabId Message (Promise Message)
foreign import onMessageAddListenerImpl :: EffectFn1 (Listener Message) Unit
foreign import runtimeSendMessageImpl :: EffectFn1 Message (Promise Message)

onClickedAddListener ∷ Listener Tab → Effect Unit
onClickedAddListener = runEffectFn1 onClickedAddListenerImpl

tabsSendMessage ∷ TabId → Message → Effect (Promise Message)
tabsSendMessage = runEffectFn2 tabsSendMessageImpl

onMessageAddListener ∷ Listener Message → Effect Unit
onMessageAddListener = runEffectFn1 onMessageAddListenerImpl

runtimeSendMessage ∷ Message → Effect (Promise Message)
runtimeSendMessage = runEffectFn1 runtimeSendMessageImpl

mkListener :: forall a. (a -> Effect Unit) -> Listener a
mkListener = mkEffectFn1
