module Browser.WebExt.BrowserAction (
  onClickedAddListener,
  setBadgeText,
  setBadgeBackgroundColor
) where

import Prelude

import Browser.WebExt.Listener (Listener)
import Browser.WebExt.Tabs (Tab, TabId)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

foreign import onClickedAddListenerImpl :: EffectFn1 (Listener Tab) Unit
foreign import setBadgeTextImpl :: EffectFn2 String TabId Unit
foreign import setBadgeBackgroundColorImpl :: EffectFn2 String TabId Unit

onClickedAddListener ∷ Listener Tab → Effect Unit
onClickedAddListener = runEffectFn1 onClickedAddListenerImpl

setBadgeText :: String -> TabId -> Effect Unit
setBadgeText = runEffectFn2 setBadgeTextImpl

setBadgeBackgroundColor :: String -> TabId -> Effect Unit
setBadgeBackgroundColor = runEffectFn2 setBadgeBackgroundColorImpl
