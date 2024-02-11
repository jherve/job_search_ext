module Browser.WebExt.BrowserAction (
  onClickedAddListener
) where

import Prelude

import Browser.WebExt.Listener (Listener)
import Browser.WebExt.Tabs (Tab)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import onClickedAddListenerImpl :: EffectFn1 (Listener Tab) Unit

onClickedAddListener ∷ Listener Tab → Effect Unit
onClickedAddListener = runEffectFn1 onClickedAddListenerImpl
