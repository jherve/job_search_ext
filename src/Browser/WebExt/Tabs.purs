module Browser.WebExt.Tabs (
  Tab,
  TabId,
  sendMessage
) where

import Browser.WebExt.Message (Message)
import Effect (Effect)
import Effect.Uncurried (EffectFn2,runEffectFn2)
import Promise (Promise)

type Tab = { id :: Int, index :: Int, title :: String, url :: String }
type TabId = Int

foreign import sendMessageImpl :: EffectFn2 TabId Message (Promise Message)

sendMessage ∷ TabId → Message → Effect (Promise Message)
sendMessage = runEffectFn2 sendMessageImpl
