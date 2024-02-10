module ExampleWebExt.Background where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn1, runEffectFn2)
import Promise (Promise)

type Tab = { id :: Int, index :: Int }
type TabId = Int
type Message = String

type Listener a = EffectFn1 a Unit

foreign import onClickedAddListenerImpl :: EffectFn1 (Listener Tab) Unit
foreign import tabsSendMessageImpl :: EffectFn2 TabId Message (Promise Message)

onClickedAddListener ∷ Listener Tab → Effect Unit
onClickedAddListener = runEffectFn1 onClickedAddListenerImpl

tabsSendMessage ∷ TabId → Message → Effect (Promise Message)
tabsSendMessage = runEffectFn2 tabsSendMessageImpl

main :: Effect Unit
main = do
  let
    listenerEff = mkEffectFn1 $ \e -> do
      logShow e
      log "Executed listener mkEffectFn1"
      _ <- tabsSendMessage e.id "message from PS"
      pure unit

  log "[bg] starting up"

  onClickedAddListener listenerEff

  log "[bg] registered"
