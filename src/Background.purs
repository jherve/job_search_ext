module ExampleWebExt.Background where

import Prelude

import Browser.Runtime (mkListener, onClickedAddListener, onMessageAddListener, tabsSendMessage)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)

main :: Effect Unit
main = do
  let
    listenerEff = mkListener $ \e -> do
      logShow e
      log "Executed listener mkEffectFn1"
      _ <- tabsSendMessage e.id "message from PS"
      pure unit

  log "[bg] starting up"

  onClickedAddListener listenerEff
  onMessageAddListener $ mkListener contentScriptMessageHandler

  log "[bg] registered"

contentScriptMessageHandler ∷ ∀ m (a ∷ Type). MonadEffect m ⇒ Show a ⇒ a → m Unit
contentScriptMessageHandler m = log $ "[bg] received msg from content : " <> show m
