module ExampleWebExt.Background where

import Prelude

import Browser.Runtime (Tab, mkListener, onClickedAddListener, onMessageAddListener, tabsSendMessage)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "[bg] starting up"

  onClickedAddListener $ mkListener browserActionOnClickedHandler
  onMessageAddListener $ mkListener contentScriptMessageHandler

browserActionOnClickedHandler :: Tab -> Effect Unit
browserActionOnClickedHandler tab = do
  _ <- tabsSendMessage tab.id "Clicked browser action"
  pure unit

contentScriptMessageHandler ∷ ∀ m (a ∷ Type). MonadEffect m ⇒ Show a ⇒ a → m Unit
contentScriptMessageHandler m = log $ "[bg] received msg from content : " <> show m
