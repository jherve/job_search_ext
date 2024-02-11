module ExampleWebExt.Background where

import Prelude

import Browser.WebExt.BrowserAction (onClickedAddListener)
import Browser.WebExt.Listener (mkListener)
import Browser.WebExt.Runtime (onMessageAddListener)
import Browser.WebExt.Tabs (Tab)
import Browser.WebExt.Tabs as Tabs
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
  _ <- Tabs.sendMessage tab.id "Clicked browser action"
  pure unit

contentScriptMessageHandler ∷ ∀ m (a ∷ Type). MonadEffect m ⇒ Show a ⇒ a → m Unit
contentScriptMessageHandler m = log $ "[bg] received msg from content : " <> show m
