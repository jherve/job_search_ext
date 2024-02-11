module ExampleWebExt.Background where

import Prelude

import Browser.WebExt.BrowserAction (onClickedAddListener)
import Browser.WebExt.Listener (mkListener)
import Browser.WebExt.Message (mkMessage)
import Browser.WebExt.Runtime (onMessageAddListener)
import Browser.WebExt.Tabs (Tab)
import Browser.WebExt.Tabs as Tabs
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)
import ExampleWebExt.RuntimeMessage (RuntimeMessage, mkRuntimeMessageHandler)

main :: Effect Unit
main = do
  log "[bg] starting up"

  onClickedAddListener $ mkListener browserActionOnClickedHandler
  onMessageAddListener $ mkRuntimeMessageHandler contentScriptMessageHandler

browserActionOnClickedHandler :: Tab -> Effect Unit
browserActionOnClickedHandler tab = do
  _ <- Tabs.sendMessage tab.id $ mkMessage { clicked: tab.id }
  pure unit

contentScriptMessageHandler ∷ ∀ m. MonadEffect m => RuntimeMessage → m Unit
contentScriptMessageHandler m = logShow m
