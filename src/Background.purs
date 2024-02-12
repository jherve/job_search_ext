module ExampleWebExt.Background where

import Prelude

import Browser.WebExt.BrowserAction (onClickedAddListener)
import Browser.WebExt.Listener (mkListener)
import Browser.WebExt.Runtime (onMessageAddListener)
import Browser.WebExt.Tabs (Tab)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)
import ExampleWebExt.NativeMessage (NativeMessage(..), connectToNativeApplication, onNativeMessageAddListener, sendMessageToNative)
import ExampleWebExt.RuntimeMessage (RuntimeMessage(..), mkRuntimeMessageHandler, sendMessageToContent)

main :: Effect Unit
main = do
  log "[bg] starting up"
  port <- connectToNativeApplication "job_search_writer"
  onNativeMessageAddListener port nativeMessageHandler

  sendMessageToNative port $ NativeMessageBackground "hello"

  onClickedAddListener $ mkListener browserActionOnClickedHandler
  onMessageAddListener $ mkRuntimeMessageHandler contentScriptMessageHandler

browserActionOnClickedHandler :: Tab -> Effect Unit
browserActionOnClickedHandler tab = do
  logShow tab
  _ <- sendMessageToContent tab.id RuntimeMessageRequestPageContent
  pure unit

contentScriptMessageHandler ∷ ∀ m. MonadEffect m => RuntimeMessage → m Unit
contentScriptMessageHandler m = logShow m

nativeMessageHandler ∷ ∀ m. MonadEffect m ⇒ NativeMessage → m Unit
nativeMessageHandler m = logShow m
