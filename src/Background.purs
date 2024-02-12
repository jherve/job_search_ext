module ExampleWebExt.Background where

import Prelude

import Browser.WebExt.BrowserAction (onClickedAddListener)
import Browser.WebExt.Listener (mkListener)
import Browser.WebExt.Message (Message)
import Browser.WebExt.Port (postMessage)
import Browser.WebExt.Port as Port
import Browser.WebExt.Runtime (onMessageAddListener)
import Browser.WebExt.Tabs (Tab)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode.Encoders (encodeString)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)
import ExampleWebExt.NativeMessage (connectToNativeApplication)
import ExampleWebExt.RuntimeMessage (RuntimeMessage(..), mkRuntimeMessageHandler, sendMessageToContent)

main :: Effect Unit
main = do
  log "[bg] starting up"
  port <- connectToNativeApplication "job_search_writer"
  Port.onMessageAddListener port $ mkListener nativeMessageHandler

  postMessage port $ encodeString "hello"

  onClickedAddListener $ mkListener browserActionOnClickedHandler
  onMessageAddListener $ mkRuntimeMessageHandler contentScriptMessageHandler

browserActionOnClickedHandler :: Tab -> Effect Unit
browserActionOnClickedHandler tab = do
  logShow tab
  _ <- sendMessageToContent tab.id RuntimeMessageRequestPageContent
  pure unit

contentScriptMessageHandler ∷ ∀ m. MonadEffect m => RuntimeMessage → m Unit
contentScriptMessageHandler m = logShow m

nativeMessageHandler ∷ ∀ m. MonadEffect m ⇒ Message → m Unit
nativeMessageHandler m = log $ "[bg] Got message from native : " <> stringify m
