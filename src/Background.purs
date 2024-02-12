module ExampleWebExt.Background where

import Prelude

import Browser.WebExt.BrowserAction (onClickedAddListener)
import Browser.WebExt.Listener (mkListener)
import Browser.WebExt.Port (Port)
import Browser.WebExt.Runtime (onMessageAddListener)
import Browser.WebExt.Tabs (Tab)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import ExampleWebExt.NativeMessage (NativeMessage(..), connectToNativeApplication, onNativeMessageAddListener, sendMessageToNative)
import ExampleWebExt.RuntimeMessage (RuntimeMessage(..), mkRuntimeMessageHandler, sendMessageToContent)
import ExampleWebExt.Storage (getJobsPath, setJobsPath)

main :: Effect Unit
main = do
  log "[bg] starting up"
  port <- connectToNativeApplication "job_search_writer"
  onNativeMessageAddListener port nativeMessageHandler

  setJobsPath "/tmp/dummy/path/value"
  sendConfigurationToNative port

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

sendConfigurationToNative ∷ Port → Effect Unit
sendConfigurationToNative port = launchAff_ do
  path <- getJobsPath
  case path of
    Left l' -> log $ "Could not read value of jobsPath : " <> printJsonDecodeError l'
    Right path' -> liftEffect $ sendMessageToNative port $ NativeMessageInitialConfiguration {jobsPath: path'}
