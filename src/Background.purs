module ExampleWebExt.Background where

import Prelude

import Browser.WebExt.BrowserAction (onClickedAddListener)
import Browser.WebExt.Listener (mkListener)
import Browser.WebExt.Port (Port)
import Browser.WebExt.Runtime (MessageSender(..))
import Browser.WebExt.Tabs (Tab)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (debug, error, log, logShow)
import ExampleWebExt.NativeMessage (NativeMessage(..), connectToNativeApplication, onNativeDisconnectAddListener, onNativeMessageAddListener, sendMessageToNative)
import ExampleWebExt.RuntimeMessage (RuntimeMessage(..), onRuntimeMessageAddListener, sendMessageToContent)
import ExampleWebExt.Storage (clearAllJobs, getJobsPath, storeJob)
import LinkedIn.Jobs.JobOffer (JobOffer(..))
import LinkedIn.Output.Types (Output(..))
import LinkedIn.UI.Basic.Types (JobFlexibility(..))

main :: Effect Unit
main = do
  log "[bg] starting up"
  port <- connectToNativeApplication "job_search_background"
  onNativeMessageAddListener port nativeMessageHandler
  onNativeDisconnectAddListener port \_ -> log "disconnected from native"

  sendConfigurationToNative port

  onClickedAddListener $ mkListener browserActionOnClickedHandler
  onRuntimeMessageAddListener $ contentScriptMessageHandler port

browserActionOnClickedHandler :: Tab -> Effect Unit
browserActionOnClickedHandler tab = do
  logShow tab
  _ <- sendMessageToContent tab.id RuntimeMessageRequestPageContent
  pure unit

contentScriptMessageHandler ∷ Port -> RuntimeMessage -> MessageSender → Effect Unit
contentScriptMessageHandler
  port
  (RuntimeMessagePageContent _ (OutJobOffer offer))
  (MessageSender {tab: Just {url, title}}) =
    case maybeMsg offer of
      Just msg -> sendMessageToNative port msg
      Nothing -> error "Job offer sent by content script could not be sent"

  where
    maybeMsg (JobOffer jo) = ado
      location <- jo.location
      flexibility <- jo.flexibility
      companyDomain <- jo.companyDomain
      _companySize <- jo.companySize
    in NativeMessageVisitedJobPage {
      url: url,
      jobTitle: jo.title,
      pageTitle: title,
      company: jo.companyName,
      companyDomain,
      companyUrl: jo.companyLink,
      location,
      hasSimplifiedProcess: jo.hasSimplifiedApplicationProcess,
      flexibility: case flexibility of
        JobFlexOnSite -> "on_site"
        JobFlexHybrid -> "hybrid"
        JobFlexFullRemote -> "full_remote"
    }

contentScriptMessageHandler _ m (MessageSender {tab, id}) = do
  let
    senderMsg = case tab of
      Just {url} -> "tab " <> url
      Nothing -> "unknown " <> id
    msg = "[bg] received " <> show m <> " from " <> senderMsg

  debug msg

nativeMessageHandler ∷ NativeMessage → Effect Unit
nativeMessageHandler (NativeMessageJobOfferList {job_offers}) = do
  clearAllJobs
  for_ job_offers \jo -> do
    storeJob jo

nativeMessageHandler m = logShow m

sendConfigurationToNative ∷ Port → Effect Unit
sendConfigurationToNative port = launchAff_ do
  path <- getJobsPath
  case path of
    Left l' -> log $ "Could not read value of jobsPath : " <> printJsonDecodeError l'
    Right path' -> liftEffect $ sendMessageToNative port $ NativeMessageInitialConfiguration {jobsPath: path'}
