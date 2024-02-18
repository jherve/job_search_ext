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
import Data.Int64 as I64
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (debug, error, log, logShow)
import ExampleWebExt.NativeMessage (ApplicationProcess(..), NativeMessage(..), connectToNativeApplication, onNativeDisconnectAddListener, onNativeMessageAddListener, sendMessageToNative)
import ExampleWebExt.RuntimeMessage (RuntimeMessage(..), onRuntimeMessageAddListener, sendMessageToContent)
import ExampleWebExt.Storage (clearAllJobs, getJobsPath, storeJob)
import LinkedIn.Jobs.JobOffer (JobOffer(..))
import LinkedIn.Output.Types (Output(..))
import LinkedIn.PageUrl (PageUrl(..))
import LinkedIn.UI.Basic.Types (JobOfferId(..))
import Web.URL as URL

main :: Effect Unit
main = do
  log "[bg] starting up"
  port <- connectToNativeApplication "job_search_background"

  onNativeMessageAddListener port nativeMessageHandler
  onNativeDisconnectAddListener port \p -> log $ "disconnected from native port " <> p.name <> " (" <> p.error <> ")"

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
  (RuntimeMessagePageContent (UrlJobOffer (JobOfferId jobId)) (OutJobOffer offer))
  (MessageSender {tab: Just {url}}) =
    case maybeMsg offer of
      Just msg -> sendMessageToNative port msg
      Nothing -> error "Job offer sent by content script could not be sent"

  where
    maybeMsg (JobOffer jo) = ado
      url <- cleanUpUrl url
    in NativeMessageAddJob {
        id: "linked_in_" <> I64.toString jobId,
        origin: "linked_in",
        title: jo.title,
        url,
        alternate_url: Nothing,
        company: jo.companyName,
        location: jo.location,
        comment: Nothing,
        company_domain: jo.companyDomain,
        company_url: Just jo.companyLink,
        flexibility: jo.flexibility,
        application_process: Just $ if jo.hasSimplifiedApplicationProcess then ApplicationProcessLinkedInSimplified else ApplicationProcessRegular,
        application_date: Nothing,
        application_rejection_date: Nothing,
        application_considered: Nothing
      }

contentScriptMessageHandler _ m (MessageSender {tab, id}) = do
  let
    senderMsg = case tab of
      Just {url} -> "tab " <> url
      Nothing -> "unknown " <> id
    msg = "[bg] received " <> show m <> " from " <> senderMsg

  debug msg

cleanUpUrl :: String -> Maybe String
cleanUpUrl u = do
  url <- URL.fromAbsolute u
  pure $ URL.toString $ URL.setSearch "" url

nativeMessageHandler ∷ Port -> NativeMessage → Effect Unit
nativeMessageHandler _ (NativeMessageJobOfferList job_offers) = do
  clearAllJobs
  for_ job_offers \jo -> do
    storeJob jo

nativeMessageHandler port NativeMessageStorageReady = sendMessageToNative port $ NativeMessageListJobsRequest
nativeMessageHandler port NativeMessageStorageUpdated = sendMessageToNative port $ NativeMessageListJobsRequest
nativeMessageHandler port (NativeMessageJobAdded _) = sendMessageToNative port $ NativeMessageListJobsRequest
nativeMessageHandler _ m = logShow m

sendConfigurationToNative ∷ Port → Effect Unit
sendConfigurationToNative port = launchAff_ do
  path <- getJobsPath
  case path of
    Left l' -> log $ "Could not read value of jobsPath : " <> printJsonDecodeError l'
    Right path' -> liftEffect $ sendMessageToNative port $ NativeMessageInitialConfiguration {jobsPath: path'}
