module JobSearchExtension.Background where

import Prelude

import Browser.WebExt.BrowserAction (setBadgeBackgroundColor, setBadgeText)
import Browser.WebExt.Port (Port)
import Browser.WebExt.Runtime (MessageSender(..))
import Browser.WebExt.Tabs (TabId)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int64 as I64
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (debug, error, log, logShow)
import JobSearchExtension.NativeMessage (ApplicationProcess(..), NativeMessage(..), connectToNativeApplication, onNativeDisconnectAddListener, onNativeMessageAddListener, sendMessageToNative)
import JobSearchExtension.RuntimeMessage (RuntimeMessage(..), onRuntimeMessageAddListener)
import JobSearchExtension.Storage (clearAllJobs, getJobsPath, storeJob)
import LinkedIn.Jobs.JobOffer (JobOffer(..))
import LinkedIn.Loadable (sleep)
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
  onRuntimeMessageAddListener $ contentScriptMessageHandler port

  sendConfigurationToNative port

contentScriptMessageHandler ∷ Port -> RuntimeMessage -> MessageSender → Effect Unit
contentScriptMessageHandler
  port
  (RuntimeMessagePageContent (UrlJobOffer (JobOfferId jobId)) (OutJobOffer offer))
  (MessageSender {tab: Just {id, url}}) =
    case maybeMsg offer, maybeCompany offer of
      Just msgJob, Just msgCompany -> do
        sendMessageToNative port msgJob
        sendMessageToNative port msgCompany
        setBadgeOk id

      _, _ -> do
        error "Job offer sent by content script could not be sent"
        setBadgeKo id

  where
    maybeMsg (JobOffer jo) = ado
      url <- cleanUpUrl url
    in NativeMessageAddJob {
        id: "linked_in_" <> I64.toString jobId,
        origin: "linked_in",
        title: jo.title,
        url,
        company: jo.companyName,
        location: jo.location,
        flexibility: jo.flexibility,
        application_process: Just $ if jo.hasSimplifiedApplicationProcess then ApplicationProcessLinkedInSimplified else ApplicationProcessRegular
      }

    maybeCompany (JobOffer jo) = Just $ NativeMessageAddCompany {
        name: jo.companyName,
        domain: jo.companyDomain,
        url: Just jo.companyLink
      }

contentScriptMessageHandler
  _
  (RuntimeMessageError err)
  (MessageSender {tab: Just {id, url}}) = do
    setBadgeKo id
    error $ "tab " <> show url <> " sent an error : " <> show err

contentScriptMessageHandler _ m (MessageSender {tab, id}) = do
  let
    senderMsg = case tab of
      Just {url} -> "tab " <> url
      Nothing -> "unknown " <> id
    msg = "[bg] received " <> show m <> " from " <> senderMsg

  debug msg

setBadgeKo ∷ TabId → Effect Unit
setBadgeKo id = setBadge id "KO" "red"

setBadgeOk ∷ TabId → Effect Unit
setBadgeOk id = setBadge id "OK" "green"

setBadge ∷ TabId → String → String → Effect Unit
setBadge tabId text color = do
  setBadgeText text tabId
  setBadgeBackgroundColor color tabId

cleanUpUrl :: String -> Maybe String
cleanUpUrl u = do
  url <- URL.fromAbsolute u
  pure $ URL.toString $ URL.setSearch "" url

nativeMessageHandler ∷ Port -> NativeMessage → Effect Unit
nativeMessageHandler _ (NativeMessageJobOfferList job_offers) = do
  clearAllJobs
  for_ job_offers \jo -> do
    storeJob jo

nativeMessageHandler _ NativeMessageStorageNotReady = log "[bg] waiting for storage"
nativeMessageHandler port NativeMessageStorageReady = sendMessageToNative port $ NativeMessageListJobsRequest
nativeMessageHandler port NativeMessageStorageUpdated = sendMessageToNative port $ NativeMessageListJobsRequest
nativeMessageHandler port (NativeMessageJobAdded _) = sendMessageToNative port $ NativeMessageListJobsRequest

-- TODO : The actual solution is to make the sender stateful
nativeMessageHandler port (NativeMessageMessageNotProcessed msg) = launchAff_ do
  sleep 500
  liftEffect $ sendMessageToNative port msg

nativeMessageHandler _ m = logShow m

sendConfigurationToNative ∷ Port → Effect Unit
sendConfigurationToNative port = launchAff_ do
  path <- getJobsPath
  case path of
    Left l' -> log $ "Could not read value of jobsPath : " <> printJsonDecodeError l'
    Right path' -> liftEffect $ sendMessageToNative port $ NativeMessageInitialConfiguration {jobsPath: path'}
