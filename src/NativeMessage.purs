module ExampleWebExt.NativeMessage where

import Prelude

import Browser.WebExt.Listener (mkListener)
import Browser.WebExt.Message (Message, mkMessage, unwrapMessage)
import Browser.WebExt.Port (Port, onDisconnectAddListener, onMessageAddListener)
import Browser.WebExt.Port as Port
import Browser.WebExt.Runtime (Application, connectNative)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), printJsonDecodeError)
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)
import LinkedIn.UI.Basic.Types (JobFlexibility)

data NativeMessage =
  NativeMessageBackground String
  | NativeMessageLog {level :: String, content :: String}
  | NativeMessageInitialConfiguration {jobsPath :: String}
  | NativeMessageStorageReady
  | NativeMessageAddJob NativePythonJobOffer
  | NativeMessageListJobsRequest
  | NativeMessageJobAlreadyExists {job_id :: String}
  | NativeMessageJobAdded {job_id :: String}
  | NativeMessageJobOfferList (Array NativePythonJobOffer)

data ApplicationProcess
  = ApplicationProcessLinkedInSimplified
  | ApplicationProcessRegular
  | ApplicationProcessCareerSite
  | ApplicationProcessSpurious

derive instance Generic ApplicationProcess _
instance Show ApplicationProcess where show = genericShow
instance EncodeJson ApplicationProcess where
  encodeJson = case _ of
    ApplicationProcessLinkedInSimplified -> encodeJson "linked_in_simplified"
    ApplicationProcessRegular -> encodeJson "regular"
    ApplicationProcessCareerSite -> encodeJson "career_site"
    ApplicationProcessSpurious -> encodeJson "spurious"

instance DecodeJson ApplicationProcess where
  decodeJson json = case decodeString json of
    Right "linked_in_simplified" -> Right ApplicationProcessLinkedInSimplified
    Right "regular" -> Right ApplicationProcessRegular
    Right "career_site" -> Right ApplicationProcessCareerSite
    Right "spurious" -> Right ApplicationProcessSpurious
    _ -> Left $ UnexpectedValue json

type NativePythonJobOffer = {
  id :: String,
  origin :: String,
  title :: String,
  url :: String,
  alternate_url :: Maybe String,
  company :: String,
  location :: Maybe String,
  company_domain :: Maybe String,
  company_url :: Maybe String,
  flexibility :: Maybe JobFlexibility,
  comment :: Maybe String,
  application_process :: Maybe ApplicationProcess,
  application_considered :: Maybe Boolean,
  application_date :: Maybe String,
  application_rejection_date :: Maybe String
}

derive instance Generic NativeMessage _
instance Show NativeMessage where show a = genericShow a
instance EncodeJson NativeMessage where encodeJson a = genericEncodeJson a

instance DecodeJson NativeMessage where
  decodeJson json = genericDecodeJson json

connectToNativeApplication ∷ Application → Effect Port
connectToNativeApplication = connectNative

decodeNativeMessage ∷ Message → Either String NativeMessage
decodeNativeMessage m =
  case unwrapMessage m of
    Left err -> Left $ printJsonDecodeError err
    Right m' -> Right m'

onNativeMessageAddListener ∷ Port → (Port -> NativeMessage → Effect Unit) → Effect Unit
onNativeMessageAddListener port f = onMessageAddListener port $ runtimeMessageHandler
  where
    runtimeMessageHandler = mkListener \m -> do
      case decodeNativeMessage m of
        Left err -> log err
        Right m' -> f port m'

onNativeDisconnectAddListener :: Port -> (Port -> Effect Unit) -> Effect Unit
onNativeDisconnectAddListener port f = onDisconnectAddListener port $ mkListener f

sendMessageToNative :: Port -> NativeMessage -> Effect Unit
sendMessageToNative port msg = do
  _ <- Port.postMessage port $ mkMessage msg
  pure unit
