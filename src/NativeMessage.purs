module ExampleWebExt.NativeMessage where

import Prelude

import Browser.WebExt.Listener (mkListener)
import Browser.WebExt.Message (Message, mkMessage, unwrapMessage)
import Browser.WebExt.Port (Port, onDisconnectAddListener, onMessageAddListener)
import Browser.WebExt.Port as Port
import Browser.WebExt.Runtime (Application, connectNative)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)

data NativeMessage =
  NativeMessageBackground String
  | NativeMessageLog {level :: String, content :: String}
  | NativeMessageInitialConfiguration {jobsPath :: String}
  | NativeMessageVisitedJobPage {
    url :: String,
    jobTitle :: String,
    pageTitle :: String,
    company :: String,
    companyDomain :: Maybe String,
    companyUrl :: String,
    location :: String,
    hasSimplifiedProcess :: Boolean,
    flexibility :: String
  }
  | NativeMessageJobAlreadyExists {job_id :: String}
  | NativeMessageJobAdded {job :: NativePythonJobOffer}
  | NativeMessageJobOfferList {job_offers :: Array NativePythonJobOffer}

type NativePythonJobOffer = {
  id :: String,
  title :: String,
  url :: String,
  application_date :: Maybe String,
  application_rejection_date :: Maybe String
}
type NativePythonMessage m = {tag :: String | m}
type NativePythonMessageLog = NativePythonMessage (level :: String, content :: String)
type NativePythonMessageInitialConfiguration = NativePythonMessage (jobsPath :: String)
type NativePythonMessageJobAlreadyExists = NativePythonMessage (job_id :: String)
type NativePythonMessageJobOfferList = NativePythonMessage (job_offers :: Array NativePythonJobOffer)
type NativePythonMessageJobAdded = NativePythonMessage (job :: NativePythonJobOffer)

derive instance Generic NativeMessage _
instance Show NativeMessage where show = genericShow
instance EncodeJson NativeMessage where encodeJson a = genericEncodeJson a

-- A function used to transform some messages sent by the native application that are in the form
-- of an object with unknown keys to an array of objects. The long-term solution is probably to 
-- change the format of the native message, but we'll probably need this function as well when
-- we read data storage in storage.local which is stored as a giant object with unknown keys.
foreign import toArrayOfObjects :: String -> Json -> Json

instance DecodeJson NativeMessage where
  decodeJson json = case decodeJson @(NativePythonMessage ()) json of
    Right {tag: "log_message"} ->
      map (\{level, content} -> NativeMessageLog {level, content}) $ decodeJson @NativePythonMessageLog json
    Right {tag: "job_already_exists"} ->
      map (\{job_id} -> NativeMessageJobAlreadyExists {job_id}) $ decodeJson @NativePythonMessageJobAlreadyExists json
    Right {tag: "job_offer_list"} ->
      map (\o -> NativeMessageJobOfferList {job_offers: o.job_offers}) $
      decodeJson @NativePythonMessageJobOfferList $
      toArrayOfObjects "job_offers" json
    Right {tag: "job_added"} ->
      map (\{job} -> NativeMessageJobAdded {job}) $ decodeJson @NativePythonMessageJobAdded json

    Right _r -> Left $ UnexpectedValue json
    Left _ -> genericDecodeJson json

connectToNativeApplication ∷ Application → Effect Port
connectToNativeApplication = connectNative

decodeNativeMessage ∷ Message → Either String NativeMessage
decodeNativeMessage m =
  case unwrapMessage m of
    Left err -> Left $ printJsonDecodeError err
    Right m' -> Right m'

onNativeMessageAddListener ∷ Port → (NativeMessage → Effect Unit) → Effect Unit
onNativeMessageAddListener port f = onMessageAddListener port $ runtimeMessageHandler
  where
    runtimeMessageHandler = mkListener \m -> do
      case decodeNativeMessage m of
        Left err -> log err
        Right m' -> f m'

onNativeDisconnectAddListener :: Port -> (Port -> Effect Unit) -> Effect Unit
onNativeDisconnectAddListener port f = onDisconnectAddListener port $ mkListener f

sendMessageToNative :: Port -> NativeMessage -> Effect Unit
sendMessageToNative port msg = do
  _ <- Port.postMessage port $ mkMessage msg
  pure unit
