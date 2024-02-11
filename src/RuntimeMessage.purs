module ExampleWebExt.RuntimeMessage where

import Prelude

import Browser.Runtime (Listener, mkListener)
import Browser.WebExt.Message (Message, mkMessage, unwrapMessage)
import Browser.WebExt.Runtime as Runtime
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)
import LinkedIn.PageUrl (PageUrl)

data RuntimeMessage =
  RuntimeMessageContentInit
  | RuntimeMessageContext PageUrl

derive instance Generic RuntimeMessage _
instance Show RuntimeMessage where show = genericShow
instance EncodeJson RuntimeMessage where encodeJson a = genericEncodeJson a
instance DecodeJson RuntimeMessage where decodeJson a = genericDecodeJson a

sendMessageToBackground ∷ RuntimeMessage → Effect Unit
sendMessageToBackground msg = do
  _ <- Runtime.sendMessage $ mkMessage msg
  pure unit

decodeRuntimeMessage ∷ Message → Either String RuntimeMessage
decodeRuntimeMessage m =
  case unwrapMessage m of
    Left err -> Left $ printJsonDecodeError err
    Right m' -> Right m'

mkRuntimeMessageHandler ∷ (RuntimeMessage → Effect Unit) → Listener Json
mkRuntimeMessageHandler f = mkListener \m -> do
  case decodeRuntimeMessage m of
    Left err -> log err
    Right m' -> f m'
