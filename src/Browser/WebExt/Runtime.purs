module Browser.WebExt.Runtime (
  Application,
  MessageSender(..),
  MessageSenderObj,
  sendMessage,
  onMessageAddListener,
  connectNative
) where

import Prelude

import Browser.WebExt.Listener (Listener2)
import Browser.WebExt.Message (Message)
import Browser.WebExt.Port (Port)
import Browser.WebExt.Tabs (Tab)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (error)
import Effect.Uncurried (EffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2)
import Promise (Promise)

type Application = String
type MessageSenderJS = Json
type MessageSenderObj = {
  id :: String,
  url :: Maybe String,
  tab :: Maybe Tab
}
data MessageSender = MessageSender MessageSenderObj
derive instance Generic MessageSender _
instance Show MessageSender where show = genericShow
instance DecodeJson MessageSender where decodeJson = genericDecodeJson

foreign import onMessageAddListenerImpl :: EffectFn1 (Listener2 Message MessageSenderJS) Unit
foreign import runtimeSendMessageImpl :: EffectFn1 Message (Promise Message)
foreign import connectNativeImpl :: EffectFn1 Application Port

onMessageAddListener ∷ Listener2 Message MessageSender → Effect Unit
onMessageAddListener f = runEffectFn1 onMessageAddListenerImpl $ mkEffectFn2 decodeAndListen
  where
    decodeAndListen m send = case decodeJson @MessageSenderObj send of
      Right val -> runEffectFn2 f m $ MessageSender val
      Left l -> error $ printJsonDecodeError l

sendMessage ∷ Message → Effect (Promise Message)
sendMessage = runEffectFn1 runtimeSendMessageImpl

connectNative ∷ Application → Effect Port
connectNative = runEffectFn1 connectNativeImpl
