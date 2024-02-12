module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Browser.WebExt.Runtime (onMessageAddListener)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (logShow, warn)
import Effect.Console (log)
import ExampleWebExt.RuntimeMessage (RuntimeMessage(..), mkRuntimeMessageHandler, sendMessageToBackground)
import LinkedIn (extractFromDocument)

main :: Effect Unit
main = do
  log "[content] starting up"

  onMessageAddListener $ mkRuntimeMessageHandler backgroundMessageHandler
  _ <- sendMessageToBackground RuntimeMessageContentInit

  extractDataAndSendToBackground

backgroundMessageHandler ∷ RuntimeMessage → Effect Unit
backgroundMessageHandler = case _ of
  RuntimeMessageRequestPageContent -> extractDataAndSendToBackground

  m -> logShow m

extractDataAndSendToBackground ∷ Effect Unit
extractDataAndSendToBackground = do
  dom <- getBrowserDom
  data_ <- extractFromDocument dom
  sendMessageToBackground RuntimeMessageContentInit
  case data_ of
    Left err -> warn $ "[content] " <> show err
    Right data_' -> sendMessageToBackground $ RuntimeMessagePageContent data_'
