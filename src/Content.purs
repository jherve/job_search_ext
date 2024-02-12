module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Browser.WebExt.Runtime (MessageSender)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (logShow, warn)
import Effect.Console (log)
import ExampleWebExt.RuntimeMessage (RuntimeMessage(..), onRuntimeMessageAddListener, sendMessageToBackground)
import LinkedIn (extractFromDocument, getContext)
import LinkedIn.PageUrl (PageUrl(..))

main :: Effect Unit
main = do
  log "[content] starting up"

  dom <- getBrowserDom
  ctx <- getContext dom

  onRuntimeMessageAddListener backgroundMessageHandler
  _ <- sendMessageToBackground RuntimeMessageContentInit

  case ctx of
    Right (UrlJobOffer _) -> extractDataAndSendToBackground
    Right (UrlListRecommendedJobOffers) -> colorAlreadyVisitedOffers
    Right (UrlSearchJobOffers) -> colorAlreadyVisitedOffers
    _ -> log "[content] Nothing to do"

-- TODO : Implement that function once local storage is updated by background
colorAlreadyVisitedOffers ∷ Effect Unit
colorAlreadyVisitedOffers = log "[content] Coloring of job offers is not implemented yet"

backgroundMessageHandler ∷ RuntimeMessage -> MessageSender → Effect Unit
backgroundMessageHandler m _ = case m of
  RuntimeMessageRequestPageContent -> extractDataAndSendToBackground

  m -> logShow m

extractDataAndSendToBackground ∷ Effect Unit
extractDataAndSendToBackground = do
  dom <- getBrowserDom
  ctx <- getContext dom
  data_ <- extractFromDocument dom
  sendMessageToBackground RuntimeMessageContentInit
  case data_, ctx of
    Left err, _ -> warn $ "[content] " <> show err
    _, Left err -> warn $ "[content] " <> show err
    Right data_', Right ctx' -> sendMessageToBackground $ RuntimeMessagePageContent ctx' data_'
