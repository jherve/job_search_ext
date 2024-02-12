module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Browser.WebExt.Runtime (onMessageAddListener)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (logShow, warn)
import Effect.Console (log)
import ExampleWebExt.RuntimeMessage (RuntimeMessage(..), mkRuntimeMessageHandler, sendMessageToBackground)
import LinkedIn (extractFromDocument, getContext)
import LinkedIn.PageUrl (PageUrl(..))

main :: Effect Unit
main = do
  log "[content] starting up"

  dom <- getBrowserDom
  ctx <- getContext dom

  onMessageAddListener $ mkRuntimeMessageHandler backgroundMessageHandler
  _ <- sendMessageToBackground RuntimeMessageContentInit

  case ctx of
    Right (UrlJobOffer _) -> extractDataAndSendToBackground
    Right (UrlListRecommendedJobOffers) -> colorAlreadyVisitedOffers
    Right (UrlSearchJobOffers) -> colorAlreadyVisitedOffers
    _ -> log "[content] Nothing to do"

-- TODO : Implement that function once local storage is updated by background
colorAlreadyVisitedOffers ∷ Effect Unit
colorAlreadyVisitedOffers = log "[content] Coloring of job offers is not implemented yet"

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
