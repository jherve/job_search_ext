module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Browser.WebExt.Runtime (MessageSender)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow, warn)
import Effect.Console (log)
import ExampleWebExt.RuntimeMessage (RuntimeMessage(..), onRuntimeMessageAddListener, sendMessageToBackground)
import LinkedIn (extractFromDocument, getContext)
import LinkedIn.Loadable (waitFor)
import LinkedIn.PageUrl (PageUrl(..))

-- TODO: This function should be implemented in PS as well
foreign import colorVisitedJobsLoopImpl :: Effect Unit

main :: Effect Unit
main = do
  log "[content] starting up"

  dom <- getBrowserDom
  ctx <- getContext dom

  onRuntimeMessageAddListener backgroundMessageHandler
  _ <- sendMessageToBackground RuntimeMessageContentInit

  case ctx of
    Right (UrlJobOffer _) -> launchAff_ waitForDataAndSend
    Right (UrlListRecommendedJobOffers) -> colorVisitedJobsLoopImpl
    Right (UrlSearchJobOffers) -> colorVisitedJobsLoopImpl
    _ -> log "[content] Nothing to do"

backgroundMessageHandler ∷ RuntimeMessage -> MessageSender → Effect Unit
backgroundMessageHandler msg _ = case msg of
  RuntimeMessageRequestPageContent -> launchAff_ waitForDataAndSend

  m -> logShow m

waitForDataAndSend ∷ Aff Unit
waitForDataAndSend = do
  dom <- liftEffect getBrowserDom
  -- If we are here we know that we are looking for a job offer.
  -- TODO: Remove this dirty hack once "Loadable" typeclass is integrated
  _ <- waitFor 200 50 ".job-details-jobs-unified-top-card__job-insight span[aria-hidden]" dom
  liftEffect extractDataAndSendToBackground

extractDataAndSendToBackground ∷ Effect Unit
extractDataAndSendToBackground = do
  dom <- getBrowserDom
  ctx <- getContext dom
  data_ <- extractFromDocument dom

  case data_, ctx of
    Left err, _ -> warn $ "[content] " <> show err
    _, Left err -> warn $ "[content] " <> show err
    Right data_', Right ctx' -> sendMessageToBackground $ RuntimeMessagePageContent ctx' data_'
