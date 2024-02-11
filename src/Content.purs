module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Browser.WebExt.Runtime (onMessageAddListener)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import ExampleWebExt.RuntimeMessage (RuntimeMessage(..), mkRuntimeMessageHandler, sendMessageToBackground)
import LinkedIn (extractFromDocument, getContext)

main :: Effect Unit
main = do
  log "[content] starting up"

  onMessageAddListener $ mkRuntimeMessageHandler backgroundMessageHandler
  _ <- sendMessageToBackground RuntimeMessageContentInit

  dom <- getBrowserDom
  getContext dom >>= logShow
  extractFromDocument dom >>= logShow

  ctx <- getContext dom
  case ctx of
    Right ctx' -> do
      _ <- sendMessageToBackground $ RuntimeMessageContext ctx'
      pure unit
    Left _ -> log "Could not send context"

backgroundMessageHandler ∷ ∀ m. MonadEffect m => RuntimeMessage → m Unit
backgroundMessageHandler m = logShow m
