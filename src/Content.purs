module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import ExampleWebExt.RuntimeMessage (RuntimeMessage(..), sendMessageToBackground)
import LinkedIn (extractFromDocument, getContext)

main :: Effect Unit
main = do
  log "[content] starting up"

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
