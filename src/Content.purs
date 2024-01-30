module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Data.Argonaut.Encode (toJsonString)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import LinkedIn (extractFromDocument, getContext)

main :: Effect Unit
main = do
  log "[content] starting up"

  dom <- getBrowserDom
  getContext dom >>= logShow
  extractFromDocument dom >>= logShow

  extracted <- extractFromDocument dom
  log $ toJsonString extracted
