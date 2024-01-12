module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  _dom <- getBrowserDom
  log "[content] starting up"
  