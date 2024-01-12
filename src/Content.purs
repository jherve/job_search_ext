module ExampleWebExt.Content where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)

import Browser.DOM (getBrowserDom)
import LinkedIn

main :: Effect Unit
main = do
  dom <- fromDocument <$> getBrowserDom
  el1 <- queryOne "h2" dom
  els <- queryAll "h2" dom

  log "[content] starting up"
  logShow el1
  logShow els
