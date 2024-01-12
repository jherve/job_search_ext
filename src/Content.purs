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
  artDecoCards <- getArtDecoCards dom
  artDecoTabs <- getArtDecoTabs dom

  log "[content] starting up"
  logShow artDecoCards
  logShow artDecoTabs
