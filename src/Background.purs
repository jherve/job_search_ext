module ExampleWebExt.Background where

import Prelude

import Effect.Console (log)
import Effect (Effect)

main :: Effect Unit
main = do
  log "[bg] starting up"
  