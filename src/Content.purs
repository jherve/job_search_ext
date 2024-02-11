module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Browser.WebExt.Runtime (mkListener, onMessageAddListener, runtimeSendMessage)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import LinkedIn (extractFromDocument, getContext)

main :: Effect Unit
main = do
  log "[content] starting up"

  onMessageAddListener $ mkListener messageListener
  _ <- runtimeSendMessage "message from content"

  dom <- getBrowserDom
  getContext dom >>= logShow
  extractFromDocument dom >>= logShow

messageListener ∷ ∀ a. Show a ⇒ a → Effect Unit
messageListener m = log $ "[content] Received message " <> show m
