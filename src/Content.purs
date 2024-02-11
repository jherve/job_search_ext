module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Browser.WebExt.Listener (mkListener)
import Browser.WebExt.Message (Message, displayMessage, mkMessage)
import Browser.WebExt.Runtime as Runtime
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import LinkedIn (extractFromDocument, getContext)

main :: Effect Unit
main = do
  log "[content] starting up"

  Runtime.onMessageAddListener $ mkListener messageListener
  _ <- Runtime.sendMessage $ mkMessage { simpleMessage: "hello from content" }

  dom <- getBrowserDom
  getContext dom >>= logShow
  extractFromDocument dom >>= logShow

messageListener ∷ Message → Effect Unit
messageListener m = log $ "[content] Received message " <> displayMessage m
