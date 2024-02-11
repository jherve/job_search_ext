module ExampleWebExt.Background where

import Prelude

import Browser.Runtime (mkListener, onClickedAddListener, tabsSendMessage)
import Effect (Effect)
import Effect.Class.Console (log, logShow)

main :: Effect Unit
main = do
  let
    listenerEff = mkListener $ \e -> do
      logShow e
      log "Executed listener mkEffectFn1"
      _ <- tabsSendMessage e.id "message from PS"
      pure unit

  log "[bg] starting up"

  onClickedAddListener listenerEff

  log "[bg] registered"
