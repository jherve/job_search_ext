module ExampleWebExt.Background where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Uncurried (EffectFn1, mkEffectFn1)

type Tab = { id :: Int, index :: Int }

type Listener a = EffectFn1 a Unit

foreign import onClickedAddListener :: Listener Tab -> Effect Unit

main :: Effect Unit
main = do
  let
    listenerEff = mkEffectFn1 $ \e -> do
      logShow e
      log "Executed listener mkEffectFn1"

  log "[bg] starting up"

  onClickedAddListener listenerEff

  log "[bg] registered"
