module ExampleWebExt.Background where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn2)
import Promise (Promise)

type Tab = { id :: Int, index :: Int }
type TabId = Int
type Message = String

type Listener a = EffectFn1 a Unit

foreign import onClickedAddListener :: Listener Tab -> Effect Unit
foreign import tabsSendMessage :: EffectFn2 TabId Message (Promise Message)

main :: Effect Unit
main = do
  let
    listenerEff = mkEffectFn1 $ \e -> do
      logShow e
      log "Executed listener mkEffectFn1"
      _ <- runEffectFn2 tabsSendMessage e.id "message from PS"
      pure unit

  log "[bg] starting up"

  onClickedAddListener listenerEff

  log "[bg] registered"
