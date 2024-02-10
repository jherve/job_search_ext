module ExampleWebExt.Background where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)

type Listener0 = Effect Unit
type Listener a = (a -> Effect Unit)

foreign import onClickedAddListener :: Listener0 -> Effect Unit
foreign import onClickedAddListener1 :: forall a. Listener a -> Effect Unit

foreign import mkListenerOne :: forall a. (Listener a) -> Effect (Listener a)

main :: Effect Unit
main = do
  listener <- mkListenerOne $ \e -> do
    log "Clicked PS 1"
  log "[bg] starting up"
  onClickedAddListener $ log "Clicked PS 0"
  onClickedAddListener1 listener
  log "[bg] registered"
