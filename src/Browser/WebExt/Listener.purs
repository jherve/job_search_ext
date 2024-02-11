module Browser.WebExt.Listener (
  Listener,
  mkListener
) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)

type Listener a = EffectFn1 a Unit

mkListener :: forall a. (a -> Effect Unit) -> Listener a
mkListener = mkEffectFn1
