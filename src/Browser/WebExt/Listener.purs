module Browser.WebExt.Listener (
  Listener,
  Listener2,
  mkListener,
  mkListener2
) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, mkEffectFn2)

type Listener a = EffectFn1 a Unit
type Listener2 a b = EffectFn2 a b Unit

mkListener :: forall a. (a -> Effect Unit) -> Listener a
mkListener = mkEffectFn1

mkListener2 ∷ ∀ a b. (a → b → Effect Unit) → Listener2 a b
mkListener2 = mkEffectFn2
