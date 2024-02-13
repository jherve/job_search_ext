module Browser.WebExt.Storage.Local where

import Prelude

import Data.Argonaut.Core (Json)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Promise (Promise)
import Promise.Aff (toAffE)

type Key = String

foreign import setImpl :: EffectFn1 Json Unit
foreign import getImpl :: EffectFn1 Key (Promise Json)
foreign import clearImpl :: Effect Unit

set ∷ Json → Effect Unit
set = runEffectFn1 setImpl

get :: Key -> Aff Json
get k = toAffE $ runEffectFn1 getImpl k

clear :: Effect Unit
clear = clearImpl
