module ExampleWebExt.NativeMessage where

import Prelude

import Browser.WebExt.Port (Port)
import Browser.WebExt.Runtime (Application, connectNative)
import Effect (Effect)

data NativeMessage =
  NativeMessageBackgroundInit

connectToNativeApplication ∷ Application → Effect Port
connectToNativeApplication = connectNative
