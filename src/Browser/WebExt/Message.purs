module Browser.WebExt.Message (Message, mkMessage, displayMessage) where

import Data.Argonaut.Core (Json, stringifyWithIndent)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

type Message = Json

mkMessage ∷ ∀ (@a ∷ Type). EncodeJson a ⇒ a → Json
mkMessage = encodeJson

displayMessage ∷ Message → String
displayMessage = stringifyWithIndent 2
