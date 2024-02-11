module Browser.WebExt.Message (Message, mkMessage, displayMessage, unwrapMessage) where

import Data.Argonaut.Core (Json, stringifyWithIndent)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either)

type Message = Json

mkMessage ∷ ∀ (@a ∷ Type). EncodeJson a ⇒ a → Json
mkMessage = encodeJson

displayMessage ∷ Message → String
displayMessage = stringifyWithIndent 2

unwrapMessage ∷ ∀ (@a ∷ Type). DecodeJson a ⇒ Json → Either JsonDecodeError a
unwrapMessage = decodeJson
