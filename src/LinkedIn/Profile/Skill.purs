module LinkedIn.Profile.Skill where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import LinkedIn.UI.Components.ArtDecoTab (ArtDecoTabElement, toHeaderBold)
import LinkedIn.UI.Elements.Types (UIElement(..))
import LinkedIn.UI.Strings.Types (UIString(..))

type SkillObject = { name :: String }
newtype Skill = Skill SkillObject

derive instance Generic Skill _
derive instance Eq Skill
instance Show Skill where show = genericShow
instance EncodeJson Skill where encodeJson a = genericEncodeJson a

fromUI ∷ ArtDecoTabElement UIElement → Either String Skill
fromUI tab = ado
    name <- note "No position found" $ extractName bold
  in
    Skill { name }
  where
    bold = toHeaderBold tab

extractName :: UIElement -> Maybe String
extractName = case _ of
  UIElement (UIStringPlain str) -> Just str
  _ -> Nothing
