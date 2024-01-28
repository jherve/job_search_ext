module LinkedIn.Profile.Skill where

import Prelude

import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import LinkedIn.ArtDecoTab (ArtDecoTabElement, toHeaderBold)
import LinkedIn.UIElements.Types (UIElement(..), UIString(..))

data Skill = Skill {
  name :: String
}

derive instance Generic Skill _
instance Show Skill where
  show = genericShow

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
