module LinkedIn.Profile.Skill where

import LinkedIn.Profile.Utils
import LinkedIn.UIElements.Parser
import Prelude

import Data.Either (Either, note)
import Data.Foldable (findMap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import LinkedIn.ArtDecoTab (ArtDecoTabElement, toUI)
import LinkedIn.UIElements.Types (UIElement(..))

data Skill = Skill {
  name :: String
}

derive instance Generic Skill _
instance Show Skill where
  show = genericShow

fromUI ∷ ArtDecoTabElement → Either String Skill
fromUI (tab) = ado
    name <- note "No position found" $ findMap extractName bold'
  in
    Skill { name }
  where
    {bold'} = toUI tab

extractName :: UIElement -> Maybe String
extractName = case _ of
  UIPlainText str -> Just str
  _ -> Nothing
