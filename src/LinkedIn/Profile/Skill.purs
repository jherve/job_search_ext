module LinkedIn.Profile.Skill where

import LinkedIn.Profile.Utils
import LinkedIn.UIElements.Parser
import Prelude

import Data.Either (Either, note)
import Data.Foldable (findMap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import LinkedIn.ArtDecoTab (ArtDecoTabElement(..))
import LinkedIn.ArtDeco (ArtDecoCenter(..), ArtDecoCenterHeader(..), ArtDecoPvsEntity(..))
import LinkedIn.UIElements.Types (UIElement(..))

data Skill = Skill {
  name :: String
}

derive instance Generic Skill _
instance Show Skill where
  show = genericShow

fromUI ∷ ArtDecoTabElement → Either String Skill
fromUI (ArtDecoTabElement {
  pvs_entity: ArtDecoPvsEntity {
    center: ArtDecoCenter {
      header: ArtDecoCenterHeader { bold }
    }
  }
}) = ado
    name <- note "No position found" $ findMap extractName bold'
  in
    Skill { name } where
  bold' = toUIElement bold

extractName :: UIElement -> Maybe String
extractName = case _ of
  UIPlainText str -> Just str
  _ -> Nothing
