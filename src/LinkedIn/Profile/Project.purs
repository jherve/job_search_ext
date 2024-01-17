module LinkedIn.Profile.Project where

import LinkedIn.Profile.Utils
import LinkedIn.UIElements.Parser
import Prelude

import Data.Either (Either, note)
import Data.Foldable (findMap)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Debug (trace)
import LinkedIn (DetachedNode)
import LinkedIn.ArtDecoCard (ArtDecoCardElement(..), ArtDecoCenter(..), ArtDecoCenterContent(..), ArtDecoCenterHeader(..), ArtDecoPvsEntity(..), ArtDecoPvsEntitySubComponent(..))
import LinkedIn.UIElements.Types (Duration, TimeSpan, UIElement(..))
import Parsing (ParseError)

data Project = Project {
  name :: String,
  timeSpan :: Maybe TimeSpan,
  description :: Maybe String
}

derive instance Generic Project _
instance Show Project where
  show = genericShow

fromUI ∷ ArtDecoCardElement → Either String Project
fromUI (ArtDecoCardElement {
  pvs_entity: ArtDecoPvsEntity {
    center: ArtDecoCenter {
      header: ArtDecoCenterHeader {
        bold,
        normal,
        light
      },
      content: ArtDecoCenterContent subComponents
    }
  }
}) = ado
    name <- note "No position found" $ findMap extractName bold'
  in
    Project {
    name,
    timeSpan: maybeExtractFromMaybe extractTimeSpan normal',
    description: maybeGetInList extractDescription content' 0
  } where
  bold' = toUIElement bold

  content' :: List (Either ParseError UIElement)
  content' = map toUIElement subC
    where subC = NEL.catMaybes $ map (\(ArtDecoPvsEntitySubComponent c) -> c) subComponents :: List (DetachedNode)

  normal' :: Maybe (Either ParseError UIElement)
  normal' = toUIElement <$> normal

  light' :: Maybe (NonEmptyList (Either ParseError UIElement))
  light' = (map toUIElement) <$> light

extractName :: UIElement -> Maybe String
extractName = case _ of
  UIPlainText str -> Just str
  _ -> Nothing

extractTimeSpan ∷ UIElement → Maybe TimeSpan
extractTimeSpan = case _ of
  UITimeSpan s -> Just s
  UIDotSeparated (UITimeSpan s) _ -> Just s
  _ -> Nothing

extractDescription ∷ UIElement → Maybe String
extractDescription = case _ of
  UIPlainText d -> Just d
  _ -> Nothing
