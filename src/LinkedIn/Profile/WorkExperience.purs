module LinkedIn.Profile.WorkExperience where

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
import LinkedIn (DetachedNode)
import LinkedIn.ArtDecoCard (ArtDecoCardElement(..), ArtDecoCenter(..), ArtDecoCenterContent(..), ArtDecoCenterHeader(..), ArtDecoPvsEntity(..), ArtDecoPvsEntitySubComponent(..))
import LinkedIn.UIElements.Types (Duration, TimeSpan, UIElement(..))
import LinkedIn.Profile.Utils
import Parsing (ParseError)

data WorkExperience = WorkExperience {
  position :: String,
  company :: Maybe String,
  contractType :: Maybe String,
  timeSpan :: Maybe TimeSpan,
  duration :: Maybe Duration,
  description :: Maybe String
}

derive instance Generic WorkExperience _
instance Show WorkExperience where
  show = genericShow

fromUI ∷ ArtDecoCardElement → Either String WorkExperience
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
    position <- note "No position found" $ findMap extractPosition bold'
  in
    WorkExperience {
    position,
    company: maybeExtractFromMaybe extractCompany normal',
    contractType: maybeExtractFromMaybe extractContractType normal',
    timeSpan: maybeFindInMaybeNEL extractTimeSpan light',
    duration: maybeFindInMaybeNEL extractDuration light',
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

extractPosition :: UIElement -> Maybe String
extractPosition = case _ of
  UIPlainText str -> Just str
  _ -> Nothing

extractCompany ∷ UIElement → Maybe String
extractCompany = case _ of
  UIPlainText str -> Just str
  UIDotSeparated (UIPlainText str) _ -> Just str
  _ -> Nothing

extractContractType ∷ UIElement → Maybe String
extractContractType = case _ of
  UIDotSeparated _ (UIPlainText str) -> Just str
  _ -> Nothing

extractTimeSpan ∷ UIElement → Maybe TimeSpan
extractTimeSpan = case _ of
  UITimeSpan s -> Just s
  UIDotSeparated (UITimeSpan s) _ -> Just s
  _ -> Nothing

extractDuration ∷ UIElement → Maybe Duration
extractDuration = case _ of
  UIDotSeparated _ (UIDuration d) -> Just d
  _ -> Nothing

extractDescription ∷ UIElement → Maybe String
extractDescription = case _ of
  UIPlainText d -> Just d
  _ -> Nothing
