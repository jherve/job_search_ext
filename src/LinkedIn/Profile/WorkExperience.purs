module LinkedIn.Profile.WorkExperience where

import Prelude

import Data.Either (Either, note)
import Data.Foldable (findMap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import LinkedIn.ArtDecoCard (ArtDecoCardElement, toCenterContent, toHeaderBold, toHeaderLight, toHeaderNormal)
import LinkedIn.DetachedNode (DetachedNode)
import LinkedIn.Profile.Utils (maybeExtractFromMaybe, maybeFindInMaybeNEL, maybeGetInList, toUIElement)
import LinkedIn.UIElements.Types (Duration, TimeSpan, UIElement(..), UIString(..))

data WorkExperience = WorkExperience {
  position :: String,
  company :: Maybe String,
  contractType :: Maybe String,
  timeSpan :: Maybe TimeSpan,
  duration :: Maybe Duration,
  description :: Maybe String
}

derive instance Generic WorkExperience _
derive instance Eq WorkExperience
instance Show WorkExperience where
  show = genericShow

fromUI ∷ ArtDecoCardElement DetachedNode → Either String WorkExperience
fromUI (card) = ado
    position <- note "No position found" $ findMap extractPosition bold
  in
    WorkExperience {
    position,
    company: maybeExtractFromMaybe extractCompany normal,
    contractType: maybeExtractFromMaybe extractContractType normal,
    timeSpan: maybeFindInMaybeNEL extractTimeSpan light,
    duration: maybeFindInMaybeNEL extractDuration light,
    description: maybeGetInList extractDescription content 0
  }
  where
    asUI = toUIElement <$> card
    normal = toHeaderNormal asUI
    light = toHeaderLight asUI
    content = toCenterContent asUI
    bold = toHeaderBold asUI

extractPosition :: UIElement -> Maybe String
extractPosition = case _ of
  UIElement (UIStringPlain str) -> Just str
  _ -> Nothing

extractCompany ∷ UIElement → Maybe String
extractCompany = case _ of
  UIElement (UIStringPlain str) -> Just str
  UIElement (UIStringDotSeparated (UIStringPlain str) _) -> Just str
  _ -> Nothing

extractContractType ∷ UIElement → Maybe String
extractContractType = case _ of
  UIElement (UIStringDotSeparated _ (UIStringPlain str)) -> Just str
  _ -> Nothing

extractTimeSpan ∷ UIElement → Maybe TimeSpan
extractTimeSpan = case _ of
  UIElement (UIStringTimeSpan s) -> Just s
  UIElement (UIStringDotSeparated (UIStringTimeSpan s) _) -> Just s
  _ -> Nothing

extractDuration ∷ UIElement → Maybe Duration
extractDuration = case _ of
  UIElement (UIStringDotSeparated _ (UIStringDuration d)) -> Just d
  _ -> Nothing

extractDescription ∷ UIElement → Maybe String
extractDescription = case _ of
  UIElement (UIStringPlain d) -> Just d
  _ -> Nothing
