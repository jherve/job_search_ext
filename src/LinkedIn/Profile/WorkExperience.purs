module LinkedIn.Profile.WorkExperience where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either, note)
import Data.Foldable (findMap)
import Data.Generic.Rep (class Generic)
import Data.List ((:))
import Data.List as L
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Show.Generic (genericShow)
import LinkedIn.UI.Basic.Types (Duration, TimeSpan)
import LinkedIn.UI.Components.ArtDecoCard (ArtDecoCardElement, toCenterContent, toHeaderBold, toHeaderLight, toHeaderNormal)
import LinkedIn.UI.Elements.Types (UIElement(..))
import LinkedIn.UI.Strings.Types (UIString(..))

type WorkExperienceObject = {
  position :: String,
  company :: Maybe String,
  contractType :: Maybe String,
  timeSpan :: Maybe TimeSpan,
  duration :: Maybe Duration,
  description :: Maybe String
}
data WorkExperience = WorkExperience WorkExperienceObject

derive instance Generic WorkExperience _
derive instance Eq WorkExperience
instance Show WorkExperience where show = genericShow
instance EncodeJson WorkExperience where encodeJson a = genericEncodeJson a

fromUI ∷ ArtDecoCardElement UIElement → Either String WorkExperience
fromUI card = ado
    position <- note "No position found" $ extractPosition bold
  in
    WorkExperience {
    position,
    company: extractCompany =<< normal,
    contractType: extractContractType =<< normal,
    timeSpan: findMap extractTimeSpan light,
    duration: findMap extractDuration light,
    description: extractDescription =<< L.index content 0
  }
  where
    normal = toHeaderNormal card
    light = toHeaderLight card
    content = toCenterContent card
    bold = toHeaderBold card

extractPosition :: UIElement -> Maybe String
extractPosition = case _ of
  UIElement (UIStringPlain str) -> Just str
  _ -> Nothing

extractCompany ∷ UIElement → Maybe String
extractCompany = case _ of
  UIElement (UIStringPlain str) -> Just str
  UIElement (UIStringDotSeparated (NonEmptyList(UIStringPlain str :| _))) -> Just str
  _ -> Nothing

extractContractType ∷ UIElement → Maybe String
extractContractType = case _ of
  UIElement (UIStringDotSeparated (NonEmptyList(_ :| UIStringPlain str : _))) -> Just str
  _ -> Nothing

extractTimeSpan ∷ UIElement → Maybe TimeSpan
extractTimeSpan = case _ of
  UIElement (UIStringTimeSpan s) -> Just s
  UIElement (UIStringDotSeparated (NonEmptyList(UIStringTimeSpan s :| _))) -> Just s
  _ -> Nothing

extractDuration ∷ UIElement → Maybe Duration
extractDuration = case _ of
  UIElement (UIStringDotSeparated (NonEmptyList(_ :| UIStringDuration d : _))) -> Just d
  _ -> Nothing

extractDescription ∷ UIElement → Maybe String
extractDescription = case _ of
  UIElement (UIStringPlain d) -> Just d
  _ -> Nothing
