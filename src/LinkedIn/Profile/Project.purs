module LinkedIn.Profile.Project where

import Prelude

import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import LinkedIn.ArtDecoCard (ArtDecoCardElement, toCenterContent, toHeaderBold, toHeaderNormal)
import LinkedIn.UIElements.Types (TimeSpan, UIElement(..), UIString(..))

data Project = Project {
  name :: String,
  timeSpan :: Maybe TimeSpan,
  description :: Maybe String
}

derive instance Generic Project _
instance Show Project where
  show = genericShow

fromUI ∷ ArtDecoCardElement UIElement → Either String Project
fromUI card = ado
    name <- note "No position found" $ extractName bold
  in
    Project {
    name,
    timeSpan: extractTimeSpan =<< normal,
    description: extractDescription =<< L.index content 0
  }
  where
    normal = toHeaderNormal card
    content = toCenterContent card
    bold = toHeaderBold card

extractName :: UIElement -> Maybe String
extractName = case _ of
  UIElement (UIStringPlain str) -> Just str
  _ -> Nothing

extractTimeSpan ∷ UIElement → Maybe TimeSpan
extractTimeSpan = case _ of
  UIElement (UIStringTimeSpan s) -> Just s
  UIElement (UIStringDotSeparated (UIStringTimeSpan s) _) -> Just s
  _ -> Nothing

extractDescription ∷ UIElement → Maybe String
extractDescription = case _ of
  UIElement (UIStringPlain d) -> Just d
  _ -> Nothing
