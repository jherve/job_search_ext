module LinkedIn.Profile.Project where

import Prelude

import Data.Either (Either, note)
import Data.Foldable (findMap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import LinkedIn.DetachedNode (DetachedNode)
import LinkedIn.ArtDecoCard (ArtDecoCardElement, toCenterContent, toHeaderBold, toHeaderNormal)
import LinkedIn.Profile.Utils (maybeExtractFromMaybe, maybeGetInList, toUIElement)
import LinkedIn.UIElements.Types (TimeSpan, UIElement(..))

data Project = Project {
  name :: String,
  timeSpan :: Maybe TimeSpan,
  description :: Maybe String
}

derive instance Generic Project _
instance Show Project where
  show = genericShow

fromUI ∷ ArtDecoCardElement DetachedNode → Either String Project
fromUI card = ado
    name <- note "No position found" $ findMap extractName bold
  in
    Project {
    name,
    timeSpan: maybeExtractFromMaybe extractTimeSpan normal,
    description: maybeGetInList extractDescription content 0
  }
  where
    asUI = toUIElement <$> card
    normal = toHeaderNormal asUI
    content = toCenterContent asUI
    bold = toHeaderBold asUI

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
