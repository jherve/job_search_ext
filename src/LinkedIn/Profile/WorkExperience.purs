module LinkedIn.Profile.WorkExperience where

import LinkedIn.UIElements.Parser
import Prelude

import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import LinkedIn (DetachedNode(..))
import LinkedIn.ArtDecoCard (ArtDecoCardElement(..), ArtDecoCenter(..), ArtDecoCenterContent(..), ArtDecoCenterHeader(..), ArtDecoPvsEntity(..), ArtDecoPvsEntitySubComponent(..))
import LinkedIn.UIElements.Types (UIElement(..))
import Parsing (ParseError, runParser)

data WorkExperience = WorkExperience {
  position :: String,
  company :: Maybe String,
  timeSpan :: Maybe String,
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
        bold: bold,
        normal,
        light
      },
      content: ArtDecoCenterContent subComponents
    }
  }
}) = note "Oops" wep where
  subC = map (\(ArtDecoPvsEntitySubComponent c) -> c) subComponents
  desc = NEL.head subC

  wep = ado
    p <- toText bold
  in WorkExperience {
    position: p,
    company: normal >>= toText,
    timeSpan: Nothing,
    description: toText desc
  }

toUIElement ∷ DetachedNode → Either ParseError UIElement
toUIElement (DetachedElement {content}) = runParser content uiElementP
toUIElement (DetachedComment str) = runParser str uiElementP
toUIElement (DetachedText str) = runParser str uiElementP

toText ∷ DetachedNode → Maybe String
toText el = case toUIElement el of
  Right (UIPlainText str) -> Just str
  Right (UIDotSeparated (UIPlainText str) _) -> Just str
  Right (UIDotSeparated _ (UIPlainText str)) -> Just str
  _ -> Nothing
