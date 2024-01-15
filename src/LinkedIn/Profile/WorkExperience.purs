module LinkedIn.Profile.WorkExperience where

import LinkedIn.UIElements.Parser
import Prelude

import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Debug (trace, traceM)
import LinkedIn (DetachedNode(..))
import LinkedIn.ArtDecoCard (ArtDecoCardElement(..), ArtDecoCenter(..), ArtDecoCenterContent(..), ArtDecoCenterHeader(..), ArtDecoPvsEntity(..), ArtDecoPvsEntitySubComponent(..))
import Parsing (runParser)

data WorkExperience = WorkExperience {
  position :: String,
  company :: Maybe String,
  timeSpan :: Maybe String,
  description :: Maybe String
}

derive instance Generic WorkExperience _
instance Show WorkExperience where
  show = genericShow

fromUI :: ArtDecoCardElement -> Either String WorkExperience
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
    p <- toContent bold
    c <- toContent <$> normal
  in WorkExperience {position: p, company: c, timeSpan: Nothing, description: toContent desc}

toContent ∷ DetachedNode → Maybe String
toContent (DetachedElement {content}) = Just content
toContent _ = Nothing

toUIElement el = runParser uiElementP el
