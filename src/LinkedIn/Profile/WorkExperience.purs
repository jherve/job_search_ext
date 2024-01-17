module LinkedIn.Profile.WorkExperience where

import LinkedIn.UIElements.Parser
import Prelude

import Data.Either (Either, hush, note)
import Data.Foldable (class Foldable, findMap)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as L
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import LinkedIn (DetachedNode(..))
import LinkedIn.ArtDecoCard (ArtDecoCardElement(..), ArtDecoCenter(..), ArtDecoCenterContent(..), ArtDecoCenterHeader(..), ArtDecoPvsEntity(..), ArtDecoPvsEntitySubComponent(..))
import LinkedIn.UIElements.Types (Duration, TimeSpan, UIElement(..))
import Parsing (ParseError, runParser)

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

maybeGetInList ::
  ∀ a. (UIElement → Maybe a)
  -> List (Either ParseError UIElement)
  -> Int
  -> Maybe a
maybeGetInList extract idx list = L.index idx list >>= hush >>= extract

maybeExtractFromMaybe ∷
  ∀ a. (UIElement → Maybe a)
  → Maybe (Either ParseError UIElement)
  → Maybe a
maybeExtractFromMaybe extract maybeNode = maybeNode >>= hush >>= extract

maybeFindInMaybeNEL ∷
  ∀ a f. Foldable f ⇒
  (UIElement → Maybe a)
  → Maybe (f (Either ParseError UIElement))
  → Maybe a
maybeFindInMaybeNEL extract = case _ of
  Just nel -> findMap (hush >>> (extract =<< _)) nel
  Nothing -> Nothing

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

toUIElement ∷ DetachedNode → Either ParseError UIElement
toUIElement (DetachedElement {content}) = runParser content uiElementP
toUIElement (DetachedComment str) = runParser str uiElementP
toUIElement (DetachedText str) = runParser str uiElementP
