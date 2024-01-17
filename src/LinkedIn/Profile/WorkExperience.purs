module LinkedIn.Profile.WorkExperience where

import LinkedIn.UIElements.Parser
import Prelude

import Data.Either (Either(..), hush, note)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
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
        bold: bold,
        normal,
        light
      },
      content: ArtDecoCenterContent subComponents
    }
  }
}) = ado
    position <- extractPosition bold'
  in
    WorkExperience {
    position,
    company: maybeExtractFromMaybe extractCompany normal',
    contractType: maybeExtractFromMaybe extractContractType normal',
    timeSpan: hush $ extractTimeSpan light',
    duration: hush $ extractDuration light',
    description: hush $ extractDescription content'
  } where
  bold' = toUIElement bold

  content' :: List (Either ParseError UIElement)
  content' = map toUIElement subC
    where subC = NEL.catMaybes $ map (\(ArtDecoPvsEntitySubComponent c) -> c) subComponents :: List (DetachedNode)

  normal' :: Maybe (Either ParseError UIElement)
  normal' = toUIElement <$> normal

  light' :: Maybe (NonEmptyList (Either ParseError UIElement))
  light' = (map toUIElement) <$> light

extractPosition ∷ Either ParseError UIElement → Either String String
extractPosition bold = case bold of
  Right (UIPlainText str) -> Right str
  _ -> Left "No position"

maybeExtractFromMaybe ∷
  ∀ a. (Either ParseError UIElement → Either String a)
  → Maybe (Either ParseError UIElement)
  → Maybe a
maybeExtractFromMaybe extract maybeNode = hush $ (extract <=< note "silent fail") maybeNode

extractCompany ∷ Either ParseError UIElement → Either String String
extractCompany = case _ of
  Right (UIPlainText str) -> Right str
  Right (UIDotSeparated (UIPlainText str) _) -> Right str
  _ -> Left "No company"

extractContractType ∷ Either ParseError UIElement → Either String String
extractContractType = case _ of
  Right (UIDotSeparated _ (UIPlainText str)) -> Right str
  _ -> Left "No company"

extractTimeSpan ∷ Maybe (NonEmptyList (Either ParseError UIElement)) → Either String TimeSpan
extractTimeSpan light = case light of
  Just l -> note "No timespan" $ NEL.findMap getTimeSpan l
  Nothing -> Left "No timespan"
  where
    getTimeSpan (Right (UIDotSeparated (UITimeSpan s) _)) = Just s
    getTimeSpan _ = Nothing

extractDuration ∷ Maybe (NonEmptyList (Either ParseError UIElement)) → Either String Duration
extractDuration light = case light of
  Just l -> note "No duration" $ NEL.findMap getDuration l
  Nothing -> Left "No duration"
  where
    getDuration (Right (UIDotSeparated _ (UIDuration d))) = Just d
    getDuration _ = Nothing

extractDescription ∷ List (Either ParseError UIElement) → Either String String
extractDescription Nil = Left "no description"
extractDescription cs = case L.head cs of
    Just (Right (UIPlainText d)) -> Right d
    _ -> Left "No description"

toUIElement ∷ DetachedNode → Either ParseError UIElement
toUIElement (DetachedElement {content}) = runParser content uiElementP
toUIElement (DetachedComment str) = runParser str uiElementP
toUIElement (DetachedText str) = runParser str uiElementP
