module LinkedIn.Profile.Project where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.List as L
import Data.List.Types (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import LinkedIn.UI.Basic.Types (TimeSpan)
import LinkedIn.UI.Components.ArtDecoCard (ArtDecoCardElement, toCenterContent, toHeaderBold, toHeaderNormal)
import LinkedIn.UI.Elements.Types (UIElement(..))
import LinkedIn.UI.Strings.Types (UIString(..))

type ProjectObject = {
  name :: String,
  timeSpan :: Maybe TimeSpan,
  description :: Maybe String
}
newtype Project = Project ProjectObject

derive instance Generic Project _
derive instance Eq Project
instance Show Project where show = genericShow
instance EncodeJson Project where encodeJson a = genericEncodeJson a

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
  UIElement (UIStringDotSeparated (NonEmptyList(UIStringTimeSpan s :| _))) -> Just s
  _ -> Nothing

extractDescription ∷ UIElement → Maybe String
extractDescription = case _ of
  UIElement (UIStringPlain d) -> Just d
  _ -> Nothing
