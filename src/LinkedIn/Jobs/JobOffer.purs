module LinkedIn.Jobs.JobOffer where

import Prelude

import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import LinkedIn.DetachedNode (DetachedNode)
import LinkedIn.JobsUnifiedTopCard (JobsUnifiedTopCardElement, toHeader, toPrimaryDescriptionLink, toPrimaryDescriptionText)
import LinkedIn.Profile.Utils (toUIElement)
import LinkedIn.UIElements.Types (UIElement(..), UIString(..))

data JobOffer = JobOffer {
  title :: String,
  companyName :: String,
  companyLink :: String,
  location :: Maybe String
}

derive instance Generic JobOffer _
instance Show JobOffer where
  show = genericShow


fromUI ∷ JobsUnifiedTopCardElement DetachedNode → Either String JobOffer
fromUI card = fromUI' =<< case traverse toUIElement card of
  Left _ -> Left "error on conversion to UI element"
  Right ui -> Right ui

fromUI' ∷ JobsUnifiedTopCardElement UIElement → Either String JobOffer
fromUI' card = ado
    title <- note "No title found" $ extractTitle header
    companyName <- note "No company found" $ extractCompany link
    companyLink <- note "No company link found" $ extractCompanyLink link
  in
    JobOffer { title, companyName, companyLink, location: extractLocation primaryDescText }
  where
    header = toHeader card
    link = toPrimaryDescriptionLink card
    primaryDescText = toPrimaryDescriptionText card

extractTitle :: UIElement -> Maybe String
extractTitle = case _ of
  UIElement (UIStringPlain str) -> Just str
  _ -> Nothing

extractCompany :: UIElement -> Maybe String
extractCompany = case _ of
  UILink _ (UIStringPlain str) -> Just str
  _ -> Nothing

extractCompanyLink :: UIElement -> Maybe String
extractCompanyLink = case _ of
  UILink link _ -> Just link
  _ -> Nothing

extractLocation :: UIElement -> Maybe String
extractLocation = case _ of
  UIElement (UIStringDotSeparated _ (UIStringPlain str)) -> Just str
  _ -> Nothing
