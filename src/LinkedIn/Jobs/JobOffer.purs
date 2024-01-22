module LinkedIn.Jobs.JobOffer where

import Prelude

import Data.Either (Either, note)
import Data.Foldable (findMap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import LinkedIn.DetachedNode (DetachedNode)
import LinkedIn.JobsUnifiedTopCard (JobsUnifiedTopCardElement, toHeader, toPrimaryDescriptionLink, toPrimaryDescriptionText)
import LinkedIn.Profile.Utils (toUIElement)
import LinkedIn.UIElements.Types (UIElement(..))

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
fromUI card = ado
    title <- note "No title found" $ findMap extractTitle header
    companyName <- note "No company found" $ findMap extractCompany link
    companyLink <- note "No company link found" $ findMap extractCompanyLink link
  in
    JobOffer { title, companyName, companyLink, location: findMap extractLocation primaryDescText }
  where
    asUI = toUIElement <$> card
    header = toHeader asUI
    link = toPrimaryDescriptionLink asUI
    primaryDescText = toPrimaryDescriptionText asUI

extractTitle :: UIElement -> Maybe String
extractTitle = case _ of
  UIPlainText str -> Just str
  _ -> Nothing

extractCompany :: UIElement -> Maybe String
extractCompany = case _ of
  UILink _ (UIPlainText str) -> Just str
  _ -> Nothing

extractCompanyLink :: UIElement -> Maybe String
extractCompanyLink = case _ of
  UILink link _ -> Just link
  _ -> Nothing

extractLocation :: UIElement -> Maybe String
extractLocation = case _ of
  UIDotSeparated _ (UIPlainText str) -> Just str
  _ -> Nothing
