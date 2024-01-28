module LinkedIn.Jobs.JobOffer where

import Prelude

import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Lens (findOf)
import Data.Maybe (Maybe(..), isJust)
import Data.Show.Generic (genericShow)
import LinkedIn.JobsUnifiedTopCard (JobsUnifiedTopCardElement, TopCardInsight(..), TopCardInsightContent(..), _top_to_action_buttons, _top_to_insights, toHeader, toPrimaryDescriptionLink, toPrimaryDescriptionText)
import LinkedIn.UIElements.Types (UIElement(..), UIString(..))

data JobOffer = JobOffer {
  title :: String,
  companyName :: String,
  companyLink :: String,
  location :: Maybe String,
  remote :: Maybe String,
  companyDomain :: Maybe String,
  companySize :: Maybe String,
  hasSimplifiedApplicationProcess :: Boolean
}

derive instance Generic JobOffer _
instance Show JobOffer where
  show = genericShow

fromUI ∷ JobsUnifiedTopCardElement UIElement → Either String JobOffer
fromUI card = ado
    title <- note "No title found" $ extractTitle header
    companyName <- note "No company found" $ extractCompany link
    companyLink <- note "No company link found" $ extractCompanyLink link
  in
    JobOffer {
      title,
      companyName,
      companyLink,
      location: extractLocation primaryDescText,
      remote: extractJobRemote =<< jobInsight,
      companyDomain: extractCompanyDomain =<< companyInsight,
      companySize: extractCompanySize =<< companyInsight,
      hasSimplifiedApplicationProcess: isJust $ extractSimplifiedApplicationProcess =<< applyButton
    }
  where
    header = toHeader card
    link = toPrimaryDescriptionLink card
    primaryDescText = toPrimaryDescriptionText card
    jobInsight = getInsight "job" card
    companyInsight = getInsight "company" card
    applyButton = findOf _top_to_action_buttons isApply card
      where
        isApply = case _ of
          UIButton {mainClass: Just main} -> main == "jobs-apply-button"
          _ -> false

getInsight ∷ String → JobsUnifiedTopCardElement UIElement → Maybe (TopCardInsight UIElement)
getInsight i card = findOf _top_to_insights (isIcon i) card
  where
    isIcon icon = case _ of
      TopCardInsight {icon: UIIcon i} -> i == icon
      _ -> false

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

extractCompanyDomain ∷ TopCardInsight UIElement → Maybe String
extractCompanyDomain = case _ of
  TopCardInsight {content: TopCardInsightContentSingle (UIElement (UIStringDotSeparated _ (UIStringPlain str)))} -> Just str
  _ -> Nothing

extractCompanySize ∷ TopCardInsight UIElement → Maybe String
extractCompanySize = case _ of
  TopCardInsight {content: TopCardInsightContentSingle (UIElement (UIStringDotSeparated (UIStringPlain str) _))} -> Just str
  _ -> Nothing

extractLocation :: UIElement -> Maybe String
extractLocation = case _ of
  UIElement (UIStringDotSeparated _ (UIStringPlain str)) -> Just str
  _ -> Nothing

extractJobRemote :: TopCardInsight UIElement -> Maybe String
extractJobRemote = case _ of
  TopCardInsight {content: TopCardInsightContentSecondary {primary: (UIElement (UIStringPlain str))}} -> Just str
  _ -> Nothing

extractSimplifiedApplicationProcess ∷ UIElement → Maybe Boolean
extractSimplifiedApplicationProcess = case _ of
  UIButton {role: Nothing} -> Just true
  _ -> Nothing
