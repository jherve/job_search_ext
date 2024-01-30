module LinkedIn.Output where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import LinkedIn (run)
import LinkedIn.Jobs.JobOffer as JO
import LinkedIn.Page.JobOffer (JobOfferPage)
import LinkedIn.Page.Projects (ProjectsPage)
import LinkedIn.Page.Skills (SkillsPage)
import LinkedIn.Page.WorkExperiences (WorkExperiencesPage)
import LinkedIn.PageUrl (PageUrl(..))
import LinkedIn.Profile.Project (Project)
import LinkedIn.Profile.Skill (Skill)
import LinkedIn.Profile.WorkExperience (WorkExperience)
import Type.Proxy (Proxy(..))
import Web.DOM (Document)

-- Surely the best solution would be that "extract" functions directly return a value of type Output
-- instead of quasi-repeating the type definitions here.
-- We'll probably need a unique Output type to be able to send out a return value anyway.
data Output =
  Projects (NonEmptyList Project)
  | Skills (NonEmptyList Skill)
  | WorkExperiences (NonEmptyList WorkExperience)
  | JobOffer JO.JobOffer

derive instance Generic Output _
instance Show Output where
  show = genericShow

toPage ∷ Partial ⇒ PageUrl → Document → Effect (Either String Output)
toPage ctx dom = case ctx of
  UrlProjects _ -> map (map Projects) $ run (Proxy :: Proxy ProjectsPage) dom
  UrlSkills _ ->  map (map Skills) $ run (Proxy :: Proxy SkillsPage) dom
  UrlWorkExperience _ ->  map (map WorkExperiences) $ run (Proxy :: Proxy WorkExperiencesPage) dom
  UrlJobOffer _ -> map (map JobOffer) $ run (Proxy :: Proxy JobOfferPage) dom
