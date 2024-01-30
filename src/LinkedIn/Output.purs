module LinkedIn.Output where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import LinkedIn (run)
import LinkedIn.Output.Types (Output(..))
import LinkedIn.Page.JobOffer (JobOfferPage)
import LinkedIn.Page.Projects (ProjectsPage)
import LinkedIn.Page.Skills (SkillsPage)
import LinkedIn.Page.WorkExperiences (WorkExperiencesPage)
import LinkedIn.PageUrl (PageUrl(..))
import Type.Proxy (Proxy(..))
import Web.DOM (Document)

toPage ∷ Partial ⇒ PageUrl → Document → Effect (Either String Output)
toPage ctx dom = case ctx of
  UrlProjects _ -> map (map OutProjects) $ run (Proxy :: Proxy ProjectsPage) dom
  UrlSkills _ ->  map (map OutSkills) $ run (Proxy :: Proxy SkillsPage) dom
  UrlWorkExperience _ ->  map (map OutWorkExperiences) $ run (Proxy :: Proxy WorkExperiencesPage) dom
  UrlJobOffer _ -> map (map OutJobOffer) $ run (Proxy :: Proxy JobOfferPage) dom
