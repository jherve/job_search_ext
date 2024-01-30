module LinkedIn.Output where

import Data.Either (Either)
import Effect (Effect)
import LinkedIn (run)
import LinkedIn.Output.Types (Output)
import LinkedIn.Page.JobOffer (JobOfferPage)
import LinkedIn.Page.Projects (ProjectsPage)
import LinkedIn.Page.Skills (SkillsPage)
import LinkedIn.Page.WorkExperiences (WorkExperiencesPage)
import LinkedIn.PageUrl (PageUrl(..))
import Type.Proxy (Proxy(..))
import Web.DOM (Document)

toOutput ∷ Partial ⇒ PageUrl → Document → Effect (Either String Output)
toOutput ctx dom = case ctx of
  UrlProjects _ -> run (Proxy :: Proxy ProjectsPage) dom
  UrlSkills _ -> run (Proxy :: Proxy SkillsPage) dom
  UrlWorkExperience _ -> run (Proxy :: Proxy WorkExperiencesPage) dom
  UrlJobOffer _ -> run (Proxy :: Proxy JobOfferPage) dom
