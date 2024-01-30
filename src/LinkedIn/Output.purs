module LinkedIn.Output where

import Prelude

import Data.Either (Either(..))
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

toOutput ∷ PageUrl → (Document → Effect (Either String Output))
toOutput = case _ of
  UrlProjects _ -> run (Proxy :: Proxy ProjectsPage)
  UrlSkills _ -> run (Proxy :: Proxy SkillsPage)
  UrlWorkExperience _ -> run (Proxy :: Proxy WorkExperiencesPage)
  UrlJobOffer _ -> run (Proxy :: Proxy JobOfferPage)
  _ -> const $ pure $ Left "Not handled yet"
