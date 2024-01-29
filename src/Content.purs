module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import LinkedIn (getContext, run)
import LinkedIn.Page.JobOffer (JobOfferPage)
import LinkedIn.Page.Projects (ProjectsPage)
import LinkedIn.Page.Skills (SkillsPage)
import LinkedIn.Page.WorkExperiences (WorkExperiencesPage)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = do
  log "[content] starting up"

  dom <- getBrowserDom

  getContext dom >>= logShow

  run (Proxy :: Proxy WorkExperiencesPage) dom >>= logShow
  run (Proxy :: Proxy SkillsPage) dom >>= logShow
  run (Proxy :: Proxy ProjectsPage) dom >>= logShow
  run (Proxy :: Proxy JobOfferPage) dom >>= logShow
