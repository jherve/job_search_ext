module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import LinkedIn (getContext, run)
import LinkedIn.Output (toOutput)
import LinkedIn.Page.JobOffer (JobOfferPage)
import LinkedIn.Page.Projects (ProjectsPage)
import LinkedIn.Page.Skills (SkillsPage)
import LinkedIn.Page.WorkExperiences (WorkExperiencesPage)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = do
  log "[content] starting up"

  dom <- getBrowserDom

  run (Proxy :: Proxy WorkExperiencesPage) dom >>= logShow
  run (Proxy :: Proxy SkillsPage) dom >>= logShow
  run (Proxy :: Proxy ProjectsPage) dom >>= logShow
  run (Proxy :: Proxy JobOfferPage) dom >>= logShow

  ctx <- getContext dom

  logShow ctx

  case ctx of
    Left l -> logShow l
    Right r -> unsafePartial $ toOutput r dom >>= logShow
