module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import LinkedIn (run, runToDetached)
import LinkedIn.Page.JobOffer as PageJ
import LinkedIn.Page.Projects as PageP
import LinkedIn.Page.Skills as PageS
import LinkedIn.Page.WorkExperiences as PageWE

main :: Effect Unit
main = do
  log "[content] starting up"

  dom <- getBrowserDom

  run PageWE.query PageWE.extract dom >>= logShow
  run PageP.query PageP.extract dom >>= logShow
  run PageS.query PageS.extract dom >>= logShow
  run PageJ.query PageJ.extract dom >>= logShow

  runToDetached PageJ.query dom >>= logShow
