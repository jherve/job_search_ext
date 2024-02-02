module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.ArtDecoCard (artDecoCardsSpec)
import Test.JobsUnifiedTopCard (jobsUnifiedTopCardSpec)
import Test.PageUrl (pageUrlSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.UIStringParser (uiStringParserSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  pageUrlSpec
  uiStringParserSpec
  artDecoCardsSpec
  jobsUnifiedTopCardSpec
