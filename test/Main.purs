module Test.Main where

import Prelude

import Effect (Effect)
import Test.ArtDecoCard (testArtDecoCard)
import Test.JobsUnifiedTopCard (testJobsUnifiedTopCard)
import Test.UIStringParser (testUIStringParser)

main :: Effect Unit
main = do
  testArtDecoCard
  testJobsUnifiedTopCard
  testUIStringParser
