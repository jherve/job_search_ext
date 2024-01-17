module Test.Main where

import Prelude

import Effect (Effect)
import Test.ArtDecoCard (testArtDecoCard)
import Test.JobsUnifiedTopCard (testJobsUnifiedTopCard)

main :: Effect Unit
main = do
  testArtDecoCard
  testJobsUnifiedTopCard
