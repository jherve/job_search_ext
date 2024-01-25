module Test.Main where

import Prelude

import Effect (Effect)
import Test.ArtDecoCard as ArtDecoCard
import Test.JobsUnifiedTopCard as JobsUnifiedTopCard
import Test.UIStringParser as UIStringParser

main :: Effect Unit
main = do
  ArtDecoCard.main
  JobsUnifiedTopCard.main
  UIStringParser.main
