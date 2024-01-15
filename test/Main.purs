module Test.Main where

import Prelude

import Effect (Effect)

import Test.ArtDecoCard (testArtDecoCard)

main :: Effect Unit
main = do
  testArtDecoCard
