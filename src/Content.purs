module ExampleWebExt.Content where

import Data.List.NonEmpty
import LinkedIn
import Prelude

import Browser.DOM (getBrowserDom)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Yoga.Tree (showTree)

main :: Effect Unit
main = do
  dom <- getBrowserDom
  artDecoCards <- getArtDecoCards dom
  artDecoTabs <- getArtDecoTabs dom
  jobsUnifiedTopCard <- getJobsUnifiedTopCard dom

  log "[content] starting up"

  sCards <- maybeShow "no card found" artDecoCards
  log sCards

  sTabs <- maybeShow "no tabs found" artDecoTabs
  log sTabs

  sTopCards <- maybeShow "no top card found" jobsUnifiedTopCard
  log sTopCards

maybeShow ∷ String → Maybe (NonEmptyList LinkedInUIElement) → Effect String
maybeShow errorMsg els =
  case els of
    Nothing -> pure errorMsg
    Just cards -> do
      tree <- asTree $ head cards
      pure $ showTree $ cutBranches filterEmpty tree
