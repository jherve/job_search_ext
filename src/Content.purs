module ExampleWebExt.Content where

import Data.List.NonEmpty
import LinkedIn
import Prelude

import Browser.DOM (getBrowserDom)
import Data.List ((:))
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import LinkedIn.ArtDecoCard (parseArtDecoCard)
import Yoga.Tree (showTree)
import Yoga.Tree.Zipper as Z

main :: Effect Unit
main = do
  dom <- getBrowserDom
  artDecoCards <- getArtDecoCards dom
  artDecoTabs <- getArtDecoTabs dom
  jobsUnifiedTopCard <- getJobsUnifiedTopCard dom

  log "[content] starting up"

  maybeShowTree artDecoCards >>= log

  maybeShowPruned "no card found" artDecoCards >>= log
  maybeShowPruned "no tabs found" artDecoTabs >>= log
  maybeShowPruned "no top card found" jobsUnifiedTopCard >>= log

  case artDecoCards of 
    Nothing -> log "nothing"
    Just l -> do
      parsed <- (\(LinkedInUIElement _ n) -> parseArtDecoCard n) $ NEL.head l
      logShow parsed


maybeShowTree ∷ Maybe (NonEmptyList LinkedInUIElement) → Effect String
maybeShowTree Nothing = pure "nope"
maybeShowTree (Just nel) = do
  tree <- asTree $ head nel
  pure $ showTree tree

maybeShowPruned ∷ String → Maybe (NonEmptyList LinkedInUIElement) → Effect String
maybeShowPruned errorMsg els = do
  trees <- asPrunedTrees els
  case trees of
    Nothing -> pure errorMsg
    Just ts -> do
      pure $ showTree $ head ts
      --pure $ showMaybeTree $ zipperTest $ head ts

showMaybeTree Nothing = "No tree"
showMaybeTree (Just t) = showTree t
