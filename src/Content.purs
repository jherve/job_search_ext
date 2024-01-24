module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import LinkedIn (LinkedInUIElement(..), asPrunedTrees, asTree, getArtDecoCards, getArtDecoTabs, getJobsUnifiedTopCard)
import LinkedIn.ArtDecoCard (queryArtDecoCard)
import LinkedIn.ArtDecoTab (queryArtDecoTab)
import LinkedIn.DetachedNode (toDetached)
import LinkedIn.JobsUnifiedTopCard (queryJobsUnifiedTopCardElement)
import LinkedIn.Page.Projects as PageP
import LinkedIn.Page.WorkExperiences as PageWE
import LinkedIn.Page.Skills as PageS
import LinkedIn.Profile.Project as PP
import LinkedIn.Profile.Skill as PS
import LinkedIn.Profile.WorkExperience as PWE
import LinkedIn.QueryRunner (runQuery)
import Yoga.Tree (Tree, showTree)

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
      queried <- (\(LinkedInUIElement _ n) -> runQuery $ queryArtDecoCard n) $ NEL.head l
      case queried of
        Left l' -> logShow l'
        Right p -> do
          detached <- traverse toDetached p
          logShow detached
          logShow $ PWE.fromUI detached
          logShow $ PP.fromUI detached

  case artDecoTabs of
    Nothing -> log "nothing"
    Just l -> do
      queried <- (\(LinkedInUIElement _ n) -> runQuery $ queryArtDecoTab n) $ NEL.head l
      case queried of
        Left l' -> logShow l'
        Right p -> do
          detached <- traverse toDetached p
          logShow detached
          logShow $ PS.fromUI detached

  case jobsUnifiedTopCard of
    Nothing -> log "nothing"
    Just l -> do
      queried <- (\(LinkedInUIElement _ n) -> runQuery $ queryJobsUnifiedTopCardElement n) $ NEL.head l
      case queried of
        Left l' -> logShow l'
        Right p -> do
          detached <- traverse toDetached p
          log "parsed OK"
          logShow detached

  wepNode <- runQuery $ PageWE.query dom
  case wepNode of
    Left l' -> logShow l'
    Right q -> do
      wep <- PageWE.extract q
      logShow wep

  projectsNode <- runQuery $ PageP.query dom
  case projectsNode of
    Left l' -> logShow l'
    Right q -> do
      projects <- PageP.extract q
      logShow projects

  skillsNode <- runQuery $ PageS.query dom
  case skillsNode of
    Left l' -> logShow l'
    Right q -> do
      skills <- PageS.extract q
      logShow skills

maybeShowTree ∷ Maybe (NonEmptyList LinkedInUIElement) → Effect String
maybeShowTree Nothing = pure "nope"
maybeShowTree (Just nel) = do
  tree <- asTree $ NEL.head nel
  pure $ showTree tree

maybeShowPruned ∷ String → Maybe (NonEmptyList LinkedInUIElement) → Effect String
maybeShowPruned errorMsg els = do
  trees <- asPrunedTrees els
  case trees of
    Nothing -> pure errorMsg
    Just ts -> do
      pure $ showTree $ NEL.head ts
      --pure $ showMaybeTree $ zipperTest $ head ts

showMaybeTree ∷ ∀ (a ∷ Type). Show a ⇒ Maybe (Tree a) → String
showMaybeTree Nothing = "No tree"
showMaybeTree (Just t) = showTree t
