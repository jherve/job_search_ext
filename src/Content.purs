module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import LinkedIn.Page.Projects as PageP
import LinkedIn.Page.WorkExperiences as PageWE
import LinkedIn.Page.Skills as PageS
import LinkedIn.QueryRunner (runQuery)

main :: Effect Unit
main = do
  log "[content] starting up"

  dom <- getBrowserDom

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
