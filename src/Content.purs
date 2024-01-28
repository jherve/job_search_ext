module ExampleWebExt.Content where

import Prelude

import Browser.DOM (getBrowserDom)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import LinkedIn.DetachedNode (DetachedNode, toDetached)
import LinkedIn.Page.JobOffer as PageJ
import LinkedIn.Page.Projects as PageP
import LinkedIn.Page.Skills as PageS
import LinkedIn.Page.WorkExperiences as PageWE
import LinkedIn.QueryRunner (QueryRunner', runQuery)
import Web.DOM (Document, Node)

main :: Effect Unit
main = do
  log "[content] starting up"

  dom <- getBrowserDom

  runQueryAndExtract PageWE.query PageWE.extract dom
  runQueryAndExtract PageP.query PageP.extract dom
  runQueryAndExtract PageS.query PageS.extract dom
  runQueryAndExtract PageJ.query PageJ.extract dom

  runQueryAndDetach PageJ.query dom >>= logShow

runQueryAndExtract ∷
  ∀ f1 a.
  Show a
  ⇒ QueryRunner' Document (f1 Node)
  → (f1 Node → Effect (Either String a))
  → Document
  → Effect Unit
runQueryAndExtract query extract dom = do
  n <- runQuery $ query dom
  case n of
    Left l' -> logShow l'
    Right q -> do
      extracted <- extract q
      logShow extracted

runQueryAndDetach ∷
  ∀ f1.
  Traversable f1
  ⇒ QueryRunner' Document (f1 Node)
  → Document
  → Effect (Maybe (f1 DetachedNode))
runQueryAndDetach query dom = do
  n <- runQuery $ query dom
  case n of
    Left _ -> pure Nothing
    Right q -> do
      d <- traverse toDetached q
      pure $ Just d
