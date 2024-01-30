module LinkedIn.Output (run, runToDetached, toOutput) where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import LinkedIn.DetachedNode (DetachedNode, toDetached)
import LinkedIn.Extractible (class Extractible)
import LinkedIn.Extractible as LE
import LinkedIn.Output.Types (Output)
import LinkedIn.Page.JobOffer (JobOfferPage)
import LinkedIn.Page.Projects (ProjectsPage)
import LinkedIn.Page.Skills (SkillsPage)
import LinkedIn.Page.WorkExperiences (WorkExperiencesPage)
import LinkedIn.PageUrl (PageUrl(..))
import LinkedIn.QueryRunner (QueryError, QueryRunner', runQuery)
import LinkedIn.UI.Elements.Parser (fromDetachedToUI)
import LinkedIn.UI.Elements.Types (UIElement)
import Type.Proxy (Proxy(..))
import Web.DOM (Document, Node)

run :: forall t.
  Traversable t
  => Extractible t
  => Proxy t
  -> Document
  -> Effect (Either String Output)
run prox dom = do
  detached <- runToDetached prox dom
  pure $ extract LE.extract $ toUI detached

runToDetached :: forall t.
  Traversable t
  => Extractible t
  => Proxy t
  -> Document
  -> Effect (Either QueryError (t DetachedNode))
runToDetached _ dom = do
  qRes <- doQuery LE.query dom
  detach qRes

doQuery ∷ ∀ b. QueryRunner' Document b → Document → Effect (Either QueryError b)
doQuery query dom = runQuery $ query dom

detach ∷ ∀ a t. Traversable t ⇒ Either a (t Node) → Effect (Either a (t DetachedNode))
detach = case _ of
  Left l -> pure $ Left l
  Right q -> do
    d <- traverse toDetached q
    pure $ Right d

toUI ∷ ∀ a t. Traversable t ⇒ Either a (t DetachedNode) → Either String (t UIElement)
toUI = case _ of
  Left _ -> Left "could not convert to UI"
  Right d -> fromDetachedToUI d

extract ∷ ∀ t a. (t UIElement → Either String a) → Either String (t UIElement) → Either String a
extract parsePage = case _ of
  Left l -> Left l
  Right ui -> parsePage ui

toOutput ∷ PageUrl → (Document → Effect (Either String Output))
toOutput = case _ of
  UrlProjects _ -> run (Proxy :: Proxy ProjectsPage)
  UrlSkills _ -> run (Proxy :: Proxy SkillsPage)
  UrlWorkExperience _ -> run (Proxy :: Proxy WorkExperiencesPage)
  UrlJobOffer _ -> run (Proxy :: Proxy JobOfferPage)
  _ -> const $ pure $ Left "Not handled yet"
