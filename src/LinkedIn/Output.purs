module LinkedIn.Output (run, runToDetached, toOutput) where

import Prelude

import Control.Monad.Except (ExceptT, except, lift, runExceptT)
import Data.Either (Either(..), either)
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
runToDetached proxy dom = runExceptT $ runToDetached'' proxy dom

runToDetached'' :: forall t.
  Traversable t
  => Extractible t
  => Proxy t
  -> Document
  -> ExceptT QueryError Effect (t DetachedNode)
runToDetached'' _ dom = do
  qRes <- LE.query @t dom
  detach'' qRes

doQuery ∷ ∀ b. QueryRunner' Document b → Document → Effect (Either QueryError b)
doQuery query dom = runQuery $ query dom

detach ∷ ∀ a t. Traversable t ⇒ Either a (t Node) → Effect (Either a (t DetachedNode))
detach n = runExceptT $ detach' n

detach' ∷ ∀ a t. Traversable t ⇒ Either a (t Node) → ExceptT a Effect (t DetachedNode)
detach' n = do
  n' <- except n
  detach'' n'

detach'' ∷ ∀ a t. Traversable t ⇒ (t Node) → ExceptT a Effect (t DetachedNode)
detach'' n = lift $ traverse toDetached n

toUI ∷ ∀ a t. Traversable t ⇒ Either a (t DetachedNode) → Either String (t UIElement)
toUI = either (const $ Left "could not convert to UI") fromDetachedToUI

extract ∷ ∀ t a. (t UIElement → Either String a) → Either String (t UIElement) → Either String a
extract parsePage = either (\err -> Left err) parsePage

toOutput ∷ PageUrl → (Document → Effect (Either String Output))
toOutput = case _ of
  UrlProjects _ -> run (Proxy :: Proxy ProjectsPage)
  UrlSkills _ -> run (Proxy :: Proxy SkillsPage)
  UrlWorkExperience _ -> run (Proxy :: Proxy WorkExperiencesPage)
  UrlJobOffer _ -> run (Proxy :: Proxy JobOfferPage)
  _ -> const $ pure $ Left "Not handled yet"
