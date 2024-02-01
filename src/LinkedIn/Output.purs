module LinkedIn.Output (run, runToDetached, toOutput) where

import Prelude

import Control.Monad.Except (ExceptT, except, lift, runExceptT, withExceptT)
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
import LinkedIn.QueryRunner (QueryError)
import LinkedIn.UI.Elements.Parser (fromDetachedToUI)
import Type.Proxy (Proxy(..))
import Web.DOM (Document)

run :: forall t.
  Traversable t
  => Extractible t
  => Proxy t
  -> Document
  -> Effect (Either String Output)
run prox dom = runExceptT $ run' prox dom

run' :: forall t.
  Traversable t
  => Extractible t
  => Proxy t
  -> Document
  -> ExceptT String Effect Output
run' prox dom = do
  detached <- withExceptT (\_ -> "Error on detach") $ runToDetached' prox dom
  asUI <- except $ fromDetachedToUI detached
  except $ LE.extract asUI

runToDetached :: forall t.
  Traversable t
  => Extractible t
  => Proxy t
  -> Document
  -> Effect (Either QueryError (t DetachedNode))
runToDetached proxy dom = runExceptT $ runToDetached' proxy dom

runToDetached' :: forall t.
  Traversable t
  => Extractible t
  => Proxy t
  -> Document
  -> ExceptT QueryError Effect (t DetachedNode)
runToDetached' _ dom = do
  qRes <- LE.query @t dom
  lift $ traverse toDetached qRes

toOutput ∷ PageUrl → (Document → Effect (Either String Output))
toOutput = case _ of
  UrlProjects _ -> run (Proxy :: Proxy ProjectsPage)
  UrlSkills _ -> run (Proxy :: Proxy SkillsPage)
  UrlWorkExperience _ -> run (Proxy :: Proxy WorkExperiencesPage)
  UrlJobOffer _ -> run (Proxy :: Proxy JobOfferPage)
  _ -> const $ pure $ Left "Not handled yet"
