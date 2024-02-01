module LinkedIn.Output (run, runToDetached, toOutput) where

import Prelude

import Control.Monad.Except (ExceptT, except, lift, withExceptT)
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
import LinkedIn.UI.Elements.Parser (toUIElement)
import Type.Proxy (Proxy(..))
import Web.DOM (Document)

run :: forall t.
  Traversable t
  => Extractible t
  => Proxy t
  -> Document
  -> ExceptT String Effect Output
run prox dom = do
  detached <- withExceptT (\_ -> "Error on detach") $ runToDetached prox dom
  asUI <- withExceptT (\_ -> "error on conversion to UI element") $ except $ traverse toUIElement detached
  except $ LE.extract asUI

runToDetached :: forall t.
  Traversable t
  => Extractible t
  => Proxy t
  -> Document
  -> ExceptT QueryError Effect (t DetachedNode)
runToDetached _ dom = do
  qRes <- LE.query @t dom
  lift $ traverse toDetached qRes

toOutput ∷ PageUrl → (Document → ExceptT String Effect Output)
toOutput = case _ of
  UrlProjects _ -> run (Proxy :: Proxy ProjectsPage)
  UrlSkills _ -> run (Proxy :: Proxy SkillsPage)
  UrlWorkExperience _ -> run (Proxy :: Proxy WorkExperiencesPage)
  UrlJobOffer _ -> run (Proxy :: Proxy JobOfferPage)
  _ -> const $ except $ Left "Not handled yet"
