module LinkedIn.Output (module LinkedIn.Output.Types, run, detachNodes, runToUI, toOutput) where

import Prelude

import Control.Monad.Except (ExceptT, except, lift, throwError, withExceptT)
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import LinkedIn.CanBeQueried (class CanBeQueried)
import LinkedIn.CanBeQueried as CBQ
import LinkedIn.DetachedNode (DetachedNode, toDetached)
import LinkedIn.Extractible (class Extractible)
import LinkedIn.Extractible as LE
import LinkedIn.Output.Types (Output, OutputError(..))
import LinkedIn.Page.JobOffer (JobOfferPage)
import LinkedIn.Page.Projects (ProjectsPage)
import LinkedIn.Page.Skills (SkillsPage)
import LinkedIn.Page.WorkExperiences (WorkExperiencesPage)
import LinkedIn.PageUrl (PageUrl(..))
import LinkedIn.UI.Elements.Parser (toUIElement)
import LinkedIn.UI.Elements.Types (UIElement)
import Parsing (parseErrorMessage)
import Type.Proxy (Proxy(..))
import Web.DOM (Document)

run :: forall root t.
  Traversable t
  => CanBeQueried root t
  => Extractible t
  => Proxy t
  -> root
  -> ExceptT OutputError Effect Output
run prox dom = detachNodes prox dom >>= convertToUI >>= extractData

runToUI :: forall root t.
  Traversable t
  => CanBeQueried root t
  => Proxy t
  -> root
  -> ExceptT OutputError Effect (t UIElement)
runToUI prox dom = detachNodes prox dom >>= convertToUI

detachNodes :: forall root t.
  Traversable t
  => CanBeQueried root t
  => Proxy t
  -> root
  -> ExceptT OutputError Effect (t DetachedNode)
detachNodes _ dom = withExceptT (\err -> ErrorOnDetach err) detached
  where
    detached = do
      qRes <- CBQ.query dom
      lift $ traverse toDetached qRes

convertToUI :: forall t.
  Traversable t
  => t DetachedNode
  -> ExceptT OutputError Effect (t UIElement)
convertToUI detached = withExceptT
  (\err -> ErrorOnUIConversion $ parseErrorMessage err) $
  except $ traverse toUIElement detached

extractData :: forall t.
  Traversable t
  => Extractible t
  => t UIElement
  -> ExceptT OutputError Effect Output
extractData asUI = withExceptT (\err -> ErrorOnExtract err) $ except $ LE.extract asUI

toOutput ∷ PageUrl → (Document → ExceptT OutputError Effect Output)
toOutput = case _ of
  UrlProjects _ -> run (Proxy :: Proxy ProjectsPage)
  UrlSkills _ -> run (Proxy :: Proxy SkillsPage)
  UrlWorkExperience _ -> run (Proxy :: Proxy WorkExperiencesPage)
  UrlJobOffer _ -> run (Proxy :: Proxy JobOfferPage)
  u -> const $ throwError $ ErrorUrlNotSupported u
