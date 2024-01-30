module LinkedIn (run, runToDetached, getContext) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import LinkedIn.DetachedNode (DetachedNode, toDetached)
import LinkedIn.Extractible (class Extractible)
import LinkedIn.Extractible as LE
import LinkedIn.Output.Types (Output)
import LinkedIn.PageUrl (PageUrl, pageUrlP)
import LinkedIn.QueryRunner (QueryError, QueryRunner', runQuery)
import LinkedIn.UI.Elements.Parser (fromDetachedToUI)
import LinkedIn.UI.Elements.Types (UIElement)
import Parsing (runParser)
import Type.Proxy (Proxy)
import Web.DOM (Document, Node)
import Web.DOM.Document (url)
import Web.URL as U

getContext ∷ Document → Effect (Either String PageUrl)
getContext dom = do
  u <- url dom
  pure $ case U.fromAbsolute u of
    Nothing -> Left "No URL found"
    Just u' -> case runParser (U.pathname u') pageUrlP of
      Left _ -> Left "Unexpected URL"
      Right page -> Right page

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
