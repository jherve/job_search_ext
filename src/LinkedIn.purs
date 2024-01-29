module LinkedIn (run, runToDetached) where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import LinkedIn.DetachedNode (DetachedNode, toDetached)
import LinkedIn.QueryRunner (QueryError, QueryRunner', runQuery)
import LinkedIn.UI.Elements.Parser (fromDetachedToUI)
import LinkedIn.UI.Elements.Types (UIElement)
import Web.DOM (Document, Node)

run :: forall t a.
  Traversable t
  ⇒ QueryRunner' Document (t Node)
  -> (t UIElement → Either String a)
  -> Document
  -> Effect (Either String a)
run query parse dom = do
  detached <- runToDetached query dom
  pure $ extract parse $ toUI detached

runToDetached :: forall t.
  Traversable t
  ⇒ QueryRunner' Document (t Node)
  -> Document
  -> Effect (Either QueryError (t DetachedNode))
runToDetached query dom = do
  qRes <- doQuery query dom
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
