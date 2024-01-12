module LinkedIn where

import Prelude
import Yoga.Tree

import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Document, Node)
import Web.DOM.Document as D
import Web.DOM.Element as E
import Web.DOM.Node (nodeName, nodeType)
import Web.DOM.Node as N
import Web.DOM.NodeList as NL
import Web.DOM.NodeType (NodeType(..))
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)

-- A light abstraction layer above the DOM manipulation API

fromDocument ∷ Document → Node
fromDocument doc = D.toNode doc

queryOne :: String -> Document -> Effect (Maybe Node)
queryOne selector doc = do
  found <- querySelector (QuerySelector selector) $ D.toParentNode doc
  pure case found of
    Nothing -> Nothing
    Just el -> Just $ E.toNode el

queryAll :: String -> Document -> Effect (Maybe (NonEmptyList Node))
queryAll selector doc = do
  found <- querySelectorAll (QuerySelector selector) $ D.toParentNode doc
  liftA1 NEL.fromFoldable $ NL.toArray found

-- First pass of naming ; from here we know what we are looking for

data LinkedInUIElement =
  ArtDecoCardRaw Node
  | ArtDecoTabRaw Node

instance Show LinkedInUIElement where
  show (ArtDecoCardRaw n) = "ArtDecoCardRaw(" <> nodeName n <> ")"
  show (ArtDecoTabRaw n) = "ArtDecoTabRaw(" <> nodeName n <> ")"

getArtDecoCards ∷ Document → Effect (Maybe (NonEmptyList LinkedInUIElement))
getArtDecoCards = queryAll' ArtDecoCardRaw "section.artdeco-card > div ~ div > div > div > ul > li"

getArtDecoTabs ∷ Document → Effect (Maybe (NonEmptyList LinkedInUIElement))
getArtDecoTabs = queryAll' ArtDecoTabRaw "div.artdeco-tabs > div > div > div > div > ul > li"

queryAll' ∷ ∀ b. (Node → b) → String → Document → Effect (Maybe (NonEmptyList b))
queryAll' constructor selector doc = do
  nodes <- queryAll selector doc
  pure case nodes of
    Nothing -> Nothing
    Just cards -> Just $ map constructor cards
