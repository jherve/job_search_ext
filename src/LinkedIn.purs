module LinkedIn where

import Prelude

import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Document, Element, Node, ParentNode)
import Web.DOM.Document as D
import Web.DOM.Element (tagName)
import Web.DOM.Element as E
import Web.DOM.Node as N
import Web.DOM.NodeList as NL
import Web.DOM.NodeType (NodeType(..))
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)

-- A light abstraction layer above the DOM manipulation API

data LinkedInNode =
  LinkedInDocument Document
  | LinkedInElement Element

fromDocument ∷ Document → LinkedInNode
fromDocument = LinkedInDocument

instance Show LinkedInNode where
  show (LinkedInDocument _) = "some document"
  show (LinkedInElement el) = "some " <> tagName el <> " element"

toParentNode' ∷ LinkedInNode → ParentNode
toParentNode' (LinkedInDocument doc) = D.toParentNode doc
toParentNode' (LinkedInElement el) = E.toParentNode el

fromNode ∷ Partial ⇒ Node → LinkedInNode
fromNode n = case N.nodeType n of
  ElementNode -> LinkedInElement $ unsafePartial $ fromJust $ E.fromNode n

queryOne :: String -> LinkedInNode -> Effect (Maybe LinkedInNode)
queryOne selector node = do
  found <- querySelector (QuerySelector selector) $ toParentNode' node
  pure case found of
    Nothing -> Nothing
    Just el -> Just $ LinkedInElement el

queryAll :: String -> LinkedInNode -> Effect (Maybe (NonEmptyList LinkedInNode))
queryAll selector node = do
  found <- querySelectorAll (QuerySelector selector) $ toParentNode' node
  nodeList <- liftA1 NEL.fromFoldable $ NL.toArray found

  pure case nodeList of
    Nothing -> Nothing
    Just list -> Just $ map (unsafePartial fromNode) list

-- First pass of naming ; from here we know what we are looking for

data LinkedInRaw =
  ArtDecoCardRaw LinkedInNode
  | ArtDecoTabRaw LinkedInNode

instance Show LinkedInRaw where
  show (ArtDecoCardRaw n) = "ArtDecoCardRaw(" <> show n <> ")"
  show (ArtDecoTabRaw n) = "ArtDecoTabRaw(" <> show n <> ")"

getArtDecoCards ∷ LinkedInNode → Effect (Maybe (NonEmptyList LinkedInRaw))
getArtDecoCards = queryAll' ArtDecoCardRaw "section.artdeco-card > div ~ div > div > div > ul > li"

getArtDecoTabs ∷ LinkedInNode → Effect (Maybe (NonEmptyList LinkedInRaw))
getArtDecoTabs = queryAll' ArtDecoTabRaw "div.artdeco-tabs > div > div > div > div > ul > li"

queryAll' ∷ ∀ b. (LinkedInNode → b) → String → LinkedInNode → Effect (Maybe (NonEmptyList b))
queryAll' constructor selector root = do
  nodes <- queryAll selector root
  pure case nodes of
    Nothing -> Nothing
    Just cards -> Just $ map constructor cards
