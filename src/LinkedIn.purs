module LinkedIn where

import Prelude
import Yoga.Tree

import Control.Comonad.Cofree (head, tail)
import Data.Array as A
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (sequence)
import Data.String as S
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Document, Element, Node)
import Web.DOM.Document as D
import Web.DOM.Element as E
import Web.DOM.Node (nodeName, nodeType, textContent)
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

data LinkedInUIElementType = LinkedInUIArtDecoCard | LinkedInUIArtDecoTab

instance Show LinkedInUIElementType where
  show LinkedInUIArtDecoCard = "ArtDecoCard"
  show LinkedInUIArtDecoTab = "ArtDecoTab"

data LinkedInUIElement = LinkedInUIElement LinkedInUIElementType Node

instance Show LinkedInUIElement where
  show (LinkedInUIElement typ n) = "LinkedInUIElement(" <> show typ <> ", " <> nodeName n <> ")"

getArtDecoCards ∷ Document → Effect (Maybe (NonEmptyList LinkedInUIElement))
getArtDecoCards = queryAll' LinkedInUIArtDecoCard "section.artdeco-card > div ~ div > div > div > ul > li"

getArtDecoTabs ∷ Document → Effect (Maybe (NonEmptyList LinkedInUIElement))
getArtDecoTabs = queryAll' LinkedInUIArtDecoTab "div.artdeco-tabs > div > div > div > div > ul > li"

queryAll' ∷ LinkedInUIElementType → String → Document → Effect (Maybe (NonEmptyList LinkedInUIElement))
queryAll' constructor selector doc = do
  nodes <- queryAll selector doc
  pure case nodes of
    Nothing -> Nothing
    Just cards -> Just $ map (LinkedInUIElement constructor) cards

data DetachedNode =
  DetachedElement {tag :: String, content :: String}
  | DetachedComment String
  | DetachedText String

instance Show DetachedNode where
  show (DetachedElement n) = "DetachedElement(" <> n.tag <> ")"
  show (DetachedComment c) = "DetachedComment(" <> c <> ")"
  show (DetachedText t) = "DetachedText(" <> t <> ")"

asTree :: LinkedInUIElement -> Effect (Tree DetachedNode)
asTree (LinkedInUIElement _ n) = asTree' n

asTree' :: Node -> Effect (Tree DetachedNode)
asTree' n = do
  children <- N.childNodes n
  childArray <- NL.toArray children :: Effect (Array Node)
  detached <- toDetached n

  case childArray of
    [] -> pure $ leaf detached
    arr -> do
      a <- sequence (map asTree' arr) :: Effect (Array (Tree DetachedNode))
      pure $ mkTree detached a

toDetached :: Node -> Effect DetachedNode
toDetached node = unsafePartial $ toDetached' (nodeType node) node where
  toDetached' :: Partial => NodeType -> Node -> Effect DetachedNode
  toDetached' ElementNode n = do
    txt <- textContent n
    let
      el = unsafePartial $ fromJust $ E.fromNode n
    pure $ DetachedElement {tag: E.tagName el, content: S.trim txt}
  toDetached' CommentNode n = do
    txt <- textContent n
    pure $ DetachedComment $ S.trim txt
  toDetached' TextNode n = do
    txt <- textContent n
    pure $ DetachedText $ S.trim txt
