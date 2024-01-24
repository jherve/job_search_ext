module LinkedIn where

import Prelude

import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import LinkedIn.DetachedNode (DetachedNode, asTree', cutBranches, filterEmpty)
import Web.DOM (Document, Node)
import Web.DOM.Document as D
import Web.DOM.Node (nodeName)
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Yoga.Tree (Tree)

-- A light abstraction layer above the DOM manipulation API

fromDocument ∷ Document → Node
fromDocument doc = D.toNode doc

queryAllNodes :: String -> Document -> Effect (Maybe (NonEmptyList Node))
queryAllNodes selector doc = do
  found <- querySelectorAll (QuerySelector selector) $ D.toParentNode doc
  liftA1 NEL.fromFoldable $ NL.toArray found

-- First pass of naming ; from here we know what we are looking for

data LinkedInUIElementType = LinkedInUIArtDecoCard | LinkedInUIArtDecoTab | LinkedInUIJobsUnifiedTopCard

instance Show LinkedInUIElementType where
  show LinkedInUIArtDecoCard = "ArtDecoCard"
  show LinkedInUIArtDecoTab = "ArtDecoTab"
  show LinkedInUIJobsUnifiedTopCard = "JobsUnifiedTopCard"

data LinkedInUIElement = LinkedInUIElement LinkedInUIElementType Node

instance Show LinkedInUIElement where
  show (LinkedInUIElement typ n) = "LinkedInUIElement(" <> show typ <> ", " <> nodeName n <> ")"

getArtDecoCards ∷ Document → Effect (Maybe (NonEmptyList LinkedInUIElement))
getArtDecoCards = queryAll' LinkedInUIArtDecoCard "section.artdeco-card > div ~ div > div > div > ul > li"

getArtDecoTabs ∷ Document → Effect (Maybe (NonEmptyList LinkedInUIElement))
getArtDecoTabs = queryAll' LinkedInUIArtDecoTab "div.artdeco-tabs > div > div > div > div > ul > li"

getJobsUnifiedTopCard ∷ Document → Effect (Maybe (NonEmptyList LinkedInUIElement))
getJobsUnifiedTopCard = queryAll' LinkedInUIJobsUnifiedTopCard "div.jobs-unified-top-card"

queryAll' ∷ LinkedInUIElementType → String → Document → Effect (Maybe (NonEmptyList LinkedInUIElement))
queryAll' constructor selector doc = do
  nodes <- queryAllNodes selector doc
  pure case nodes of
    Nothing -> Nothing
    Just cards -> Just $ map (LinkedInUIElement constructor) cards

asTree :: LinkedInUIElement -> Effect (Tree DetachedNode)
asTree (LinkedInUIElement _ n) = asTree' n

asPrunedTrees :: Maybe (NonEmptyList LinkedInUIElement) → Effect (Maybe (NonEmptyList (Tree DetachedNode)))
asPrunedTrees =
  case _ of
    Nothing -> pure Nothing
    Just els -> do
      trees <- traverse asPrunedTree els
      pure $ Just $ trees

asPrunedTree :: LinkedInUIElement → Effect (Tree DetachedNode)
asPrunedTree el =  do
  tree <- asTree el
  pure $ cutBranches filterEmpty tree
