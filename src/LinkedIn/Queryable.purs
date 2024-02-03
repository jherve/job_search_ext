module LinkedIn.Queryable where

import Prelude

import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Document, Node, ParentNode)
import Web.DOM.Document as D
import Web.DOM.Element as E
import Web.DOM.Node as N
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)

-- A light abstraction layer above the DOM query API

class Queryable a where
  toNode :: a -> Node
  toParentNode :: a -> ParentNode

instance Queryable Node where
  toParentNode :: Node -> ParentNode
  toParentNode n =
    unsafePartial $ fromJust $ intoParentNode' n where
        intoParentNode' :: Node -> Maybe ParentNode
        intoParentNode' node = do
          he <- E.fromNode node
          pure $ E.toParentNode he

  toNode = identity

instance Queryable Document where
  toNode = D.toNode
  toParentNode = D.toParentNode

queryOneNode :: forall a. Queryable a => String -> a -> Effect (Maybe Node)
queryOneNode selector n = do
  found <- querySelector (QuerySelector selector) $ toParentNode n
  pure case found of
    Nothing -> Nothing
    Just el -> Just $ E.toNode el

queryAllNodes :: forall a. Queryable a => String -> a -> Effect (Maybe (NonEmptyList Node))
queryAllNodes selector n = do
  found <- querySelectorAll (QuerySelector selector) $ toParentNode n
  liftA1 NEL.fromFoldable $ NL.toArray found

getChildrenArray :: forall a. Queryable a => a -> Effect (Array Node)
getChildrenArray n = do
    children <- N.childNodes $ toNode n
    NL.toArray children
