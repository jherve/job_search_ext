module LinkedIn.Utils where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (sequence)
import Effect (Effect)
import LinkedIn (DetachedNode, toDetached)
import LinkedIn.Types (ParseError(..), Parser)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Node, ParentNode)
import Web.DOM.Element as E
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)

toParentNode' :: Node -> ParentNode
toParentNode' n =
  unsafePartial $ fromJust $ intoParentNode' n where
      intoParentNode' :: Node -> Maybe ParentNode
      intoParentNode' node = do
        he <- E.fromNode node
        pure $ E.toParentNode he

queryOne :: String -> Node -> Effect (Maybe Node)
queryOne selector n = do
  found <- querySelector (QuerySelector selector) $ toParentNode' n
  pure case found of
    Nothing -> Nothing
    Just el -> Just $ E.toNode el

queryAll :: String -> Node -> Effect (Maybe (NonEmptyList Node))
queryAll selector n = do
  found <- querySelectorAll (QuerySelector selector) $ toParentNode' n
  liftA1 NEL.fromFoldable $ NL.toArray found

parseDetachedNode :: Parser DetachedNode
parseDetachedNode node = do
  node' <- toDetached node
  pure $ Right node'

queryAndDetachOne ∷ String -> Parser DetachedNode
queryAndDetachOne selector n = queryOneAndParse selector parseDetachedNode n

queryAndDetachMany ∷ String -> Parser (NonEmptyList DetachedNode)
queryAndDetachMany selector n = queryManyAndParse selector parseDetachedNode n

queryOneAndParse ∷ ∀ a. String → Parser a → Parser a
queryOneAndParse selector parser n = do
  selected <- queryOne selector n

  case selected of
    Nothing -> pure $ Left $ NodeNotFoundError selector
    Just node -> parser node

queryManyAndParse ∷ ∀ a. String → Parser a → Parser (NonEmptyList a)
queryManyAndParse selector parser n = do
  selected <- queryAll selector n
  case selected of
    Nothing -> pure $ Left $ NodeListNotFoundError selector
    Just nodes -> do
      nodes' <- sequence $ map parser nodes :: Effect (NonEmptyList((Either ParseError a)))
      pure $ sequence nodes'
