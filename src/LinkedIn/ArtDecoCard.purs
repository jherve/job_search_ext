module LinkedIn.ArtDecoCard where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT)
import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Debug (trace)
import Effect (Effect)
import Effect.Exception (try)
import LinkedIn (DetachedNode, toDetached)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Node, ParentNode)
import Web.DOM.Element as E
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)

data ArtDecoPvsEntitySubComponents = ArtDecoPvsEntitySubComponents (Maybe (NonEmptyList DetachedNode))
derive instance Generic ArtDecoPvsEntitySubComponents _
instance Show ArtDecoPvsEntitySubComponents where
  show = genericShow

data ArtDecoCenterContent = ArtDecoCenterContent (Maybe (NonEmptyList DetachedNode))
derive instance Generic ArtDecoCenterContent _
instance Show ArtDecoCenterContent where
  show = genericShow

parseArtDecoCenterContent ∷ Node → Effect (Either String ArtDecoCenterContent)
parseArtDecoCenterContent n = do
  list <- queryAndDetachMany ":scope > ul > li" n
  pure $ Right (ArtDecoCenterContent (hush list))

data ArtDecoCenterHeader = ArtDecoCenterHeader {
  bold :: DetachedNode,
  normal :: Maybe DetachedNode,
  light :: Maybe (NonEmptyList DetachedNode)
}

derive instance Generic ArtDecoCenterHeader _
instance Show ArtDecoCenterHeader where
  show = genericShow

parseArtDecoCenterHeader :: Node -> Effect (Either String ArtDecoCenterHeader)
parseArtDecoCenterHeader n = do
  bold <- queryAndDetachOne ":scope div.t-bold > span[aria-hidden=true]" n
  normal <- queryAndDetachOne ":scope span.t-normal:not(t-black--light) > span[aria-hidden=true]" n
  light <- queryAndDetachMany ":scope span.t-black--light > span[aria-hidden=true]" n

  pure $ ado
    b <- bold
  in ArtDecoCenterHeader {bold: b, normal: hush normal, light: hush light}

data ArtDecoCenter = ArtDecoCenter {
  header :: ArtDecoCenterHeader,
  content :: ArtDecoCenterContent
}

derive instance Generic ArtDecoCenter _
instance Show ArtDecoCenter where
  show = genericShow

parseArtDecoCenter :: Node -> Effect (Either String ArtDecoCenter)
parseArtDecoCenter n = do
  header <- queryOneAndParse ":scope > div" parseArtDecoCenterHeader n
  content <- queryOneAndParse ":scope > div ~ div" parseArtDecoCenterContent n

  pure $ ado
    h <- header
    c <- content
  in ArtDecoCenter {header: h, content: c}

data ArtDecoPvsEntity = ArtDecoPvsEntity {
  side :: Unit,
  center :: ArtDecoCenter
}

derive instance Generic ArtDecoPvsEntity _
instance Show ArtDecoPvsEntity where
  show = genericShow

parseArtDecoPvsEntity :: Node -> Effect (Either String ArtDecoPvsEntity)
parseArtDecoPvsEntity n = do
  center <- queryOneAndParse ":scope > div.display-flex" parseArtDecoCenter n

  pure $ ado
    c <- center
  in ArtDecoPvsEntity {side: unit, center: c}

data ArtDecoCardElement = ArtDecoCardElement {
  pvs_entity :: ArtDecoPvsEntity
}

derive instance Generic ArtDecoCardElement _
instance Show ArtDecoCardElement where
  show = genericShow

parseArtDecoCard :: Node -> Effect (Either String ArtDecoCardElement)
parseArtDecoCard n = do
  pvs <- queryOneAndParse ":scope div.pvs-entity--padded" parseArtDecoPvsEntity n

  pure $ ado
    p <- pvs
  in ArtDecoCardElement {pvs_entity: p}

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

queryAndDetachOne ∷ String -> Node → Effect (Either String DetachedNode)
queryAndDetachOne selector n = do
  node <- queryOne selector n
  case node of
    Nothing -> pure $ Left $ "Could not find node with selector " <> selector
    Just node -> do
      node <- toDetached node
      pure $ Right node

queryAndDetachMany ∷ String -> Node → Effect (Either String (NonEmptyList DetachedNode))
queryAndDetachMany selector n = do
  nodes <- queryAll selector n
  case nodes of
    Nothing -> pure $ Left $ "Did not find any node with selector " <> selector
    Just nodes -> do
      nodes <- traverse toDetached nodes
      pure $ Right nodes

queryOneAndParse ∷ ∀ a. String → (Node → Effect (Either String a)) → Node → Effect (Either String a)
queryOneAndParse selector parser n = do
  node <- queryOne selector n

  case node of
    Nothing -> pure $ Left $ "Could not find node with selector " <> selector
    Just node -> parser node
