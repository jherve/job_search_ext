module LinkedIn.ArtDecoCard where

import Prelude

import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Effect (Effect)
import LinkedIn (DetachedNode, toDetached)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Node, ParentNode)
import Web.DOM.Element as E
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)

data ParseError =
  NodeNotFoundError String
  | NodeListNotFoundError String

derive instance Generic ParseError _
instance Show ParseError where
  show = genericShow

type Parser a = Node → Effect (Either ParseError a)

data ArtDecoPvsEntitySubComponent = ArtDecoPvsEntitySubComponent DetachedNode
derive instance Generic ArtDecoPvsEntitySubComponent _
instance Show ArtDecoPvsEntitySubComponent where
  show = genericShow

parseArtDecoPvsEntitySubComponent ∷ Parser ArtDecoPvsEntitySubComponent
parseArtDecoPvsEntitySubComponent n = do
  content <- queryAndDetachOne "span[aria-hidden=true]" n
  pure $ ado
    c <- content
  in ArtDecoPvsEntitySubComponent c

data ArtDecoCenterContent = ArtDecoCenterContent (NonEmptyList ArtDecoPvsEntitySubComponent)
derive instance Generic ArtDecoCenterContent _
instance Show ArtDecoCenterContent where
  show = genericShow

parseArtDecoCenterContent ∷ Parser ArtDecoCenterContent
parseArtDecoCenterContent n = do
  list <- queryManyAndParse ":scope > ul > li" parseArtDecoPvsEntitySubComponent n
  pure $ ado
    l <- list
  in ArtDecoCenterContent l

data ArtDecoCenterHeader = ArtDecoCenterHeader {
  bold :: DetachedNode,
  normal :: Maybe DetachedNode,
  light :: Maybe (NonEmptyList DetachedNode)
}

derive instance Generic ArtDecoCenterHeader _
instance Show ArtDecoCenterHeader where
  show = genericShow

parseArtDecoCenterHeader :: Parser ArtDecoCenterHeader
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

parseArtDecoCenter :: Parser ArtDecoCenter
parseArtDecoCenter n = do
  header <- queryOneAndParse ":scope > div" parseArtDecoCenterHeader n
  content <- queryOneAndParse ":scope > div.pvs-entity__sub-components" parseArtDecoCenterContent n

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

parseArtDecoPvsEntity :: Parser ArtDecoPvsEntity
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

parseArtDecoCard :: Parser ArtDecoCardElement
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
