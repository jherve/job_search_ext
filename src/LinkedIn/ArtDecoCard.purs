module LinkedIn.ArtDecoCard where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT)
import Data.Either (Either(..))
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

queryAndDetachOne ∷ String -> Node → Effect (Maybe DetachedNode)
queryAndDetachOne selector n = do
  node <- queryOne selector n
  case node of
    Nothing -> pure $ Nothing
    Just node -> do
      node <- toDetached node
      pure $ Just node

queryAndDetachMany ∷ String -> Node → Effect (Maybe (NonEmptyList DetachedNode))
queryAndDetachMany selector n = do
  nodes <- queryAll selector n
  case nodes of
    Nothing -> pure $ Nothing
    Just nodes -> do
      nodes <- traverse toDetached nodes
      pure $ Just nodes

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

  pure $ case bold of
    Nothing -> Left "Could not parse ArtDecoCenterHeader"
    Just bold -> Right (ArtDecoCenterHeader {bold: bold, normal: normal, light: light})

data ArtDecoCenter = ArtDecoCenter {
  header :: ArtDecoCenterHeader,
  content :: Unit
}

derive instance Generic ArtDecoCenter _
instance Show ArtDecoCenter where
  show = genericShow

parseArtDecoCenter :: Node -> Effect (Either String ArtDecoCenter)
parseArtDecoCenter n = do
  header <- queryOne ":scope > div" n
  case header of
    Nothing -> pure $ Left "Could not parse ArtDecoCenter"
    Just header -> do
      header <- parseArtDecoCenterHeader header :: Effect (Either String ArtDecoCenterHeader)
      pure $ case header of
        Left l -> Left l
        Right header -> Right (ArtDecoCenter {header: header, content: unit})

data ArtDecoPvsEntity = ArtDecoPvsEntity {
  side :: Unit,
  center :: ArtDecoCenter
}

derive instance Generic ArtDecoPvsEntity _
instance Show ArtDecoPvsEntity where
  show = genericShow

parseArtDecoPvsEntity :: Node -> Effect (Either String ArtDecoPvsEntity)
parseArtDecoPvsEntity n = do
  center <- queryOne ":scope > div.display-flex" n
  case center of
    Nothing -> pure $ Left "Could not parse ArtDecoPvsEntity"
    Just center -> do
      center <- parseArtDecoCenter center :: Effect (Either String ArtDecoCenter)
      pure $ case center of
        Left l -> Left l
        Right center -> Right $ ArtDecoPvsEntity {side: unit, center: center}

data ArtDecoCardElement = ArtDecoCardElement {
  pvs_entity :: ArtDecoPvsEntity
}

derive instance Generic ArtDecoCardElement _
instance Show ArtDecoCardElement where
  show = genericShow

parseArtDecoCard :: Node -> Effect (Either String ArtDecoCardElement)
parseArtDecoCard n = do
  pvs <- queryOne ":scope div.pvs-entity--padded" n
  case pvs of
    Nothing -> pure $  Left "Could not parse entity"
    Just pvs -> do
      entity <- parseArtDecoPvsEntity pvs :: Effect (Either String ArtDecoPvsEntity)
      pure $ case entity of
        Left l -> Left l
        Right entity -> Right $ ArtDecoCardElement {pvs_entity: entity}

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
