module LinkedIn.ArtDecoCard where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Debug (trace)
import Effect (Effect)
import Effect.Exception (try)
import LinkedIn (DetachedNode, toDetached)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Node, ParentNode)
import Web.DOM.Element as E
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)

data ArtDecoCenterHeader = ArtDecoCenterHeader {
  bold :: DetachedNode,
  normal :: Unit,
  light :: Array Unit
}

derive instance Generic ArtDecoCenterHeader _
instance Show ArtDecoCenterHeader where
  show = genericShow

parseArtDecoCenterHeader :: Node -> Effect (Either String ArtDecoCenterHeader)
parseArtDecoCenterHeader n = do
  bold <- queryOne ":scope div.t-bold > span[aria-hidden=true]" n
  case bold of
    Nothing -> pure $ Left "Could not parse ArtDecoCenterHeader"
    Just bold -> do
      bold <- toDetached bold
      pure $ Right (ArtDecoCenterHeader {bold: bold, normal: unit, light: [unit]})

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
