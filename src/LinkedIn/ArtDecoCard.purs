module LinkedIn.ArtDecoCard where

import Prelude

import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import LinkedIn (DetachedNode)
import LinkedIn.Types (Parser)
import LinkedIn.Utils (queryAndDetachMany, queryAndDetachOne, queryManyAndParse, queryOneAndParse)

data ArtDecoPvsEntitySubComponent = ArtDecoPvsEntitySubComponent DetachedNode
derive instance Generic ArtDecoPvsEntitySubComponent _
derive instance Eq ArtDecoPvsEntitySubComponent
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
derive instance Eq ArtDecoCenterContent
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
derive instance Eq ArtDecoCenterHeader
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
derive instance Eq ArtDecoCenter
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
derive instance Eq ArtDecoPvsEntity
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
derive instance Eq ArtDecoCardElement
instance Show ArtDecoCardElement where
  show = genericShow

parseArtDecoCard :: Parser ArtDecoCardElement
parseArtDecoCard n = do
  pvs <- queryOneAndParse ":scope div.pvs-entity--padded" parseArtDecoPvsEntity n

  pure $ ado
    p <- pvs
  in ArtDecoCardElement {pvs_entity: p}
