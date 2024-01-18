module LinkedIn.ArtDeco where

import Prelude

import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import LinkedIn (DetachedNode)
import LinkedIn.Profile.Utils (toUIElement)
import LinkedIn.Types (Parser)
import LinkedIn.UIElements.Types (UIElement)
import LinkedIn.Utils (queryAndDetachMany, queryAndDetachOne, queryManyAndParse, queryOneAndParse)
import Parsing (ParseError)


data ArtDecoPvsEntity = ArtDecoPvsEntity {
  side :: Unit,
  center :: ArtDecoCenter
}

data ArtDecoCenter = ArtDecoCenter {
  header :: ArtDecoCenterHeader,
  content :: ArtDecoCenterContent
}

data ArtDecoCenterHeader = ArtDecoCenterHeader {
  bold :: DetachedNode,
  normal :: Maybe DetachedNode,
  light :: Maybe (NonEmptyList DetachedNode)
}

data ArtDecoCenterContent = ArtDecoCenterContent (NonEmptyList ArtDecoPvsEntitySubComponent)

data ArtDecoPvsEntitySubComponent = ArtDecoPvsEntitySubComponent (Maybe DetachedNode)


derive instance Generic ArtDecoPvsEntitySubComponent _
derive instance Eq ArtDecoPvsEntitySubComponent
instance Show ArtDecoPvsEntitySubComponent where
  show = genericShow

derive instance Generic ArtDecoCenterContent _
derive instance Eq ArtDecoCenterContent
instance Show ArtDecoCenterContent where
  show = genericShow

derive instance Generic ArtDecoCenterHeader _
derive instance Eq ArtDecoCenterHeader
instance Show ArtDecoCenterHeader where
  show = genericShow

derive instance Generic ArtDecoCenter _
derive instance Eq ArtDecoCenter
instance Show ArtDecoCenter where
  show = genericShow

derive instance Generic ArtDecoPvsEntity _
derive instance Eq ArtDecoPvsEntity
instance Show ArtDecoPvsEntity where
  show = genericShow


parseArtDecoPvsEntitySubComponent ∷ Parser ArtDecoPvsEntitySubComponent
parseArtDecoPvsEntitySubComponent n = do
  content <- queryAndDetachOne "span[aria-hidden=true]" n
  pure $ Right $ ArtDecoPvsEntitySubComponent $ hush content

parseArtDecoCenterContent ∷ Parser ArtDecoCenterContent
parseArtDecoCenterContent n = do
  list <- queryManyAndParse ":scope > ul > li" parseArtDecoPvsEntitySubComponent n
  pure $ ado
    l <- list
  in ArtDecoCenterContent l

parseArtDecoCenterHeader :: Parser ArtDecoCenterHeader
parseArtDecoCenterHeader n = do
  bold <- queryAndDetachOne ":scope div.t-bold > span[aria-hidden=true]" n
  normal <- queryAndDetachOne ":scope span.t-normal:not(t-black--light) > span[aria-hidden=true]" n
  light <- queryAndDetachMany ":scope span.t-black--light > span[aria-hidden=true]" n

  pure $ ado
    b <- bold
  in ArtDecoCenterHeader {bold: b, normal: hush normal, light: hush light}

parseArtDecoCenter :: Parser ArtDecoCenter
parseArtDecoCenter n = do
  header <- queryOneAndParse ":scope > div" parseArtDecoCenterHeader n
  content <- queryOneAndParse ":scope > div.pvs-entity__sub-components" parseArtDecoCenterContent n

  pure $ ado
    h <- header
    c <- content
  in ArtDecoCenter {header: h, content: c}

parseArtDecoPvsEntity :: Parser ArtDecoPvsEntity
parseArtDecoPvsEntity n = do
  center <- queryOneAndParse ":scope > div.display-flex" parseArtDecoCenter n

  pure $ ado
    c <- center
  in ArtDecoPvsEntity {side: unit, center: c}

toHeaderBold ∷ ArtDecoPvsEntity → Either ParseError UIElement
toHeaderBold (ArtDecoPvsEntity {
    center: ArtDecoCenter { header: ArtDecoCenterHeader { bold }
  }
}) = toUIElement bold

toHeaderNormal ∷ ArtDecoPvsEntity → Maybe (Either ParseError UIElement)
toHeaderNormal (ArtDecoPvsEntity {
  center: ArtDecoCenter { header: ArtDecoCenterHeader { normal }}
}) = toUIElement <$> normal

toHeaderLight ∷ ArtDecoPvsEntity → Maybe (NonEmptyList (Either ParseError UIElement))
toHeaderLight (ArtDecoPvsEntity {
  center: ArtDecoCenter { header: ArtDecoCenterHeader { light } }
}) = (map toUIElement) <$> light

toCenterContent ∷ ArtDecoPvsEntity → List (Either ParseError UIElement)
toCenterContent (ArtDecoPvsEntity {
  center: ArtDecoCenter { content: ArtDecoCenterContent subComponents }
}) = map toUIElement subC
  where subC = NEL.catMaybes $ map (\(ArtDecoPvsEntitySubComponent c) -> c) subComponents :: List (DetachedNode)