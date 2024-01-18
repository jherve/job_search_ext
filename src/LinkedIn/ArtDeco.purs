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


data ArtDecoPvsEntity a = ArtDecoPvsEntity {
  side :: Unit,
  center :: ArtDecoCenter a
}

data ArtDecoCenter a = ArtDecoCenter {
  header :: ArtDecoCenterHeader a,
  content :: ArtDecoCenterContent a
}

data ArtDecoCenterHeader a = ArtDecoCenterHeader {
  bold :: a,
  normal :: Maybe a,
  light :: Maybe (NonEmptyList a)
}

data ArtDecoCenterContent a = ArtDecoCenterContent (NonEmptyList (ArtDecoPvsEntitySubComponent a))

data ArtDecoPvsEntitySubComponent a = ArtDecoPvsEntitySubComponent (Maybe a)


derive instance Generic (ArtDecoPvsEntitySubComponent a) _
derive instance Eq a => Eq (ArtDecoPvsEntitySubComponent a)
instance Show a => Show (ArtDecoPvsEntitySubComponent a) where
  show = genericShow
instance Functor ArtDecoPvsEntitySubComponent where
  map f (ArtDecoPvsEntitySubComponent c) = ArtDecoPvsEntitySubComponent (map f c)

derive instance Generic (ArtDecoCenterContent a) _
derive instance Eq a => Eq(ArtDecoCenterContent a)
instance Show a => Show(ArtDecoCenterContent a) where
  show = genericShow
instance Functor ArtDecoCenterContent where
  map f (ArtDecoCenterContent nel) = ArtDecoCenterContent (map (map f) nel)

derive instance Generic (ArtDecoCenterHeader a) _
derive instance Eq a => Eq(ArtDecoCenterHeader a)
instance Show a => Show(ArtDecoCenterHeader a) where
  show = genericShow
instance Functor ArtDecoCenterHeader where
  map f (ArtDecoCenterHeader {bold, normal, light}) =
    ArtDecoCenterHeader ({
      bold: f bold,
      normal: map f normal,
      light: map (map f) light
    })

derive instance Generic (ArtDecoCenter a) _
derive instance Eq a => Eq(ArtDecoCenter a)
instance Show a => Show(ArtDecoCenter a) where
  show = genericShow
instance Functor ArtDecoCenter where
  map f (ArtDecoCenter {content, header}) =
    ArtDecoCenter ({content: map f content, header: map f header})

derive instance Generic (ArtDecoPvsEntity a) _
derive instance Eq a => Eq(ArtDecoPvsEntity a)
instance Show a => Show(ArtDecoPvsEntity a) where
  show = genericShow
instance Functor ArtDecoPvsEntity where
  map f (ArtDecoPvsEntity {side, center}) =
    ArtDecoPvsEntity ({side, center: map f center})


parseArtDecoPvsEntitySubComponent ∷ Parser (ArtDecoPvsEntitySubComponent DetachedNode)
parseArtDecoPvsEntitySubComponent n = do
  content <- queryAndDetachOne "span[aria-hidden=true]" n
  pure $ Right $ ArtDecoPvsEntitySubComponent $ hush content

parseArtDecoCenterContent ∷ Parser (ArtDecoCenterContent DetachedNode)
parseArtDecoCenterContent n = do
  list <- queryManyAndParse ":scope > ul > li" parseArtDecoPvsEntitySubComponent n
  pure $ ado
    l <- list
  in ArtDecoCenterContent l

parseArtDecoCenterHeader :: Parser (ArtDecoCenterHeader DetachedNode)
parseArtDecoCenterHeader n = do
  bold <- queryAndDetachOne ":scope div.t-bold > span[aria-hidden=true]" n
  normal <- queryAndDetachOne ":scope span.t-normal:not(t-black--light) > span[aria-hidden=true]" n
  light <- queryAndDetachMany ":scope span.t-black--light > span[aria-hidden=true]" n

  pure $ ado
    b <- bold
  in ArtDecoCenterHeader {bold: b, normal: hush normal, light: hush light}

parseArtDecoCenter :: Parser (ArtDecoCenter DetachedNode)
parseArtDecoCenter n = do
  header <- queryOneAndParse ":scope > div" parseArtDecoCenterHeader n
  content <- queryOneAndParse ":scope > div.pvs-entity__sub-components" parseArtDecoCenterContent n

  pure $ ado
    h <- header
    c <- content
  in ArtDecoCenter {header: h, content: c}

parseArtDecoPvsEntity :: Parser (ArtDecoPvsEntity DetachedNode)
parseArtDecoPvsEntity n = do
  center <- queryOneAndParse ":scope > div.display-flex" parseArtDecoCenter n

  pure $ ado
    c <- center
  in ArtDecoPvsEntity {side: unit, center: c}

toHeaderBold ∷ forall a. ArtDecoPvsEntity a → a
toHeaderBold (ArtDecoPvsEntity {
  center: ArtDecoCenter { header: ArtDecoCenterHeader { bold }}
}) = bold

toHeaderNormal ∷ forall a. ArtDecoPvsEntity a → Maybe (a)
toHeaderNormal (ArtDecoPvsEntity {
  center: ArtDecoCenter { header: ArtDecoCenterHeader { normal }}
}) = normal

toHeaderLight ∷ forall a. ArtDecoPvsEntity a → Maybe (NonEmptyList a)
toHeaderLight (ArtDecoPvsEntity {
  center: ArtDecoCenter { header: ArtDecoCenterHeader { light }}
}) = light

toCenterContent ∷ forall a. ArtDecoPvsEntity a → List a
toCenterContent (ArtDecoPvsEntity {
  center: ArtDecoCenter { content: ArtDecoCenterContent subComponents }
}) = NEL.catMaybes $ map (\(ArtDecoPvsEntitySubComponent c) -> c) subComponents
