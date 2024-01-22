module LinkedIn.ArtDeco where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverseDefault)
import LinkedIn.QueryRunner (QueryRunner, ignoreNotFound, queryAll, queryOne, subQueryMany, subQueryOne)
import Web.DOM (Node)


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
derive instance Functor ArtDecoPvsEntitySubComponent

instance Foldable ArtDecoPvsEntitySubComponent where
  foldMap f (ArtDecoPvsEntitySubComponent sc) = foldMap f sc

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable ArtDecoPvsEntitySubComponent where
  sequence (ArtDecoPvsEntitySubComponent subComponents) = ado
    sc <- sequence subComponents
  in ArtDecoPvsEntitySubComponent sc

  traverse = \x -> traverseDefault x

derive instance Generic (ArtDecoCenterContent a) _
derive instance Eq a => Eq(ArtDecoCenterContent a)
instance Show a => Show(ArtDecoCenterContent a) where
  show = genericShow
derive instance Functor ArtDecoCenterContent

instance Foldable ArtDecoCenterContent where
  foldMap f (ArtDecoCenterContent c) = foldMap (foldMap f) c

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable ArtDecoCenterContent where
  sequence (ArtDecoCenterContent center) = ado
    c <- sequence (map sequence center)
  in ArtDecoCenterContent c

  traverse = \x -> traverseDefault x

derive instance Generic (ArtDecoCenterHeader a) _
derive instance Eq a => Eq(ArtDecoCenterHeader a)
instance Show a => Show(ArtDecoCenterHeader a) where
  show = genericShow
derive instance Functor ArtDecoCenterHeader

instance Foldable ArtDecoCenterHeader where
  foldMap f (ArtDecoCenterHeader {bold, normal, light}) = f bold <> foldMap f normal <> foldMap (foldMap f) light

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable ArtDecoCenterHeader where
  sequence (ArtDecoCenterHeader {bold, normal, light}) = ado
    b <- bold
    n <- sequence normal
    l <- sequence (map sequence light)
  in ArtDecoCenterHeader {bold: b, normal: n, light: l}

  traverse = \x -> traverseDefault x

derive instance Generic (ArtDecoCenter a) _
derive instance Eq a => Eq(ArtDecoCenter a)
instance Show a => Show(ArtDecoCenter a) where
  show = genericShow
derive instance Functor ArtDecoCenter 

instance Foldable ArtDecoCenter where
  foldMap f (ArtDecoCenter {header, content}) = foldMap f header <> foldMap f content

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable ArtDecoCenter where
  sequence (ArtDecoCenter {header, content}) = ado
    h <- sequence header
    c <- sequence content
  in ArtDecoCenter {header: h, content: c}

  traverse = \x -> traverseDefault x

derive instance Generic (ArtDecoPvsEntity a) _
derive instance Eq a => Eq(ArtDecoPvsEntity a)
instance Show a => Show(ArtDecoPvsEntity a) where
  show = genericShow
derive instance Functor ArtDecoPvsEntity

instance Foldable ArtDecoPvsEntity where
  foldMap f (ArtDecoPvsEntity {side: _, center}) = foldMap f center

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable ArtDecoPvsEntity where
  sequence (ArtDecoPvsEntity {side, center}) = ado
    s <- pure side
    c <- sequence center
  in ArtDecoPvsEntity {side: s, center: c}

  traverse = \x -> traverseDefault x

queryArtDecoPvsEntitySubComponent ∷ QueryRunner (ArtDecoPvsEntitySubComponent Node)
queryArtDecoPvsEntitySubComponent n = do
  content <- ignoreNotFound $ queryOne "span[aria-hidden=true]" n
  pure $ ArtDecoPvsEntitySubComponent content

queryArtDecoCenterContent ∷ QueryRunner (ArtDecoCenterContent Node)
queryArtDecoCenterContent n = do
  sc <- subQueryMany queryArtDecoPvsEntitySubComponent ":scope > ul > li" n
  pure $ ArtDecoCenterContent sc

queryArtDecoCenterHeader ∷ QueryRunner (ArtDecoCenterHeader Node)
queryArtDecoCenterHeader n = do
  bold <- queryOne ":scope div.t-bold > span[aria-hidden=true]" n
  normal <-
    ignoreNotFound $
    queryOne ":scope span.t-normal:not(t-black--light) > span[aria-hidden=true]" n
  light <-
    ignoreNotFound $
    queryAll ":scope span.t-black--light > span[aria-hidden=true]" n

  pure $ ArtDecoCenterHeader {bold, normal, light}

queryArtDecoCenter ∷ QueryRunner (ArtDecoCenter Node)
queryArtDecoCenter n = do
  header <- subQueryOne queryArtDecoCenterHeader ":scope > div" n
  content <- subQueryOne queryArtDecoCenterContent ":scope > div.pvs-entity__sub-components" n

  pure $ ArtDecoCenter {header, content}

queryArtDecoPvsEntity ∷ QueryRunner (ArtDecoPvsEntity Node)
queryArtDecoPvsEntity n = do
  center <- subQueryOne queryArtDecoCenter ":scope > div.display-flex" n
  pure $ ArtDecoPvsEntity {side: unit, center}

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
