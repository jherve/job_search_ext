module LinkedIn.ArtDecoTab where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverseDefault)
import LinkedIn.ArtDeco (ArtDecoPvsEntity, queryArtDecoPvsEntity)
import LinkedIn.ArtDeco as AD
import LinkedIn.QueryRunner (QueryRunner, subQueryOne)
import Web.DOM (Node)


data ArtDecoTabElement a = ArtDecoTabElement {
  pvs_entity :: ArtDecoPvsEntity a
}

derive instance Generic (ArtDecoTabElement a) _
derive instance Eq a => Eq (ArtDecoTabElement a)
instance Show a => Show (ArtDecoTabElement a) where
  show = genericShow
derive instance Functor ArtDecoTabElement

instance Foldable ArtDecoTabElement where
  foldMap f (ArtDecoTabElement {pvs_entity}) = foldMap f pvs_entity

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable ArtDecoTabElement where
  sequence (ArtDecoTabElement {pvs_entity}) = ado
    p <- sequence pvs_entity
  in ArtDecoTabElement {pvs_entity: p}

  traverse = \x -> traverseDefault x

queryArtDecoTab :: QueryRunner (ArtDecoTabElement Node)
queryArtDecoTab n = do
  pvs_entity <- subQueryOne queryArtDecoPvsEntity ":scope div.pvs-entity--padded" n
  pure $ ArtDecoTabElement {pvs_entity}

toCenterContent ∷ forall a. ArtDecoTabElement a → List a
toCenterContent = toPvsEntity >>> AD.toCenterContent

toHeaderBold ∷ forall a. ArtDecoTabElement a → a
toHeaderBold = toPvsEntity >>> AD.toHeaderBold

toHeaderLight ∷ forall a. ArtDecoTabElement a → Maybe (NonEmptyList a)
toHeaderLight = toPvsEntity >>> AD.toHeaderLight

toHeaderNormal ∷ forall a. ArtDecoTabElement a → Maybe a
toHeaderNormal = toPvsEntity >>> AD.toHeaderNormal

toPvsEntity ∷ forall a. ArtDecoTabElement a → ArtDecoPvsEntity a
toPvsEntity (ArtDecoTabElement { pvs_entity }) = pvs_entity
