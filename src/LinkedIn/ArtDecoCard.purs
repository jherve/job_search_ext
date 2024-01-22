module LinkedIn.ArtDecoCard where

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


data ArtDecoCardElement a = ArtDecoCardElement {
  pvs_entity :: ArtDecoPvsEntity a
}

derive instance Generic (ArtDecoCardElement a) _
derive instance Eq a => Eq (ArtDecoCardElement a)
instance Show a => Show (ArtDecoCardElement a) where
  show = genericShow
derive instance Functor ArtDecoCardElement

instance Foldable ArtDecoCardElement where
  foldMap f (ArtDecoCardElement {pvs_entity}) = foldMap f pvs_entity

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable ArtDecoCardElement where
  sequence (ArtDecoCardElement {pvs_entity}) = ado
    p <- sequence pvs_entity
  in ArtDecoCardElement {pvs_entity: p}

  traverse = \x -> traverseDefault x

queryArtDecoCard :: QueryRunner (ArtDecoCardElement Node)
queryArtDecoCard n = do
  pvs_entity <- subQueryOne queryArtDecoPvsEntity ":scope div.pvs-entity--padded" n
  pure $ ArtDecoCardElement {pvs_entity}

toCenterContent ∷ forall a. ArtDecoCardElement a → List a
toCenterContent = toPvsEntity >>> AD.toCenterContent

toHeaderBold ∷ forall a. ArtDecoCardElement a → a
toHeaderBold = toPvsEntity >>> AD.toHeaderBold

toHeaderLight ∷ forall a.  ArtDecoCardElement a → Maybe (NonEmptyList a)
toHeaderLight = toPvsEntity >>> AD.toHeaderLight

toHeaderNormal ∷ forall a. ArtDecoCardElement a → Maybe a
toHeaderNormal = toPvsEntity >>> AD.toHeaderNormal

toPvsEntity ∷ forall a. ArtDecoCardElement a → ArtDecoPvsEntity a
toPvsEntity (ArtDecoCardElement { pvs_entity }) = pvs_entity
