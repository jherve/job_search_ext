module LinkedIn.ArtDecoCard where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import LinkedIn (DetachedNode)
import LinkedIn.ArtDeco (ArtDecoPvsEntity, parseArtDecoPvsEntity)
import LinkedIn.ArtDeco as AD
import LinkedIn.Types (Parser)
import LinkedIn.Utils (queryOneAndParse)


data ArtDecoCardElement a = ArtDecoCardElement {
  pvs_entity :: ArtDecoPvsEntity a
}

derive instance Generic (ArtDecoCardElement a) _
derive instance Eq a => Eq (ArtDecoCardElement a)
instance Show a => Show (ArtDecoCardElement a) where
  show = genericShow
instance Functor ArtDecoCardElement where
  map f (ArtDecoCardElement {pvs_entity}) =
    ArtDecoCardElement ({pvs_entity: map f pvs_entity})

parseArtDecoCard :: Parser (ArtDecoCardElement DetachedNode)
parseArtDecoCard n = do
  pvs <- queryOneAndParse ":scope div.pvs-entity--padded" parseArtDecoPvsEntity n

  pure $ ado
    p <- pvs
  in ArtDecoCardElement {pvs_entity: p}

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
