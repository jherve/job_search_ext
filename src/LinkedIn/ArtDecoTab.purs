module LinkedIn.ArtDecoTab where

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


data ArtDecoTabElement a = ArtDecoTabElement {
  pvs_entity :: ArtDecoPvsEntity a
}

derive instance Generic (ArtDecoTabElement a) _
derive instance Eq a => Eq (ArtDecoTabElement a)
instance Show a => Show (ArtDecoTabElement a) where
  show = genericShow
derive instance Functor ArtDecoTabElement

parseArtDecoTab :: Parser (ArtDecoTabElement DetachedNode)
parseArtDecoTab n = do
  pvs <- queryOneAndParse ":scope div.pvs-entity--padded" parseArtDecoPvsEntity n

  pure $ ado
    p <- pvs
  in ArtDecoTabElement {pvs_entity: p}

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
