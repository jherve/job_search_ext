module LinkedIn.ArtDecoTab where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import LinkedIn (DetachedNode)
import LinkedIn.ArtDeco (ArtDecoPvsEntity, parseArtDecoPvsEntity)
import LinkedIn.ArtDeco as AD
import LinkedIn.Types (Parser)
import LinkedIn.UIElements.Types (UIElement)
import LinkedIn.Utils (queryOneAndParse)
import Parsing (ParseError)


data ArtDecoTabElement a = ArtDecoTabElement {
  pvs_entity :: ArtDecoPvsEntity a
}

derive instance Generic (ArtDecoTabElement a) _
derive instance Eq a => Eq (ArtDecoTabElement a)
instance Show a => Show (ArtDecoTabElement a) where
  show = genericShow
instance Functor ArtDecoTabElement where
  map f (ArtDecoTabElement {pvs_entity}) =
    ArtDecoTabElement ({pvs_entity: map f pvs_entity})

parseArtDecoTab :: Parser (ArtDecoTabElement DetachedNode)
parseArtDecoTab n = do
  pvs <- queryOneAndParse ":scope div.pvs-entity--padded" parseArtDecoPvsEntity n

  pure $ ado
    p <- pvs
  in ArtDecoTabElement {pvs_entity: p}

toCenterContent ∷ ArtDecoTabElement DetachedNode → List (Either ParseError UIElement)
toCenterContent = toPvsEntity >>> AD.toCenterContent

toHeaderBold ∷ ArtDecoTabElement DetachedNode → Either ParseError UIElement
toHeaderBold = toPvsEntity >>> AD.toHeaderBold

toHeaderLight ∷ ArtDecoTabElement DetachedNode → Maybe (NonEmptyList (Either ParseError UIElement))
toHeaderLight = toPvsEntity >>> AD.toHeaderLight

toHeaderNormal ∷ ArtDecoTabElement DetachedNode → Maybe (Either ParseError UIElement)
toHeaderNormal = toPvsEntity >>> AD.toHeaderNormal

toPvsEntity ∷ forall a. ArtDecoTabElement a → ArtDecoPvsEntity a
toPvsEntity (ArtDecoTabElement { pvs_entity }) = pvs_entity
