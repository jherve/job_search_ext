module LinkedIn.ArtDecoCard where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import LinkedIn.ArtDeco (ArtDecoPvsEntity, parseArtDecoPvsEntity)
import LinkedIn.ArtDeco as AD
import LinkedIn.Types (Parser)
import LinkedIn.UIElements.Types (UIElement)
import LinkedIn.Utils (queryOneAndParse)
import Parsing (ParseError)


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

toCenterContent ∷ ArtDecoCardElement → List (Either ParseError UIElement)
toCenterContent = toPvsEntity >>> AD.toCenterContent

toHeaderBold ∷ ArtDecoCardElement → Either ParseError UIElement
toHeaderBold = toPvsEntity >>> AD.toHeaderBold

toHeaderLight ∷ ArtDecoCardElement → Maybe (NonEmptyList (Either ParseError UIElement))
toHeaderLight = toPvsEntity >>> AD.toHeaderLight

toHeaderNormal ∷ ArtDecoCardElement → Maybe (Either ParseError UIElement)
toHeaderNormal = toPvsEntity >>> AD.toHeaderNormal

toPvsEntity ∷ ArtDecoCardElement → ArtDecoPvsEntity
toPvsEntity (ArtDecoCardElement { pvs_entity }) = pvs_entity
