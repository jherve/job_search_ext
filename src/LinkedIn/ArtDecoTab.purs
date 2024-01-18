module LinkedIn.ArtDecoTab where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import LinkedIn.Types (Parser)
import LinkedIn.Utils (queryOneAndParse)
import LinkedIn.UIElements.Types (UIElement)
import LinkedIn.ArtDeco (ArtDecoPvsEntity, parseArtDecoPvsEntity)
import LinkedIn.ArtDeco as AD
import Parsing (ParseError)


data ArtDecoTabElement = ArtDecoTabElement {
  pvs_entity :: ArtDecoPvsEntity
}

derive instance Generic ArtDecoTabElement _
derive instance Eq ArtDecoTabElement
instance Show ArtDecoTabElement where
  show = genericShow

parseArtDecoTab :: Parser ArtDecoTabElement
parseArtDecoTab n = do
  pvs <- queryOneAndParse ":scope div.pvs-entity--padded" parseArtDecoPvsEntity n

  pure $ ado
    p <- pvs
  in ArtDecoTabElement {pvs_entity: p}

toCenterContent ∷ ArtDecoTabElement → List (Either ParseError UIElement)
toCenterContent = toPvsEntity >>> AD.toCenterContent

toHeaderBold ∷ ArtDecoTabElement → Either ParseError UIElement
toHeaderBold = toPvsEntity >>> AD.toHeaderBold

toHeaderLight ∷ ArtDecoTabElement → Maybe (NonEmptyList (Either ParseError UIElement))
toHeaderLight = toPvsEntity >>> AD.toHeaderLight

toHeaderNormal ∷ ArtDecoTabElement → Maybe (Either ParseError UIElement)
toHeaderNormal = toPvsEntity >>> AD.toHeaderNormal

toPvsEntity ∷ ArtDecoTabElement → ArtDecoPvsEntity
toPvsEntity (ArtDecoTabElement { pvs_entity }) = pvs_entity
