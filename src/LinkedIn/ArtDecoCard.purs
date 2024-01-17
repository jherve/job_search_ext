module LinkedIn.ArtDecoCard where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import LinkedIn.Types (Parser)
import LinkedIn.Utils (queryOneAndParse)
import LinkedIn.ArtDeco (ArtDecoPvsEntity, parseArtDecoPvsEntity)


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
