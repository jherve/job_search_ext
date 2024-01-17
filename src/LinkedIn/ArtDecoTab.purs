module LinkedIn.ArtDecoTab where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import LinkedIn.Types (Parser)
import LinkedIn.Utils (queryOneAndParse)
import LinkedIn.ArtDeco (ArtDecoPvsEntity, parseArtDecoPvsEntity)
import LinkedIn.ArtDeco as AD


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

toUI (ArtDecoTabElement { pvs_entity }) = AD.toUI pvs_entity
