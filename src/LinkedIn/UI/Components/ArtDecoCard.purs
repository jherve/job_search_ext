module LinkedIn.UI.Components.ArtDecoCard where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens', toListOf, view)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverseDefault)
import Data.Tuple (Tuple(..))
import LinkedIn.CanBeQueried (class CanBeQueried, subQueryOne)
import LinkedIn.Queryable (class Queryable)
import LinkedIn.UI.Components.ArtDeco (ArtDecoPvsEntity, _pvs_to_header_bold, _pvs_to_header_light, _pvs_to_header_normal, _pvs_to_subcomponents)
import Type.Proxy (Proxy(..))

type ArtDecoCardElementObject a = { pvs_entity :: ArtDecoPvsEntity a }
newtype ArtDecoCardElement a = ArtDecoCardElement (ArtDecoCardElementObject a)

derive instance Generic (ArtDecoCardElement a) _
derive instance Eq a => Eq (ArtDecoCardElement a)
instance Show a => Show (ArtDecoCardElement a) where show = genericShow
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

instance Queryable q => CanBeQueried q ArtDecoCardElement where
  query n = do
    pvs_entity <- subQueryOne ":scope div.pvs-entity--padded" n
    pure $ ArtDecoCardElement {pvs_entity}

toHeaderBold ∷ ∀ a. ArtDecoCardElement a → a
toHeaderBold = view $ _card_to_pvs_entity <<< _pvs_to_header_bold

toHeaderNormal ∷ ∀ a. ArtDecoCardElement a → Maybe a
toHeaderNormal = view $ _card_to_pvs_entity <<< _pvs_to_header_normal

toHeaderLight ∷ ∀ a. ArtDecoCardElement a → List a
toHeaderLight = view $ _card_to_pvs_entity <<< _pvs_to_header_light

toCenterContent ∷ ∀ a. ArtDecoCardElement a → List a
toCenterContent c = toContent c
  where
    toContent = toListOf $ _card_to_pvs_entity <<< _pvs_to_subcomponents

_card :: forall a. Lens' (ArtDecoCardElement a) { pvs_entity ∷ ArtDecoPvsEntity a }
_card = lens' \(ArtDecoCardElement c) -> Tuple c \c' -> ArtDecoCardElement c'

_card_to_pvs_entity :: forall a. Lens' (ArtDecoCardElement a) (ArtDecoPvsEntity a)
_card_to_pvs_entity = _card <<< prop (Proxy :: Proxy "pvs_entity")
