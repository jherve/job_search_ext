module LinkedIn.UI.Components.ArtDecoTab where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens', toListOf, view)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as L
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverseDefault)
import Data.Tuple (Tuple(..))
import LinkedIn.CanBeQueried (class CanBeQueried, query)
import LinkedIn.QueryRunner (QueryRunner', subQueryOne)
import LinkedIn.Queryable (class Queryable)
import LinkedIn.UI.Components.ArtDeco (ArtDecoPvsEntity, _pvs_to_header_bold, _pvs_to_header_light, _pvs_to_header_normal, _pvs_to_subcomponents, queryArtDecoPvsEntity)
import Type.Proxy (Proxy(..))
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

instance Queryable q => CanBeQueried q ArtDecoTabElement where
  query n = do
    pvs_entity <- subQueryOne query ":scope div.pvs-entity--padded" n
    pure $ ArtDecoTabElement {pvs_entity}

toHeaderBold ∷ ∀ a. ArtDecoTabElement a → a
toHeaderBold = view $ _tab_to_pvs_entity <<< _pvs_to_header_bold

toHeaderNormal ∷ ∀ a. ArtDecoTabElement a → Maybe a
toHeaderNormal = view $ _tab_to_pvs_entity <<< _pvs_to_header_normal

toHeaderLight ∷ ∀ a. ArtDecoTabElement a → Maybe (NonEmptyList a)
toHeaderLight = view $ _tab_to_pvs_entity <<< _pvs_to_header_light

toCenterContent ∷ ∀ a. ArtDecoTabElement a → List a
toCenterContent c = L.catMaybes $ toContent c
  where
    toContent = toListOf $ _tab_to_pvs_entity <<< _pvs_to_subcomponents

_tab :: forall a. Lens' (ArtDecoTabElement a) { pvs_entity ∷ ArtDecoPvsEntity a }
_tab = lens' \(ArtDecoTabElement t) -> Tuple t \t' -> ArtDecoTabElement t'

_tab_to_pvs_entity :: forall a. Lens' (ArtDecoTabElement a) (ArtDecoPvsEntity a)
_tab_to_pvs_entity = _tab <<< prop (Proxy :: Proxy "pvs_entity")
