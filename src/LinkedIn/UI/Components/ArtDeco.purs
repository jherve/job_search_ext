module LinkedIn.UI.Components.ArtDeco where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', Traversal', lens', traversed)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverseDefault)
import Data.Tuple (Tuple(..))
import LinkedIn.CanBeQueried (class CanBeQueried, subQueryNEL, subQueryOne)
import LinkedIn.QueryRunner (ignoreNotFound, queryList, queryOne)
import LinkedIn.Queryable (class Queryable)
import Type.Proxy (Proxy(..))


data ArtDecoPvsEntity a = ArtDecoPvsEntity {
  side :: Unit,
  center :: ArtDecoCenter a
}

data ArtDecoCenter a = ArtDecoCenter {
  header :: ArtDecoCenterHeader a,
  content :: ArtDecoCenterContent a
}

data ArtDecoCenterHeader a = ArtDecoCenterHeader {
  bold :: a,
  normal :: Maybe a,
  light :: List a
}

data ArtDecoCenterContent a = ArtDecoCenterContent (NonEmptyList (ArtDecoPvsEntitySubComponent a))

data ArtDecoPvsEntitySubComponent a = ArtDecoPvsEntitySubComponent a


derive instance Generic (ArtDecoPvsEntitySubComponent a) _
derive instance Eq a => Eq (ArtDecoPvsEntitySubComponent a)
instance Show a => Show (ArtDecoPvsEntitySubComponent a) where
  show = genericShow
derive instance Functor ArtDecoPvsEntitySubComponent

instance Foldable ArtDecoPvsEntitySubComponent where
  foldMap f (ArtDecoPvsEntitySubComponent sc) = f sc

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable ArtDecoPvsEntitySubComponent where
  sequence (ArtDecoPvsEntitySubComponent subComponents) = ado
    sc <- subComponents
  in ArtDecoPvsEntitySubComponent sc

  traverse = \x -> traverseDefault x

instance Queryable q => CanBeQueried q ArtDecoPvsEntitySubComponent where
  query n = do
    content <- queryOne "span[aria-hidden=true]" n
    pure $ ArtDecoPvsEntitySubComponent content

derive instance Generic (ArtDecoCenterContent a) _
derive instance Eq a => Eq(ArtDecoCenterContent a)
instance Show a => Show(ArtDecoCenterContent a) where
  show = genericShow
derive instance Functor ArtDecoCenterContent

instance Foldable ArtDecoCenterContent where
  foldMap f (ArtDecoCenterContent c) = foldMap (foldMap f) c

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable ArtDecoCenterContent where
  sequence (ArtDecoCenterContent center) = ado
    c <- sequence (map sequence center)
  in ArtDecoCenterContent c

  traverse = \x -> traverseDefault x

instance Queryable q => CanBeQueried q ArtDecoCenterContent where
  query n = do
    sc <- subQueryNEL ":scope > ul > li" n
    pure $ ArtDecoCenterContent sc

derive instance Generic (ArtDecoCenterHeader a) _
derive instance Eq a => Eq(ArtDecoCenterHeader a)
instance Show a => Show(ArtDecoCenterHeader a) where
  show = genericShow
derive instance Functor ArtDecoCenterHeader

instance Foldable ArtDecoCenterHeader where
  foldMap f (ArtDecoCenterHeader {bold, normal, light}) = f bold <> foldMap f normal <> foldMap f light

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable ArtDecoCenterHeader where
  sequence (ArtDecoCenterHeader {bold, normal, light}) = ado
    b <- bold
    n <- sequence normal
    l <- sequence light
  in ArtDecoCenterHeader {bold: b, normal: n, light: l}

  traverse = \x -> traverseDefault x

instance Queryable q => CanBeQueried q ArtDecoCenterHeader where
  query n = do
    bold <- queryOne ":scope div.t-bold > span[aria-hidden=true]" n
    normal <-
      ignoreNotFound $
      queryOne ":scope span.t-normal:not(t-black--light) > span[aria-hidden=true]" n
    light <-
      queryList ":scope span.t-black--light > span[aria-hidden=true]" n

    pure $ ArtDecoCenterHeader {bold, normal, light}

derive instance Generic (ArtDecoCenter a) _
derive instance Eq a => Eq(ArtDecoCenter a)
instance Show a => Show(ArtDecoCenter a) where
  show = genericShow
derive instance Functor ArtDecoCenter

instance Foldable ArtDecoCenter where
  foldMap f (ArtDecoCenter {header, content}) = foldMap f header <> foldMap f content

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable ArtDecoCenter where
  sequence (ArtDecoCenter {header, content}) = ado
    h <- sequence header
    c <- sequence content
  in ArtDecoCenter {header: h, content: c}

  traverse = \x -> traverseDefault x

instance Queryable q => CanBeQueried q ArtDecoCenter where
  query n = do
    header <- subQueryOne ":scope > div" n
    content <- subQueryOne ":scope > div.pvs-entity__sub-components" n

    pure $ ArtDecoCenter {header, content}

derive instance Generic (ArtDecoPvsEntity a) _
derive instance Eq a => Eq(ArtDecoPvsEntity a)
instance Show a => Show(ArtDecoPvsEntity a) where
  show = genericShow
derive instance Functor ArtDecoPvsEntity

instance Foldable ArtDecoPvsEntity where
  foldMap f (ArtDecoPvsEntity {side: _, center}) = foldMap f center

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable ArtDecoPvsEntity where
  sequence (ArtDecoPvsEntity {side, center}) = ado
    s <- pure side
    c <- sequence center
  in ArtDecoPvsEntity {side: s, center: c}

  traverse = \x -> traverseDefault x

instance Queryable q => CanBeQueried q ArtDecoPvsEntity where
  query n = do
    center <- subQueryOne ":scope > div.display-flex" n
    pure $ ArtDecoPvsEntity {side: unit, center}

_pvs_entity :: forall a. Lens' (ArtDecoPvsEntity a) { center ∷ ArtDecoCenter a , side ∷ Unit }
_pvs_entity = lens'
  \(ArtDecoPvsEntity pvs) -> Tuple pvs \pvs' -> ArtDecoPvsEntity pvs'

_center :: forall a. Lens' (ArtDecoCenter a) { content ∷ ArtDecoCenterContent a , header ∷ ArtDecoCenterHeader a }
_center = lens'
  \(ArtDecoCenter center) -> Tuple center \center' -> ArtDecoCenter center'

_header :: forall a. Lens' (ArtDecoCenterHeader a) { bold ∷ a , light ∷ List a , normal ∷ Maybe a }
_header = lens' \(ArtDecoCenterHeader h) -> Tuple h \h' -> ArtDecoCenterHeader h'

_content :: forall a. Lens' (ArtDecoCenterContent a) (NonEmptyList (ArtDecoPvsEntitySubComponent a))
_content = lens' \(ArtDecoCenterContent cs) -> Tuple cs \cs' -> ArtDecoCenterContent cs'

_sub_component :: forall a. Lens' (ArtDecoPvsEntitySubComponent a) a
_sub_component = lens' \(ArtDecoPvsEntitySubComponent s) -> Tuple s \s' -> ArtDecoPvsEntitySubComponent s'

_pvs_to_header :: forall a. Lens' (ArtDecoPvsEntity a) { bold ∷ a , light ∷ List a , normal ∷ Maybe a }
_pvs_to_header = _pvs_entity
  <<< prop (Proxy :: Proxy "center")
  <<< _center
  <<< prop (Proxy :: Proxy "header")
  <<< _header

_pvs_to_subcomponents ∷ ∀ a. Traversal' (ArtDecoPvsEntity a) a
_pvs_to_subcomponents = _pvs_entity
  <<< prop (Proxy :: Proxy "center")
  <<< _center
  <<< prop (Proxy :: Proxy "content")
  <<< _content
  <<< traversed
  <<< _sub_component

_pvs_to_header_bold :: forall a. Lens' (ArtDecoPvsEntity a) a
_pvs_to_header_bold = _pvs_to_header <<< prop (Proxy :: Proxy "bold")

_pvs_to_header_normal :: forall a. Lens' (ArtDecoPvsEntity a) (Maybe a)
_pvs_to_header_normal = _pvs_to_header <<< prop (Proxy :: Proxy "normal")

_pvs_to_header_light :: forall a. Lens' (ArtDecoPvsEntity a) (List a)
_pvs_to_header_light = _pvs_to_header <<< prop (Proxy :: Proxy "light")
