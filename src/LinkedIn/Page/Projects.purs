module LinkedIn.Page.Projects where

import Prelude

import Data.Either (Either)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)
import Effect (Effect)
import LinkedIn.ArtDecoCard (ArtDecoCardElement, queryArtDecoCard)
import LinkedIn.DetachedNode (toDetached)
import LinkedIn.Profile.Project (Project)
import LinkedIn.Profile.Project as PP
import LinkedIn.Profile.Utils (fromDetachedToUI)
import LinkedIn.QueryRunner (QueryRunner', subQueryMany)
import Web.DOM (Document, Node)

data ProjectsPage a = ProjectsPage (NonEmptyList (ArtDecoCardElement a))

derive instance Generic (ProjectsPage a) _
derive instance Eq a => Eq (ProjectsPage a)
instance Show a => Show (ProjectsPage a) where
  show = genericShow
derive instance Functor ProjectsPage

instance Foldable ProjectsPage where
  foldMap f (ProjectsPage cards) = foldMap (foldMap f) cards

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable ProjectsPage where
  sequence (ProjectsPage cards) = ado
    ts <- sequence (map sequence cards)
  in ProjectsPage ts

  traverse = \x -> traverseDefault x

query :: QueryRunner' Document (ProjectsPage Node)
query n = do
  cards <- subQueryMany queryArtDecoCard "section.artdeco-card > div ~ div > div > div > ul > li" n
  pure $ ProjectsPage cards

extract ∷ ProjectsPage Node → Effect (Either String (NonEmptyList Project))
extract p = do
  detached <- traverse toDetached p
  let
    ProjectsPage cards = detached
  pure $ traverse (PP.fromUI <=< fromDetachedToUI) cards
