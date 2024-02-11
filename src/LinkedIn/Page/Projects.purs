module LinkedIn.Page.Projects where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)
import LinkedIn.CanBeQueried (class CanBeQueried, subQueryNEL)
import LinkedIn.Extractible (class Extractible)
import LinkedIn.Output.Types (Output(..))
import LinkedIn.Profile.Project as PP
import LinkedIn.UI.Components.ArtDecoCard (ArtDecoCardElement)
import Web.DOM (Document)

data ProjectsPage a = ProjectsPage (NonEmptyList (ArtDecoCardElement a))

derive instance Generic (ProjectsPage a) _
derive instance Eq a => Eq (ProjectsPage a)
instance Show a => Show (ProjectsPage a) where show = genericShow
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

instance CanBeQueried Document ProjectsPage where
  query n = do
    cards <- subQueryNEL "section.artdeco-card > div ~ div > div > div > ul > li" n
    pure $ ProjectsPage cards

instance Extractible ProjectsPage where
  extract (ProjectsPage cards) = OutProjects <$> traverse PP.fromUI cards
