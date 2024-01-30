module LinkedIn.Page.WorkExperiences where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)
import LinkedIn.Extractible (class Extractible)
import LinkedIn.Output.Types (Output(..))
import LinkedIn.Profile.WorkExperience as PWE
import LinkedIn.QueryRunner (subQueryMany)
import LinkedIn.UI.Components.ArtDecoCard (ArtDecoCardElement, queryArtDecoCard)

data WorkExperiencesPage a = WorkExperiencesPage (NonEmptyList (ArtDecoCardElement a))

derive instance Generic (WorkExperiencesPage a) _
derive instance Eq a => Eq (WorkExperiencesPage a)
instance Show a => Show (WorkExperiencesPage a) where
  show = genericShow
derive instance Functor WorkExperiencesPage

instance Foldable WorkExperiencesPage where
  foldMap f (WorkExperiencesPage cards) = foldMap (foldMap f) cards

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable WorkExperiencesPage where
  sequence (WorkExperiencesPage cards) = ado
    ts <- sequence (map sequence cards)
  in WorkExperiencesPage ts

  traverse = \x -> traverseDefault x

instance Extractible WorkExperiencesPage where
  query n = do
    cards <- subQueryMany queryArtDecoCard "section.artdeco-card > div ~ div > div > div > ul > li" n
    pure $ WorkExperiencesPage cards

  extract (WorkExperiencesPage cards) = OutWorkExperiences <$> traverse PWE.fromUI cards
