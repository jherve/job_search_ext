module LinkedIn.Page.WorkExperiences where

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
import LinkedIn.Profile.WorkExperience (WorkExperience)
import LinkedIn.Profile.WorkExperience as PWE
import LinkedIn.QueryRunner (QueryRunner', subQueryMany)
import Web.DOM (Document, Node)

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

query :: QueryRunner' Document (WorkExperiencesPage Node)
query n = do
  cards <- subQueryMany queryArtDecoCard "section.artdeco-card > div ~ div > div > div > ul > li" n
  pure $ WorkExperiencesPage cards

extract ∷ WorkExperiencesPage Node → Effect (Either String (NonEmptyList WorkExperience))
extract p = do
  detached <- traverse toDetached p
  let
    WorkExperiencesPage cards = detached
  pure $ traverse PWE.fromUI cards
