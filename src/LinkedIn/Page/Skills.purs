module LinkedIn.Page.Skills where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)
import LinkedIn.CanBeQueried (class CanBeQueried, subQueryNEL)
import LinkedIn.Extractible (class Extractible)
import LinkedIn.Output.Types (Output(..))
import LinkedIn.Profile.Skill as PS
import LinkedIn.UI.Components.ArtDecoTab (ArtDecoTabElement)
import Web.DOM (Document)

newtype SkillsPage a = SkillsPage (NonEmptyList (ArtDecoTabElement a))

derive instance Generic (SkillsPage a) _
derive instance Eq a => Eq (SkillsPage a)
instance Show a => Show (SkillsPage a) where show = genericShow
derive instance Functor SkillsPage

instance Foldable SkillsPage where
  foldMap f (SkillsPage tabs) = foldMap (foldMap f) tabs

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable SkillsPage where
  sequence (SkillsPage tabs) = ado
    ts <- sequence (map sequence tabs)
  in SkillsPage ts

  traverse = \x -> traverseDefault x

instance CanBeQueried Document SkillsPage where
  query n = do
    tabs <- subQueryNEL "div.artdeco-tabs > div > div > div > div > ul > li" n
    pure $ SkillsPage tabs

instance Extractible SkillsPage where
  extract (SkillsPage tabs) = OutSkills <$> traverse PS.fromUI tabs
