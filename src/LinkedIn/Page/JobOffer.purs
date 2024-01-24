module LinkedIn.Page.JobOffer where

import Prelude

import Data.Either (Either)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)
import Effect (Effect)
import LinkedIn.DetachedNode (toDetached)
import LinkedIn.Jobs.JobOffer (JobOffer)
import LinkedIn.Jobs.JobOffer as JJO
import LinkedIn.JobsUnifiedTopCard (JobsUnifiedTopCardElement, queryJobsUnifiedTopCardElement)
import LinkedIn.QueryRunner (QueryRunner', subQueryOne)
import Web.DOM (Document, Node)

data JobOfferPage a = JobOfferPage (JobsUnifiedTopCardElement a)

derive instance Generic (JobOfferPage a) _
derive instance Eq a => Eq (JobOfferPage a)
instance Show a => Show (JobOfferPage a) where
  show = genericShow
derive instance Functor JobOfferPage

instance Foldable JobOfferPage where
  foldMap f (JobOfferPage topCard) = foldMap f topCard

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable JobOfferPage where
  sequence (JobOfferPage topCard) = ado
    tc <- sequence topCard
  in JobOfferPage tc

  traverse = \x -> traverseDefault x

query :: QueryRunner' Document (JobOfferPage Node)
query n = do
  card <- subQueryOne queryJobsUnifiedTopCardElement "div.jobs-unified-top-card" n
  pure $ JobOfferPage card

extract ∷ JobOfferPage Node → Effect (Either String JobOffer)
extract p = do
  detached <- traverse toDetached p
  let
    JobOfferPage tabs = detached
  pure $ JJO.fromUI tabs
