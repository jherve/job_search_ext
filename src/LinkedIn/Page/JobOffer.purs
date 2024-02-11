module LinkedIn.Page.JobOffer where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverseDefault)
import LinkedIn.CanBeQueried (class CanBeQueried, subQueryOne)
import LinkedIn.Extractible (class Extractible)
import LinkedIn.Jobs.JobOffer as JJO
import LinkedIn.Output.Types (Output(..))
import LinkedIn.UI.Components.JobsUnifiedTopCard (JobsUnifiedTopCardElement)
import Web.DOM (Document)

data JobOfferPage a = JobOfferPage (JobsUnifiedTopCardElement a)

derive instance Generic (JobOfferPage a) _
derive instance Eq a => Eq (JobOfferPage a)
instance Show a => Show (JobOfferPage a) where show = genericShow
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

instance CanBeQueried Document JobOfferPage where
  query n = do
    card <- subQueryOne "div.jobs-unified-top-card" n
    pure $ JobOfferPage card

instance Extractible JobOfferPage where
  extract (JobOfferPage tabs) = OutJobOffer <$> JJO.fromUI tabs
