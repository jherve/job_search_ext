module LinkedIn.JobsUnifiedTopCard where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)
import LinkedIn.QueryRunner (QueryError(..), QueryRunner, chooseOne, chooseOne3, ignoreNotFound, queryAll, queryOne, queryText)
import Web.DOM (Node)
import Web.DOM.Node as N

data JobsUnifiedTopCardElement a = JobsUnifiedTopCardElement {
  header :: a,
  primaryDescription :: TopCardPrimaryDescription a,
  insights :: Maybe (NonEmptyList (TopCardInsight a)),
  actions :: Maybe (NonEmptyList (TopCardAction a))
}

data TopCardPrimaryDescription a = TopCardPrimaryDescription {
  link :: a,
  text :: a,
  tvmText :: Maybe (NonEmptyList a)
}

data TopCardInsight a = TopCardInsight {
  icon :: a,
  content :: TopCardInsightContent a
}

data TopCardInsightContent a =
  TopCardInsightContentSingle a
  | TopCardInsightContentSecondary {primary :: a, secondary :: NonEmptyList (TopCardSecondaryInsight a)}
  | TopCardInsightContentButton a

data TopCardSecondaryInsight a =
  TopCardSecondaryInsightNested a
  | TopCardSecondaryInsightPlain a

-- External application : <button id="ember74"  class="jobs-apply-button artdeco-button artdeco-button--3 artdeco-button--primary ember-view artdeco-button--icon-right" role="link">
-- LinkedIn Applcation : <button id="ember115" class="jobs-apply-button artdeco-button artdeco-button--3 artdeco-button--primary ember-view" data-job-id="3786945580">
data TopCardAction a = TopCardActionApplyButton a

derive instance Generic (JobsUnifiedTopCardElement a) _
derive instance Eq a => Eq (JobsUnifiedTopCardElement a)
instance Show a => Show (JobsUnifiedTopCardElement a) where
  show = genericShow
derive instance Functor JobsUnifiedTopCardElement

instance Foldable JobsUnifiedTopCardElement where
  foldMap f (JobsUnifiedTopCardElement {header, primaryDescription, insights, actions}) =
    f header
    <> foldMap f primaryDescription
    <> foldMap (foldMap (foldMap f)) insights
    <> foldMap (foldMap (foldMap f)) actions

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable JobsUnifiedTopCardElement where
  sequence (JobsUnifiedTopCardElement {header, primaryDescription, insights, actions}) = ado
    h <- header
    pd <- sequence primaryDescription
    i <- traverseMayNel insights
    a <- traverseMayNel actions
  in JobsUnifiedTopCardElement {header: h, primaryDescription: pd, insights: i, actions: a}

  traverse = \x -> traverseDefault x

traverseMayNel :: forall m t a. Traversable t => Applicative m => Maybe(NonEmptyList (t (m a))) -> m (Maybe (NonEmptyList (t a)))
traverseMayNel (Just o) = map pure (sequence (map sequence o))
traverseMayNel Nothing = pure Nothing

derive instance Generic (TopCardPrimaryDescription a) _
derive instance Eq a => Eq (TopCardPrimaryDescription a)
instance Show a => Show (TopCardPrimaryDescription a) where
  show = genericShow
derive instance Functor TopCardPrimaryDescription

instance Foldable TopCardPrimaryDescription where
  foldMap f (TopCardPrimaryDescription {link, text, tvmText}) = f link <> f text <> foldMap (foldMap f) tvmText

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable TopCardPrimaryDescription where
  sequence (TopCardPrimaryDescription {link, text, tvmText}) = ado
    l <- link
    t <- text
    tvm <- sequence (map sequence tvmText)
  in TopCardPrimaryDescription {link: l, text: t, tvmText: tvm}

  traverse = \x -> traverseDefault x

derive instance Generic (TopCardInsight a) _
derive instance Eq a => Eq (TopCardInsight a)
instance Show a => Show (TopCardInsight a) where
  show = genericShow
derive instance Functor TopCardInsight

instance Foldable TopCardInsight where
  foldMap f (TopCardInsight {icon, content}) = f icon <> foldMap f content

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable TopCardInsight where
  sequence (TopCardInsight {icon, content}) = ado
    i <- icon
    c <- sequence content
  in TopCardInsight {icon: i, content: c}

  traverse = \x -> traverseDefault x

derive instance Generic (TopCardInsightContent a) _
derive instance Eq a => Eq (TopCardInsightContent a)
instance Show a => Show (TopCardInsightContent a) where
  show = genericShow
derive instance Functor TopCardInsightContent

instance Foldable TopCardInsightContent where
  foldMap f (TopCardInsightContentSingle a) = f a
  foldMap f (TopCardInsightContentButton a) = f a
  foldMap f (TopCardInsightContentSecondary {primary, secondary}) = f primary <> foldMap (foldMap f) secondary

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable TopCardInsightContent where
  sequence (TopCardInsightContentSingle ins) = TopCardInsightContentSingle <$> ins
  sequence (TopCardInsightContentButton ins) = TopCardInsightContentButton <$> ins
  sequence (TopCardInsightContentSecondary {primary, secondary}) = ado
    p <- primary
    s <- sequence (map sequence secondary)
  in TopCardInsightContentSecondary {primary: p, secondary: s}

  traverse = \x -> traverseDefault x

derive instance Generic (TopCardSecondaryInsight a) _
derive instance Eq a => Eq (TopCardSecondaryInsight a)
instance Show a => Show (TopCardSecondaryInsight a) where
  show = genericShow
derive instance Functor TopCardSecondaryInsight

instance Foldable TopCardSecondaryInsight where
  foldMap f (TopCardSecondaryInsightNested a) = f a
  foldMap f (TopCardSecondaryInsightPlain a) = f a

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable TopCardSecondaryInsight where
  sequence (TopCardSecondaryInsightNested ins) = TopCardSecondaryInsightNested <$> ins
  sequence (TopCardSecondaryInsightPlain ins) = TopCardSecondaryInsightPlain <$> ins

  traverse = \x -> traverseDefault x

derive instance Generic (TopCardAction a) _
derive instance Eq a => Eq (TopCardAction a)
instance Show a => Show (TopCardAction a) where
  show = genericShow
derive instance Functor TopCardAction

instance Foldable TopCardAction where
  foldMap f (TopCardActionApplyButton a) = f a

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable TopCardAction where
  sequence (TopCardActionApplyButton app) = ado
    a <- app
  in TopCardActionApplyButton a

  traverse = \x -> traverseDefault x

queryTopCardAction :: QueryRunner (TopCardAction Node)
queryTopCardAction n = pure $ TopCardActionApplyButton n

queryTopCardSecondaryInsightNested :: QueryRunner (TopCardSecondaryInsight Node)
queryTopCardSecondaryInsightNested n = do
  nested <- queryOne ":scope span[aria-hidden=true]" n
  pure $ TopCardSecondaryInsightNested nested

queryTopCardSecondaryInsightPlain :: QueryRunner (TopCardSecondaryInsight Node)
queryTopCardSecondaryInsightPlain n = pure $ TopCardSecondaryInsightPlain n

queryTopCardSecondaryInsight :: QueryRunner (TopCardSecondaryInsight Node)
queryTopCardSecondaryInsight n =
  chooseOne queryTopCardSecondaryInsightNested queryTopCardSecondaryInsightPlain n

queryTopCardInsightContentSingle :: QueryRunner (TopCardInsightContent Node)
queryTopCardInsightContentSingle n = pure $ TopCardInsightContentSingle n

queryTopCardInsightContentButton :: QueryRunner (TopCardInsightContent Node)
queryTopCardInsightContentButton n =
  if type_ == "BUTTON"
  then pure $ TopCardInsightContentButton n
  else throwError (QNodeUnexpectedType "BUTTON" type_)

  where type_ = N.nodeName n

queryTopCardInsightContentSecondary :: QueryRunner (TopCardInsightContent Node)
queryTopCardInsightContentSecondary n = do
  primary <- queryOne ":scope > span:first-child span[aria-hidden=true]" n
  secondary <- traverse queryTopCardSecondaryInsight
                =<< queryAll ":scope > span.job-details-jobs-unified-top-card__job-insight-view-model-secondary" n
  pure $ TopCardInsightContentSecondary {primary, secondary}

queryTopCardInsightContent :: QueryRunner (TopCardInsightContent Node)
queryTopCardInsightContent n =
  chooseOne3 queryTopCardInsightContentSecondary queryTopCardInsightContentButton queryTopCardInsightContentSingle n

queryTopCardInsight :: QueryRunner (TopCardInsight Node)
queryTopCardInsight n = do
  icon <- chooseOne (queryOne ":scope li-icon") (queryOne ":scope svg") n
  content <- queryTopCardInsightContent =<< getContentNode n

  pure $ TopCardInsight {icon, content}

  where
    getContentNode = chooseOne (queryOne ":scope > span") (queryOne ":scope > button")

queryTopCardPrimaryDescription :: QueryRunner (TopCardPrimaryDescription Node)
queryTopCardPrimaryDescription n = do
  link <- queryOne ":scope > a" n
  text <- queryText 1 n
  tvmText <- ignoreNotFound $ queryAll "span.tvm__text" n

  pure $ TopCardPrimaryDescription {link, text, tvmText: tvmText}

queryJobsUnifiedTopCardElement :: QueryRunner (JobsUnifiedTopCardElement Node)
queryJobsUnifiedTopCardElement n = do
  header <- queryOne "h1.job-details-jobs-unified-top-card__job-title" n
  primaryDescription <- queryTopCardPrimaryDescription
                          =<< queryOne "div.job-details-jobs-unified-top-card__primary-description-container > div" n
  insights <- ignoreNotFound
                <<< traverse queryTopCardInsight
                =<< queryAll "li.job-details-jobs-unified-top-card__job-insight" n
  actions <- ignoreNotFound <<< traverse queryTopCardAction =<< queryAll ".mt5 button" n

  pure $ JobsUnifiedTopCardElement {
    header,
    primaryDescription,
    insights,
    actions
  }

toHeader ∷ forall a. JobsUnifiedTopCardElement a → a
toHeader (JobsUnifiedTopCardElement {header}) = header

toPrimaryDescriptionLink ∷ forall a. JobsUnifiedTopCardElement a → a
toPrimaryDescriptionLink (JobsUnifiedTopCardElement {
  primaryDescription: TopCardPrimaryDescription {link}
}) = link

toPrimaryDescriptionText ∷ forall a. JobsUnifiedTopCardElement a → a
toPrimaryDescriptionText (JobsUnifiedTopCardElement {
  primaryDescription: TopCardPrimaryDescription {text}
}) = text
