module LinkedIn.UI.Components.JobsUnifiedTopCard where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', Prism', Traversal', lens', prism', traversed, view)
import Data.Lens.Record (prop)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence, traverse, traverseDefault)
import Data.Tuple (Tuple(..))
import LinkedIn.CanBeQueried (class CanBeQueried, query)
import LinkedIn.QueryRunner (QueryError(..), QueryRunner', ignoreNotFound, queryAll, queryOne, querySelf, queryText)
import LinkedIn.Queryable (class Queryable, toNode)
import Type.Proxy (Proxy(..))
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
data TopCardAction a = TopCardActionButton a

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

instance Queryable q => CanBeQueried q JobsUnifiedTopCardElement where
  query n = do
    header <- queryOne "h1.job-details-jobs-unified-top-card__job-title" n
    primaryDescription <- query
                            =<< queryOne "div.job-details-jobs-unified-top-card__primary-description-container > div" n
    insights <- ignoreNotFound
                  <<< traverse query
                  =<< queryAll "li.job-details-jobs-unified-top-card__job-insight" n
    actions <- ignoreNotFound <<< traverse query =<< queryAll ".mt5 button" n

    pure $ JobsUnifiedTopCardElement {
      header,
      primaryDescription,
      insights,
      actions
    }

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

instance Queryable q => CanBeQueried q TopCardPrimaryDescription where
  query n = do
    link <- queryOne ":scope > a" n
    text <- queryText 1 n
    tvmText <- ignoreNotFound $ queryAll "span.tvm__text" n

    pure $ TopCardPrimaryDescription {link, text, tvmText: tvmText}

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

instance Queryable q => CanBeQueried q TopCardInsight where
  query n = do
    icon <- queryOne ":scope li-icon" n <|> queryOne ":scope svg" n
    content <- query =<< getContentNode n

    pure $ TopCardInsight {icon, content}

    where
      getContentNode n' = queryOne ":scope > span" n' <|> queryOne ":scope > button" n'

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

instance Queryable q => CanBeQueried q TopCardInsightContent where
  query n =
    queryTopCardInsightContentSecondary n
    <|> queryTopCardInsightContentButton n
    <|> queryTopCardInsightContentSingle n

    where
      queryTopCardInsightContentSingle n = do
        n' <- querySelf n
        pure $ TopCardInsightContentSingle n'

      queryTopCardInsightContentButton n =
        if type_ == "BUTTON"
        then do
          n' <- querySelf n
          pure $ TopCardInsightContentButton n'
        else throwError (QNodeUnexpectedType "BUTTON" type_)

        where type_ = N.nodeName $ toNode n

      queryTopCardInsightContentSecondary n = do
        primary <- queryOne ":scope > span:first-child span[aria-hidden=true]" n
        secondary <- traverse query
                      =<< queryAll ":scope > span.job-details-jobs-unified-top-card__job-insight-view-model-secondary" n
        pure $ TopCardInsightContentSecondary {primary, secondary}

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

instance Queryable q => CanBeQueried q TopCardSecondaryInsight where
  query n = queryTopCardSecondaryInsightNested n <|> queryTopCardSecondaryInsightPlain n
    where
      queryTopCardSecondaryInsightNested n = do
        nested <- queryOne ":scope span[aria-hidden=true]" n
        pure $ TopCardSecondaryInsightNested nested

      queryTopCardSecondaryInsightPlain n = do
        n' <- querySelf n
        pure $ TopCardSecondaryInsightPlain n'

derive instance Generic (TopCardAction a) _
derive instance Eq a => Eq (TopCardAction a)
instance Show a => Show (TopCardAction a) where
  show = genericShow
derive instance Functor TopCardAction

instance Foldable TopCardAction where
  foldMap f (TopCardActionButton a) = f a

  foldl = \x -> foldlDefault x
  foldr = \x -> foldrDefault x

instance Traversable TopCardAction where
  sequence (TopCardActionButton app) = ado
    a <- app
  in TopCardActionButton a

  traverse = \x -> traverseDefault x

instance Queryable q => CanBeQueried q TopCardAction where
  query n = do
    n' <- querySelf n
    pure $ TopCardActionButton n'

toHeader ∷ forall a. JobsUnifiedTopCardElement a → a
toHeader = view $ _top_card <<< prop (Proxy :: Proxy "header")

toPrimaryDescriptionLink ∷ forall a. JobsUnifiedTopCardElement a → a
toPrimaryDescriptionLink = view $ _top_card
  <<< prop (Proxy :: Proxy "primaryDescription")
  <<< _primary_description
  <<< prop (Proxy :: Proxy "link")

toPrimaryDescriptionText ∷ forall a. JobsUnifiedTopCardElement a → a
toPrimaryDescriptionText = view $ _top_card
  <<< prop (Proxy :: Proxy "primaryDescription")
  <<< _primary_description
  <<< prop (Proxy :: Proxy "text")

_top_to_insights ∷ ∀ a. Traversal' (JobsUnifiedTopCardElement a) (TopCardInsight a)
_top_to_insights = _top_card
  <<< prop (Proxy :: Proxy "insights")
  <<< traversed
  <<< traversed

_insight_to_content = prop (Proxy :: Proxy "content")
  <<< traversed

_top_to_action_buttons ∷ ∀ a. Traversal' (JobsUnifiedTopCardElement a) a
_top_to_action_buttons = _top_card
  <<< prop (Proxy :: Proxy "actions")
  <<< traversed
  <<< traversed
  <<< _action_button

_top_card ∷ forall a. Lens' (JobsUnifiedTopCardElement a) { actions ∷ Maybe (NonEmptyList (TopCardAction a)) , header ∷ a , insights ∷ Maybe (NonEmptyList (TopCardInsight a)) , primaryDescription ∷ TopCardPrimaryDescription a }
_top_card = lens' \(JobsUnifiedTopCardElement c) -> Tuple c \c' -> JobsUnifiedTopCardElement c'

_insight ∷ forall a. Lens' (TopCardInsight a) { content ∷ TopCardInsightContent a , icon ∷ a }
_insight = lens' \(TopCardInsight i) -> Tuple i \i' -> TopCardInsight i'

_action_button ∷ forall a. Lens' (TopCardAction a) a
_action_button = lens' \(TopCardActionButton i) -> Tuple i \i' -> TopCardActionButton i'

_primary_description ∷ ∀ a. Lens' (TopCardPrimaryDescription a) { link ∷ a , text ∷ a , tvmText ∷ Maybe (NonEmptyList a) }
_primary_description = lens' \(TopCardPrimaryDescription i) -> Tuple i \i' -> TopCardPrimaryDescription i'

_insight_content_single ∷ forall a. Prism' (TopCardInsightContent a) a
_insight_content_single = prism' TopCardInsightContentSingle case _ of
  TopCardInsightContentSingle i -> Just i
  _ -> Nothing

_insight_content_button ∷ forall a. Prism' (TopCardInsightContent a) a
_insight_content_button = prism' TopCardInsightContentButton case _ of
  TopCardInsightContentButton i -> Just i
  _ -> Nothing

_insight_content_secondary ∷ forall a. Prism' (TopCardInsightContent a) { primary ∷ a , secondary ∷ NonEmptyList (TopCardSecondaryInsight a) }
_insight_content_secondary = prism' TopCardInsightContentSecondary case _ of
  TopCardInsightContentSecondary i -> Just i
  _ -> Nothing

_insight_content_secondary_nested ∷ forall a. Prism' (TopCardSecondaryInsight a) a
_insight_content_secondary_nested = prism' TopCardSecondaryInsightNested case _ of
  TopCardSecondaryInsightNested i -> Just i
  _ -> Nothing

_insight_content_secondary_plain ∷ forall a. Prism' (TopCardSecondaryInsight a) a
_insight_content_secondary_plain = prism' TopCardSecondaryInsightPlain case _ of
  TopCardSecondaryInsightPlain i -> Just i
  _ -> Nothing
