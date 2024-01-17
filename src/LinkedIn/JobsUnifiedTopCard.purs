module LinkedIn.JobsUnifiedTopCard where

import Control.Alt
import Prelude

import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Debug (trace)
import LinkedIn (DetachedNode(..))
import LinkedIn.Types (ParseError(..), Parser)
import LinkedIn.Utils (detachNonEmptyTextChild, parseDetachedNode, queryAndDetachMany, queryAndDetachOne, queryManyAndParse, queryOneAndParse)

data JobsUnifiedTopCardElement = JobsUnifiedTopCardElement {
  header :: DetachedNode,
  primaryDescription :: Maybe TopCardPrimaryDescription,
  insights :: Maybe (NonEmptyList TopCardInsight),
  actions :: Maybe (NonEmptyList TopCardAction)
}

data TopCardPrimaryDescription = TopCardPrimaryDescription {
  link :: DetachedNode,
  text :: DetachedNode,
  tvmText :: Maybe (NonEmptyList DetachedNode)
}

data TopCardInsight = TopCardInsight {
  icon :: DetachedNode,
  content :: TopCardInsightContent
}

data TopCardInsightContent =
  TopCardInsightContentSingle DetachedNode
  | TopCardInsightContentSecondary {primary :: DetachedNode, secondary :: NonEmptyList TopCardSecondaryInsight}
  | TopCardInsightContentButton DetachedNode

data TopCardSecondaryInsight =
  TopCardSecondaryInsightNested DetachedNode
  | TopCardSecondaryInsightPlain DetachedNode

-- External application : <button id="ember74"  class="jobs-apply-button artdeco-button artdeco-button--3 artdeco-button--primary ember-view artdeco-button--icon-right" role="link">
-- LinkedIn Applcation : <button id="ember115" class="jobs-apply-button artdeco-button artdeco-button--3 artdeco-button--primary ember-view" data-job-id="3786945580">
data TopCardAction = TopCardActionApplyButton DetachedNode

derive instance Generic JobsUnifiedTopCardElement _
derive instance Eq JobsUnifiedTopCardElement
instance Show JobsUnifiedTopCardElement where
  show = genericShow

derive instance Generic TopCardPrimaryDescription _
derive instance Eq TopCardPrimaryDescription
instance Show TopCardPrimaryDescription where
  show = genericShow

derive instance Generic TopCardInsight _
derive instance Eq TopCardInsight
instance Show TopCardInsight where
  show = genericShow

derive instance Generic TopCardInsightContent _
derive instance Eq TopCardInsightContent
instance Show TopCardInsightContent where
  show = genericShow

derive instance Generic TopCardSecondaryInsight _
derive instance Eq TopCardSecondaryInsight
instance Show TopCardSecondaryInsight where
  show = genericShow

derive instance Generic TopCardAction _
derive instance Eq TopCardAction
instance Show TopCardAction where
  show = genericShow

parseTopCardAction :: Parser TopCardAction
parseTopCardAction n = do
  self <- parseDetachedNode n

  pure $ ado
    s <- self
  in TopCardActionApplyButton s

parseTopCardSecondaryInsight :: Parser TopCardSecondaryInsight
parseTopCardSecondaryInsight n = do
  nested <- queryAndDetachOne ":scope span[aria-hidden=true]" n
  plain <- parseDetachedNode n

  pure $ case nested, plain of
    Right p@(DetachedElement _), _ -> Right $ TopCardSecondaryInsightNested p
    _, Right p@(DetachedElement _) -> Right $ TopCardSecondaryInsightPlain p
    _, _ -> Left TextNotFoundError

parseTopCardInsightContent :: Parser TopCardInsightContent
parseTopCardInsightContent n = do
  primary <- queryAndDetachOne ":scope > span:first-child span[aria-hidden=true]" n
  secondary <- queryManyAndParse
    ":scope > span.job-details-jobs-unified-top-card__job-insight-view-model-secondary"
    parseTopCardSecondaryInsight
    n
  self <- parseDetachedNode n

  pure $ case primary, secondary, self of
    _, _, Right b@(DetachedElement {tag: "BUTTON"}) -> Right $ TopCardInsightContentButton b
    Right p@(DetachedElement _), Right s, _ -> Right $ TopCardInsightContentSecondary {primary: p, secondary: s}
    _, _, Right el@(DetachedElement _) -> Right $ TopCardInsightContentSingle el
    _, _, _ -> Left TextNotFoundError

parseTopCardInsight :: Parser TopCardInsight
parseTopCardInsight n = do
  icon <- queryAndDetachOne ":scope li-icon" n
  svg <- queryAndDetachOne ":scope svg" n
  content <- queryOneAndParse ":scope > span" parseTopCardInsightContent n
  actionButton <- queryOneAndParse ":scope > button" parseTopCardInsightContent n

  pure $ ado
    i <- icon <|> svg
    c <- content <|> actionButton
  in TopCardInsight {icon: i, content: c}

parseTopCardPrimaryDescription :: Parser TopCardPrimaryDescription
parseTopCardPrimaryDescription n = do
  link <- queryAndDetachOne ":scope > a" n
  text <- detachNonEmptyTextChild n
  tvmText <- queryAndDetachMany "span.tvm__text" n

  pure $ ado
    l <- link
    t <- text
  in TopCardPrimaryDescription {link: l, text: t, tvmText: hush tvmText}

parseJobsUnifiedTopCardElement :: Parser JobsUnifiedTopCardElement
parseJobsUnifiedTopCardElement n = do
  h1 <- queryAndDetachOne "h1.job-details-jobs-unified-top-card__job-title" n
  primary <- queryOneAndParse
    "div.job-details-jobs-unified-top-card__primary-description-container > div"
    parseTopCardPrimaryDescription
    n
  insights <- queryManyAndParse
    "li.job-details-jobs-unified-top-card__job-insight"
    parseTopCardInsight
    n
  actions <- queryManyAndParse
    ".mt5 button"
    parseTopCardAction
    n

  pure $ ado
    h <- h1
  in JobsUnifiedTopCardElement {
    header: h,
    primaryDescription: hush primary,
    insights: hush insights,
    actions: hush actions
  }
