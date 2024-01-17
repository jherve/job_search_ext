module LinkedIn.JobsUnifiedTopCard where

import Prelude

import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import LinkedIn (DetachedNode)
import LinkedIn.Types (Parser)
import LinkedIn.Utils (detachNonEmptyTextChild, queryAndDetachMany, queryAndDetachOne, queryOneAndParse)

data JobsUnifiedTopCardElement = JobsUnifiedTopCardElement {
  header :: DetachedNode,
  primaryDescription :: Maybe TopCardPrimaryDescription
}

data TopCardPrimaryDescription = TopCardPrimaryDescription {
  link :: DetachedNode,
  text :: DetachedNode,
  neutral :: Maybe (NonEmptyList DetachedNode)
}

derive instance Generic JobsUnifiedTopCardElement _
derive instance Eq JobsUnifiedTopCardElement
instance Show JobsUnifiedTopCardElement where
  show = genericShow

derive instance Generic TopCardPrimaryDescription _
derive instance Eq TopCardPrimaryDescription
instance Show TopCardPrimaryDescription where
  show = genericShow

parseTopCardPrimaryDescription :: Parser TopCardPrimaryDescription
parseTopCardPrimaryDescription n = do
  link <- queryAndDetachOne ":scope > a" n
  text <- detachNonEmptyTextChild n
  neutral <- queryAndDetachMany "span.tvm__text--neutral" n
  
  pure $ ado
    l <- link
    t <- text
  in TopCardPrimaryDescription {link: l, text: t, neutral: hush neutral}

parseJobsUnifiedTopCardElement :: Parser JobsUnifiedTopCardElement
parseJobsUnifiedTopCardElement n = do
  h1 <- queryAndDetachOne "h1.job-details-jobs-unified-top-card__job-title" n
  primary <- queryOneAndParse 
    "div.job-details-jobs-unified-top-card__primary-description-container > div" 
    parseTopCardPrimaryDescription
    n

  pure $ ado
    h <- h1
  in JobsUnifiedTopCardElement {header: h, primaryDescription: hush primary}
