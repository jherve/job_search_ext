module LinkedIn.PageUrl (PageUrl(..), pageUrlP) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Int64 as I64
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (codePointFromChar)
import LinkedIn.UI.Basic.Types (JobOfferId(..))
import Parsing (Parser, fail)
import Parsing.Combinators (try)
import Parsing.String (char, string)
import Parsing.String.Basic (takeWhile)

data PageUrl =
  UrlProfileMain String
  | UrlProjects String
  | UrlSkills String
  | UrlWorkExperience String
  | UrlEducation String
  | UrlLanguage String
  | UrlJobOffer JobOfferId
  | UrlListRecommendedJobOffers
  | UrlSearchJobOffers

derive instance Eq PageUrl
derive instance Generic PageUrl _
instance Show PageUrl where show = genericShow
instance EncodeJson PageUrl where encodeJson a = genericEncodeJson a

pathComponentP :: String -> Parser String Unit
pathComponentP s = do
  _ <- char('/')
  _ <- string(s)
  pure unit

unknownPathComponentP ∷ Parser String String
unknownPathComponentP = do
  _ <- char('/')
  takeWhile (\c -> c /= codePointFromChar '/')

profileMainP ∷ Parser String PageUrl
profileMainP = do
  _ <- pathComponentP("in")
  name <- unknownPathComponentP
  pure $ UrlProfileMain name

detailsP ∷ String -> Parser String String
detailsP s = do
  _ <- pathComponentP("in")
  name <- unknownPathComponentP
  _ <- pathComponentP("details")
  _ <- pathComponentP(s)
  _ <- char('/')

  pure name

projectP ∷ Parser String PageUrl
projectP = do
  name <- detailsP("projects")
  pure $ UrlProjects name

skillsP ∷ Parser String PageUrl
skillsP = do
  name <- detailsP("skills")
  pure $ UrlSkills name

workExperienceP ∷ Parser String PageUrl
workExperienceP = do
  name <- detailsP("experience")
  pure $ UrlWorkExperience name

educationP ∷ Parser String PageUrl
educationP = do
  name <- detailsP("education")
  pure $ UrlEducation name

languagesP ∷ Parser String PageUrl
languagesP = do
  name <- detailsP("languages")
  pure $ UrlLanguage name

jobViewP :: Parser String PageUrl
jobViewP = do
  _ <- pathComponentP("jobs")
  _ <- pathComponentP("view")
  id <- unknownPathComponentP

  case I64.fromString id of
    Nothing -> fail "Not an int"
    Just i -> pure $ UrlJobOffer (JobOfferId i)

recommendedJobsP :: Parser String PageUrl
recommendedJobsP = do
  _ <- pathComponentP("jobs")
  _ <- pathComponentP("collections")
  _ <- pathComponentP("recommended")
  pure UrlListRecommendedJobOffers

jobSearchP :: Parser String PageUrl
jobSearchP = do
  _ <- pathComponentP("jobs")
  _ <- pathComponentP("search")
  pure UrlSearchJobOffers

pageUrlP ∷ Parser String PageUrl
pageUrlP = try projectP
  <|> try skillsP
  <|> try workExperienceP
  <|> try educationP
  <|> try languagesP
  <|> try profileMainP
  <|> try jobViewP
  <|> try recommendedJobsP
  <|> try jobSearchP
