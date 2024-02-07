module Test.PageUrl where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Int64 (Int64)
import Data.Int64 as I64
import Data.Maybe (fromJust)
import LinkedIn.PageUrl (PageUrl(..), pageUrlP)
import LinkedIn.UI.Basic.Types (JobOfferId(..))
import Parsing (runParser)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

toI64 ∷ String → Int64
toI64 s = unsafePartial $ fromJust $ I64.fromString s

pageUrlSpec :: Spec Unit
pageUrlSpec = do
  describe "Page URL parsers" do
    it "projects page" do
      runParser "/in/username/details/projects/" pageUrlP `shouldEqual` Right(UrlProjects "username")
    it "skills page" do
      runParser "/in/username/details/skills/" pageUrlP `shouldEqual` Right(UrlSkills "username")
    it "experience page" do
      runParser "/in/username/details/experience/" pageUrlP `shouldEqual` Right(UrlWorkExperience "username")
    it "languages page" do
      runParser "/in/username/details/languages/" pageUrlP `shouldEqual` Right(UrlLanguage "username")
    it "education page" do
      runParser "/in/username/details/education/" pageUrlP `shouldEqual` Right(UrlEducation "username")
    it "jobs page" do
      runParser "/jobs/view/3764313323/" pageUrlP `shouldEqual` Right(UrlJobOffer (JobOfferId (toI64 "3764313323")))
    it "recommended jobs page" do
      runParser "/jobs/collections/recommended/" pageUrlP `shouldEqual` Right(UrlListRecommendedJobOffers)
    it "jobs search page" do
      runParser "/jobs/search/" pageUrlP `shouldEqual` Right(UrlSearchJobOffers)
    it "not an url" do
      runParser "/not/a/supported/url/" pageUrlP `shouldSatisfy` isLeft
