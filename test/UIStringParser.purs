module Test.UIStringParser where

import Prelude

import Data.Date (Month(..))
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List.Types (NonEmptyList(..))
import Data.NonEmpty (NonEmpty(..))
import LinkedIn.UI.Basic.Parser (durationP, medianDotSeparated, monthYearP, timeSpanP)
import LinkedIn.UI.Basic.Types (Duration(..), TimeSpan(..))
import LinkedIn.UI.Strings.Parser (uiStringDurationP, uiStringdotSeparatedP)
import LinkedIn.UI.Strings.Types (UIString(..))
import Parsing (ParseError(..), Position(..), runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (toMonthYear')

uiStringParserSpec :: Spec Unit
uiStringParserSpec = do
  describe "month year parser" do
    let run s = runParser s monthYearP

    it "works" do
      run "fév. 2004" `shouldEqual` Right(toMonthYear' February 2004)
      run "juin 2012" `shouldEqual` Right(toMonthYear' June 2012)

  describe "timespan parser" do
    let run s = runParser s timeSpanP

    it "works" do
      run "juin 2012 - aujourd’hui" `shouldEqual` Right(TimeSpanToToday (toMonthYear' June 2012))
      run "juin 2012 - mai 2021" `shouldEqual` Right(TimeSpanBounded (toMonthYear' June 2012) (toMonthYear' May 2021))

  describe "duration parser" do
    let run s = runParser s durationP

    it "works" do
      run "2 ans 3 mois" `shouldEqual` Right(YearsMonth 2 3)
      run "1 an 3 mois" `shouldEqual` Right(YearsMonth 1 3)
      run "3 mois" `shouldEqual` Right(Months 3)
      run "3 ans" `shouldEqual` Right(Years 3)
      run "1 an" `shouldEqual` Right(Years 1)

  describe "dot separated strings parser" do
    let run s = runParser s medianDotSeparated

    it "works" do
      run "some text 1 · some text 2" `shouldEqual` Right(NonEmptyList(NonEmpty "some text 1" ("some text 2" : Nil)))
      run "· some text after a dot" `shouldEqual` Right(NonEmptyList(NonEmpty "some text after a dot" Nil))
      run "some text before a dot ·" `shouldEqual` Right(NonEmptyList(NonEmpty "some text before a dot" Nil))
      run "string with no dot" `shouldEqual` (Left (ParseError "Expected '•'" (Position { column: 19, index: 18, line: 1 })))
      run "· some text in between dots ·" `shouldEqual` Right(NonEmptyList(NonEmpty "some text in between dots" Nil))

  describe "UI duration parser" do
    let run s = runParser s uiStringDurationP

    it "works" do
      run "2 ans 3 mois" `shouldEqual` Right(UIStringDuration (YearsMonth 2 3))

  describe "UI dot separated string parser" do
    let run s = runParser s uiStringdotSeparatedP

    it "works" do
      run "some text 1 · some text 2" `shouldEqual` Right(UIStringDotSeparated (NonEmptyList(NonEmpty (UIStringPlain "some text 1") ((UIStringPlain "some text 2") : Nil))))
