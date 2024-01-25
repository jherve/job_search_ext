module Test.UIStringParser
  ( main
  )
  where

import Data.Date (Month(..))
import Data.Either (Either(..))
import Effect (Effect)
import LinkedIn.UIElements.Parser (durationP, monthYearP, timeSpanP, uiStringP)
import LinkedIn.UIElements.Types (Duration(..), MonthYear, TimeSpan(..), UIString(..))
import Parsing (runParser, ParseError)
import Prelude (Unit, discard)
import Test.Assert (assertEqual)
import Test.Utils (toMonthYear')

runMonthYear ∷ String → Either ParseError MonthYear
runMonthYear s = runParser s monthYearP

runTimeSpan ∷ String → Either ParseError TimeSpan
runTimeSpan s = runParser s timeSpanP

runDuration ∷ String → Either ParseError Duration
runDuration s = runParser s durationP

runUIString ∷ String → Either ParseError UIString
runUIString s = runParser s uiStringP

testMonthYearParser ∷ Effect Unit
testMonthYearParser = do 
  assertEqual {
    actual:  runMonthYear "fév. 2004",
    expected: Right(toMonthYear' February 2004)
  }
  assertEqual {
    actual:  runMonthYear "juin 2012",
    expected: Right(toMonthYear' June 2012)
  }

testTimeSpanParser ∷ Effect Unit
testTimeSpanParser = do 
  assertEqual {
    actual:  runTimeSpan "juin 2012 - aujourd’hui",
    expected: Right(TimeSpanToToday (toMonthYear' June 2012))
  }
  assertEqual {
    actual:  runTimeSpan "juin 2012 - mai 2021",
    expected: Right(TimeSpanBounded (toMonthYear' June 2012) (toMonthYear' May 2021))
  }

testDurationParser ∷ Effect Unit
testDurationParser = do 
  assertEqual {
    actual: runDuration "2 ans 3 mois",
    expected: Right(YearsMonth 2 3)
  }
  assertEqual {
    actual: runDuration "1 an 3 mois",
    expected: Right(YearsMonth 1 3)
  }
  assertEqual {
    actual: runDuration "3 mois",
    expected: Right(Months 3)
  }
  assertEqual {
    actual: runDuration "3 ans",
    expected: Right(Years 3)
  }
  assertEqual {
    actual: runDuration "1 an",
    expected: Right(Years 1)
  }

testUIParserDuration ∷ Effect Unit
testUIParserDuration = do
  assertEqual {
    actual: runUIString "2 ans 3 mois",
    expected: Right(UIStringDuration (YearsMonth 2 3))
  }

testUIParserDotSeparated ∷ Effect Unit
testUIParserDotSeparated = do
  assertEqual {
    actual: runUIString "2 ans 3 mois · some text",
    expected: Right(UIStringDotSeparated (UIStringDuration (YearsMonth 2 3)) (UIStringPlain "some text"))
  }

  assertEqual {
    actual: runUIString "· Boulogne-Billancourt, Île-de-France, France",
    expected: Right(UIStringDotSeparated (UIStringPlain "") (UIStringPlain "Boulogne-Billancourt, Île-de-France, France"))
  }

main :: Effect Unit
main = do
  testMonthYearParser
  testTimeSpanParser
  testDurationParser

  testUIParserDuration
  testUIParserDotSeparated
