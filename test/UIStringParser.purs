module Test.UIStringParser
  ( main
  )
  where

import Data.Date (Month(..))
import Data.Either (Either(..))
import Effect (Effect)
import LinkedIn.UIElements.Parser (durationP, monthYearP, timeSpanP, uiStringDurationP, uiStringdotSeparatedP)
import LinkedIn.UIElements.Types (Duration(..), TimeSpan(..), UIString(..))
import Parsing (runParser)
import Prelude (Unit, discard)
import Test.Assert (assertEqual)
import Test.Utils (toMonthYear')

testMonthYearParser ∷ Effect Unit
testMonthYearParser = do 
  assertEqual {
    actual:  run "fév. 2004",
    expected: Right(toMonthYear' February 2004)
  }
  assertEqual {
    actual:  run "juin 2012",
    expected: Right(toMonthYear' June 2012)
  }

  where run s = runParser s monthYearP

testTimeSpanParser ∷ Effect Unit
testTimeSpanParser = do 
  assertEqual {
    actual:  run "juin 2012 - aujourd’hui",
    expected: Right(TimeSpanToToday (toMonthYear' June 2012))
  }
  assertEqual {
    actual:  run "juin 2012 - mai 2021",
    expected: Right(TimeSpanBounded (toMonthYear' June 2012) (toMonthYear' May 2021))
  }

  where run s = runParser s timeSpanP

testDurationParser ∷ Effect Unit
testDurationParser = do 
  assertEqual {
    actual: run "2 ans 3 mois",
    expected: Right(YearsMonth 2 3)
  }
  assertEqual {
    actual: run "1 an 3 mois",
    expected: Right(YearsMonth 1 3)
  }
  assertEqual {
    actual: run "3 mois",
    expected: Right(Months 3)
  }
  assertEqual {
    actual: run "3 ans",
    expected: Right(Years 3)
  }
  assertEqual {
    actual: run "1 an",
    expected: Right(Years 1)
  }

  where run s = runParser s durationP

testUIParserDuration ∷ Effect Unit
testUIParserDuration = do
  assertEqual {
    actual: run "2 ans 3 mois",
    expected: Right(UIStringDuration (YearsMonth 2 3))
  }

  where run s = runParser s uiStringDurationP

testUIParserDotSeparated ∷ Effect Unit
testUIParserDotSeparated = do
  assertEqual {
    actual: run "2 ans 3 mois · some text",
    expected: Right(UIStringDotSeparated (UIStringDuration (YearsMonth 2 3)) (UIStringPlain "some text"))
  }

  assertEqual {
    actual: run "· Boulogne-Billancourt, Île-de-France, France",
    expected: Right(UIStringDotSeparated (UIStringPlain "") (UIStringPlain "Boulogne-Billancourt, Île-de-France, France"))
  }

  where run s = runParser s uiStringdotSeparatedP

main :: Effect Unit
main = do
  testMonthYearParser
  testTimeSpanParser
  testDurationParser

  testUIParserDuration
  testUIParserDotSeparated
