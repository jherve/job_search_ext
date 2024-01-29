module Test.UIStringParser
  ( main
  )
  where

import Prelude

import Data.Date (Month(..))
import Data.Either (Either(..))
import Effect (Effect)
import LinkedIn.UI.Basic.Parser (durationP, monthYearP, timeSpanP)
import LinkedIn.UI.Basic.Types (Duration(..), TimeSpan(..))
import LinkedIn.UI.Strings.Parser (uiStringDurationP, uiStringdotSeparatedP)
import LinkedIn.UI.Strings.Types (UIString(..))
import Parsing (ParseError(..), Position(..), runParser)
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
    actual: run "some text 1 · some text 2",
    expected: Right(UIStringDotSeparated (UIStringPlain "some text 1") (UIStringPlain "some text 2"))
  }

  assertEqual {
    actual: run "· some text after a dot",
    expected: Right(UIStringDotSeparated (UIStringPlain "") (UIStringPlain "some text after a dot"))
  }

  assertEqual {
    actual: run "some text before a dot ·",
    expected: Right(UIStringDotSeparated (UIStringPlain "some text before a dot") (UIStringPlain ""))
  }

  assertEqual {
    actual: run "string with no dot",
    expected: (Left (ParseError "Expected '•'" (Position { column: 19, index: 18, line: 1 })))
  }

  where run s = runParser s uiStringdotSeparatedP

main :: Effect Unit
main = do
  testMonthYearParser
  testTimeSpanParser
  testDurationParser

  testUIParserDuration
  testUIParserDotSeparated
