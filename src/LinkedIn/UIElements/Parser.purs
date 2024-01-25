module LinkedIn.UIElements.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Array as A
import Data.Date (Month(..), Year)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Int (fromNumber)
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple (Tuple(..))
import LinkedIn.UIElements.Types (Duration(..), MonthYear(..), MonthYearOrToday(..), TimeSpan(..), UIString(..))
import Parsing (Parser, fail, runParser)
import Parsing.Combinators (choice, try)
import Parsing.String (string, char, rest)
import Parsing.String.Basic (intDecimal, number, space, takeWhile)

monthStrToMonth :: Map String Month
monthStrToMonth = M.fromFoldable (
  Tuple "janv." January
  : Tuple "fév." February
  : Tuple "mars" March
  : Tuple "avr." April
  : Tuple "mai" May
  : Tuple "juin" June
  : Tuple "juil." July
  : Tuple "août" August
  : Tuple "sept." September
  : Tuple "oct." October
  : Tuple "nov." November
  : Tuple "déc." December
  : Nil
)

toMonth :: String -> Maybe Month
toMonth month_ = M.lookup month_ monthStrToMonth

toYear :: Number -> Maybe Year
toYear year_ = case fromNumber year_ of
  Just y -> toEnum y
  Nothing -> Nothing

monthP :: Parser String Month
monthP = do
  a <- choice $ string <$> (A.fromFoldable $ M.keys monthStrToMonth)
  case toMonth a of
    Just month_ -> pure month_
    Nothing -> fail "Not a month"

yearP :: Parser String Year
yearP = do
  a <- number
  case toYear a of
    Just year_ -> pure year_
    Nothing -> fail "Not a year"


monthYearP :: Parser String MonthYear
monthYearP = do
  m <- monthP
  _ <- space
  y <- yearP
  pure $ MonthYear m y

todayP :: Parser String MonthYearOrToday
todayP = do
  _ <- string("aujourd’hui")
  pure Today

timeSpanP :: Parser String TimeSpan
timeSpanP = do
  start <- monthYearP
  _ <- space
  _ <- char('-')
  _ <- space
  end <- MY <$> monthYearP <|> todayP

  pure $ case end of
    Today -> TimeSpanToToday start
    MY my -> TimeSpanBounded start my


durationP :: Parser String Duration
durationP = (try yearsMonthP) <|> (try monthsP) <|> yearsP where
  yearsInt = do
    y <- intDecimal
    _ <- space
    _ <- try (string("ans")) <|> string("an")

    pure y

  monthInt = do
    m <- intDecimal
    _ <- space
    _ <- string("mois")

    pure m

  yearsP = do
    y <- yearsInt
    pure $ Years y

  monthsP = do
    y <- monthInt
    pure $ Months y

  yearsMonthP = do
    y <- yearsInt
    _ <- space
    m <- monthInt

    pure $ YearsMonth y m

medianDotP ∷ Parser String Char
medianDotP = char('·') <|> char('•')

commaP :: Parser String Char
commaP = char(',')

stringWithoutCommaP :: Parser String String
stringWithoutCommaP = takeWhile (\c -> c /= codePointFromChar ',')

stringWithoutMedianDotP :: Parser String String
stringWithoutMedianDotP = takeWhile (\c -> c /= codePointFromChar '·' && c /= codePointFromChar '•')

uiStringP :: Parser String UIString
uiStringP = (try uiStringdotSeparatedP) <|> uiStringSingleP

uiStringdotSeparatedP ∷ Parser String UIString
uiStringdotSeparatedP = do
  subStr <- stringWithoutMedianDotP
  _ <- medianDotP
  _ <- space
  sub2Str <- rest
  case runParser subStr uiStringSingleP of
    Right sub -> case runParser sub2Str uiStringSingleP of
      Right sub2 -> pure $ UIStringDotSeparated sub sub2
      Left _ -> fail "not a sub"
    Left _ -> fail "not a sub"

uiStringSingleP ∷ Parser String UIString
uiStringSingleP = (try uiStringDurationP) <|> (try uiStringTimeSpanP) <|> uiStringPlainP

uiStringDurationP ∷ Parser String UIString
uiStringDurationP = UIStringDuration <$> durationP

uiStringTimeSpanP ∷ Parser String UIString
uiStringTimeSpanP = UIStringTimeSpan <$> timeSpanP

uiStringPlainP ∷ Parser String UIString
uiStringPlainP = UIStringPlain <$> rest
