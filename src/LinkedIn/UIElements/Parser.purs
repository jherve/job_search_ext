module LinkedIn.UIElements.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Array as A
import Data.Date (Month(..), Year)
import Data.Either (hush)
import Data.Enum (toEnum)
import Data.Int (fromNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String as S
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple (Tuple(..))
import LinkedIn.UIElements.Types (Duration(..), MonthYear(..), MonthYearOrToday(..), TimeSpan(..), UIString(..))
import Parsing (Parser, ParserT, fail, liftMaybe, runParser)
import Parsing.Combinators (choice, try)
import Parsing.String (char, rest, string)
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

uiStringWithoutMedianDotP ∷ Parser String UIString
uiStringWithoutMedianDotP = do
  s <- rest
  liftMaybe (\_ -> "nope") $ hush $ runParser s uiStringSingleP

uiStringdotSeparatedP ∷ Parser String UIString
uiStringdotSeparatedP = do
  Tuple s1 s2 <- medianDotSeparated

  let
    intoUiElement :: String -> Parser String UIString
    intoUiElement s = liftMaybe (\_ -> "could not convert to ui element") $ hush $ runParser s uiStringSingleP

  s1' <- intoUiElement s1
  s2' <- intoUiElement s2

  pure $ UIStringDotSeparated s1' s2'

sepBy2 :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
sepBy2 p sep = do
  a0 <- p
  a1 <- sep *> p
  as <- L.manyRec $ sep *> p
  pure $ NEL.cons a0 $ NEL.cons' a1 as

commaSeparated ∷ Parser String (NonEmptyList String)
commaSeparated = stringWithoutCommaP `sepBy2` commaP

medianDotSeparated ∷ Parser String (Tuple String String)
medianDotSeparated = do
  a0 <- stringWithoutMedianDotP
  a1 <- medianDotP *> rest
  pure $ Tuple (S.trim a0) (S.trim a1)

uiStringSingleP ∷ Parser String UIString
uiStringSingleP = (try uiStringDurationP) <|> (try uiStringTimeSpanP) <|> uiStringPlainP

uiStringDurationP ∷ Parser String UIString
uiStringDurationP = UIStringDuration <$> durationP

uiStringTimeSpanP ∷ Parser String UIString
uiStringTimeSpanP = UIStringTimeSpan <$> timeSpanP

uiStringPlainP ∷ Parser String UIString
uiStringPlainP = UIStringPlain <$> rest
