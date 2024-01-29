module LinkedIn.UI.Strings.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Either (hush)
import Data.Tuple (Tuple(..))
import LinkedIn.UI.Basic.Parser (durationP, jobFlexP, medianDotSeparated, timeSpanP)
import LinkedIn.UI.Strings.Types (UIString(..))
import Parsing (Parser, liftMaybe, runParser)
import Parsing.Combinators (try)
import Parsing.String (rest)

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

uiStringSingleP ∷ Parser String UIString
uiStringSingleP = (try uiStringDurationP) <|> (try uiStringTimeSpanP) <|> (try uiStringJobFlexP) <|> uiStringPlainP

uiStringDurationP ∷ Parser String UIString
uiStringDurationP = UIStringDuration <$> durationP

uiStringTimeSpanP ∷ Parser String UIString
uiStringTimeSpanP = UIStringTimeSpan <$> timeSpanP

uiStringJobFlexP ∷ Parser String UIString
uiStringJobFlexP = UIStringJobFlex <$> jobFlexP

uiStringPlainP ∷ Parser String UIString
uiStringPlainP = UIStringPlain <$> rest
