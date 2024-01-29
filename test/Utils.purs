module Test.Utils where

import Prelude

import Data.Date (Month)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import LinkedIn.UI.Basic.Parser (toYear)
import LinkedIn.UI.Basic.Types (MonthYear(..))
import Partial.Unsafe (unsafePartial)

toMonthYear' :: Month -> Int -> MonthYear
toMonthYear' m y = MonthYear m y' where
  y' = unsafePartial $ fromJust $ toYear $ toNumber y
