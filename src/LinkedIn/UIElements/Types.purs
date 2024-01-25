module LinkedIn.UIElements.Types where

import Prelude

import Data.Date (Month, Year)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data MonthYear = MonthYear Month Year

derive instance Eq MonthYear
derive instance Generic MonthYear _
instance Show MonthYear where
  show = genericShow

data TimeSpan =
  TimeSpanBounded MonthYear MonthYear
  | TimeSpanToToday MonthYear

derive instance Eq TimeSpan
derive instance Generic TimeSpan _
instance Show TimeSpan where
  show = genericShow

data MonthYearOrToday = MY MonthYear | Today

data Duration =
  Years Int
  | Months Int
  | YearsMonth Int Int

derive instance Eq Duration
derive instance Generic Duration _
instance Show Duration where
  show = genericShow

data UIString =
  UIStringDuration Duration
  | UIStringTimeSpan TimeSpan
  | UIStringPlain String
  | UIStringDotSeparated UIString UIString

derive instance Generic UIString _
instance Show UIString where
  show (UIStringDotSeparated ui1 ui2) = "(UIStringDotSeparated " <> show ui1 <> show ui2 <> ")"
  show u = genericShow u

data UIElement =
  UIElement UIString
  | UILink String UIString
  | UIButton (Maybe String) UIString
  | UIIcon String

derive instance Generic UIElement _
instance Show UIElement where
  show = genericShow
