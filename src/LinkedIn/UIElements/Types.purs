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

data UIElement = 
  UIDuration Duration
  | UITimeSpan TimeSpan
  | UIPlainText String
  | UIDotSeparated UIElement UIElement
  | UILink String UIElement
  | UIButton (Maybe String) UIElement

derive instance Generic UIElement _
instance Show UIElement where
  show (UIDotSeparated ui1 ui2) = "(UIDotSeparated " <> show ui1 <> show ui2 <> ")"
  show u = genericShow u
