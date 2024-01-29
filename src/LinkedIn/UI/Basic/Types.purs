module LinkedIn.UI.Basic.Types where

import Prelude

import Data.Date (Month, Year)
import Data.Generic.Rep (class Generic)
import Data.Int64 (Int64)
import Data.Show.Generic (genericShow)

newtype JobOfferId = JobOfferId Int64

derive instance Eq JobOfferId
derive instance Generic JobOfferId _
instance Show JobOfferId where
  show = genericShow

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

data JobFlexibility = JobFlexHybrid | JobFlexOnSite | JobFlexFullRemote

derive instance Eq JobFlexibility
derive instance Generic JobFlexibility _
instance Show JobFlexibility where
  show = genericShow
