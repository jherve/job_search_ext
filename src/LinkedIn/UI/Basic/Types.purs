module LinkedIn.UI.Basic.Types where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
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
instance EncodeJson MonthYear where
  encodeJson a = encodeString "monthyear" -- TODO

data TimeSpan =
  TimeSpanBounded MonthYear MonthYear
  | TimeSpanToToday MonthYear

derive instance Eq TimeSpan
derive instance Generic TimeSpan _
instance Show TimeSpan where
  show = genericShow
instance EncodeJson TimeSpan where
  encodeJson a = genericEncodeJson a

data MonthYearOrToday = MY MonthYear | Today

data Duration =
  Years Int
  | Months Int
  | YearsMonth Int Int

derive instance Eq Duration
derive instance Generic Duration _
instance Show Duration where
  show = genericShow
instance EncodeJson Duration where
  encodeJson a = encodeString "duration" -- TODO

data JobFlexibility = JobFlexHybrid | JobFlexOnSite | JobFlexFullRemote

derive instance Eq JobFlexibility
derive instance Generic JobFlexibility _
instance Show JobFlexibility where
  show = genericShow
instance EncodeJson JobFlexibility where
  encodeJson a = genericEncodeJson a
