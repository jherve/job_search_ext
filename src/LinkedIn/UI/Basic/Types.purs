module LinkedIn.UI.Basic.Types where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Decode.Decoders (decodeNumber)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Encoders (encodeNumber, encodeString)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Date (Month(..), Year)
import Data.Either (note)
import Data.Enum (toEnum)
import Data.Generic.Rep (class Generic)
import Data.Int64 (Int64)
import Data.Int64 as I64
import Data.Show.Generic (genericShow)

newtype JobOfferId = JobOfferId Int64

derive instance Eq JobOfferId
derive instance Generic JobOfferId _
instance Show JobOfferId where show = genericShow
instance EncodeJson JobOfferId where
  encodeJson (JobOfferId a) = encodeNumber $ I64.toNumber a
instance DecodeJson JobOfferId where
  decodeJson json = do
    nb <- decodeNumber json
    i64 <- note (UnexpectedValue json) $ I64.fromNumber nb
    pure $ JobOfferId i64

data MonthYear = MonthYear Month Year

derive instance Eq MonthYear
derive instance Generic MonthYear _
instance Show MonthYear where show = genericShow
instance EncodeJson MonthYear where
  encodeJson _ = encodeString "monthyear" -- TODO
instance DecodeJson MonthYear where
  decodeJson _json = note MissingValue $ do
    year <- toEnum 2000
    pure $ MonthYear January year

data TimeSpan =
  TimeSpanBounded MonthYear MonthYear
  | TimeSpanToToday MonthYear

derive instance Eq TimeSpan
derive instance Generic TimeSpan _
instance Show TimeSpan where show = genericShow
instance EncodeJson TimeSpan where encodeJson a = genericEncodeJson a
instance DecodeJson TimeSpan where decodeJson a = genericDecodeJson a

data MonthYearOrToday = MY MonthYear | Today

data Duration =
  Years Int
  | Months Int
  | YearsMonth Int Int

derive instance Eq Duration
derive instance Generic Duration _
instance Show Duration where show = genericShow
instance EncodeJson Duration where encodeJson _ = encodeString "duration" -- TODO
instance DecodeJson Duration where decodeJson a = genericDecodeJson a

data JobFlexibility = JobFlexHybrid | JobFlexOnSite | JobFlexFullRemote

derive instance Eq JobFlexibility
derive instance Generic JobFlexibility _
instance Show JobFlexibility where show = genericShow
instance EncodeJson JobFlexibility where encodeJson a = genericEncodeJson a
instance DecodeJson JobFlexibility where decodeJson a = genericDecodeJson a
