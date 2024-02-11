module LinkedIn.UI.Strings.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Show.Generic (genericShow)
import LinkedIn.UI.Basic.Types (Duration, JobFlexibility, TimeSpan)

data UIString =
  UIStringDuration Duration
  | UIStringTimeSpan TimeSpan
  | UIStringJobFlex JobFlexibility
  | UIStringPlain String
  | UIStringDotSeparated (NonEmptyList UIString)

derive instance Eq UIString
derive instance Generic UIString _
instance Show UIString where show u = genericShow u
