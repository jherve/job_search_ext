module LinkedIn.UI.Strings.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import LinkedIn.UI.Basic.Types (Duration, JobFlexibility, TimeSpan)

data UIString =
  UIStringDuration Duration
  | UIStringTimeSpan TimeSpan
  | UIStringJobFlex JobFlexibility
  | UIStringPlain String
  | UIStringDotSeparated UIString UIString

derive instance Eq UIString
derive instance Generic UIString _
instance Show UIString where
  show (UIStringDotSeparated ui1 ui2) = "(UIStringDotSeparated " <> show ui1 <> show ui2 <> ")"
  show u = genericShow u
