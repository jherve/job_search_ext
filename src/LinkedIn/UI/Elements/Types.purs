module LinkedIn.UI.Elements.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import LinkedIn.UI.Strings.Types (UIString)

data UIElement =
  UIElement UIString
  | UILink String UIString
  | UIButton {role :: Maybe String, label :: UIString, mainClass :: Maybe String}
  | UIIcon String

derive instance Generic UIElement _
instance Show UIElement where
  show = genericShow
