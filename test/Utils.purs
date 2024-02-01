module Test.Utils where

import Prelude

import Data.Date (Month)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.Traversable (class Traversable, traverse)
import LinkedIn.DetachedNode (DetachedNode)
import LinkedIn.UI.Basic.Parser (toYear)
import LinkedIn.UI.Basic.Types (MonthYear(..))
import LinkedIn.UI.Elements.Parser (toUIElement)
import LinkedIn.UI.Elements.Types (UIElement)
import Partial.Unsafe (unsafePartial)

toMonthYear' :: Month -> Int -> MonthYear
toMonthYear' m y = MonthYear m y' where
  y' = unsafePartial $ fromJust $ toYear $ toNumber y

fromDetachedToUI ∷ ∀ t. Traversable t ⇒ t DetachedNode → Either String (t UIElement)
fromDetachedToUI el = case traverse toUIElement el of
  Left _ -> Left "error on conversion to UI element"
  Right ui -> Right ui
