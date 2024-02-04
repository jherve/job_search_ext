module Test.Utils where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Date (Month)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.Traversable (class Traversable, traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import LinkedIn.CanBeQueried (class CanBeQueried)
import LinkedIn.DetachedNode (DetachedNode)
import LinkedIn.Extractible (class Extractible)
import LinkedIn.Output (Output, OutputError, run, runToDetached)
import LinkedIn.QueryRunner (QueryError)
import LinkedIn.UI.Basic.Parser (toYear)
import LinkedIn.UI.Basic.Types (MonthYear(..))
import LinkedIn.UI.Elements.Parser (toUIElement)
import LinkedIn.UI.Elements.Types (UIElement)
import Node.JsDom (jsDomFromFile, jsDomParse)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy)
import Web.DOM (Document)

toMonthYear' :: Month -> Int -> MonthYear
toMonthYear' m y = MonthYear m y' where
  y' = unsafePartial $ fromJust $ toYear $ toNumber y

fromDetachedToUI ∷ ∀ t. Traversable t ⇒ t DetachedNode → Either String (t UIElement)
fromDetachedToUI el = case traverse toUIElement el of
  Left _ -> Left "error on conversion to UI element"
  Right ui -> Right ui

detachFromFile :: forall t.
  Traversable t
  => CanBeQueried Document t
  => Proxy t
  -> String
  -> Aff (Either QueryError (t DetachedNode))
detachFromFile proxy filePath = do
  dom <- liftEffect $ jsDomFromFile filePath
  liftEffect $ runExceptT $ runToDetached proxy dom

detachFromString :: forall t.
  Traversable t
  => CanBeQueried Document t
  => Proxy t
  -> String
  -> Aff (Either QueryError (t DetachedNode))
detachFromString proxy string = do
  dom <- liftEffect $ jsDomParse string
  liftEffect $ runExceptT $ runToDetached proxy dom

getOutputFromFile :: forall t.
  Traversable t
  => Extractible t
  => CanBeQueried Document t
  => Proxy t
  -> String
  -> Aff (Either OutputError Output)
getOutputFromFile proxy string = do
  dom <- liftEffect $ jsDomFromFile string
  liftEffect $ runExceptT $ run proxy dom
