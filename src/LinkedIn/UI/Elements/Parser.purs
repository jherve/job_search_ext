module LinkedIn.UI.Elements.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse)
import LinkedIn.DetachedNode (DetachedNode(..))
import LinkedIn.UI.Strings.Parser (uiStringP)
import LinkedIn.UI.Elements.Types (UIElement(..))
import Parsing (ParseError(..), initialPos, runParser)

fromDetachedToUI ∷ ∀ t. Traversable t ⇒ t DetachedNode → Either String (t UIElement)
fromDetachedToUI el = case traverse toUIElement el of
  Left _ -> Left "error on conversion to UI element"
  Right ui -> Right ui

-- TODO : should certainly use another type than ParseError here
toUIElement ∷ DetachedNode → Either ParseError UIElement
toUIElement (DetachedElement {content}) = wrapInUiElement content
toUIElement (DetachedComment str) = wrapInUiElement str
toUIElement (DetachedText str) = wrapInUiElement str

toUIElement (DetachedSvgElement {id, dataTestIcon, tag: "svg"}) = case id <|> dataTestIcon of
  Just i -> Right (UIIcon i)
  Nothing -> Left (ParseError "SVG element could not be identified" initialPos)
toUIElement (DetachedSvgElement _) = Left (ParseError "SVG element could not be identified" initialPos)

toUIElement (DetachedLiIcon i) = Right (UIIcon i)

toUIElement (DetachedButton {content, role, classes}) =  map toButton $ runParser content uiStringP
  where toButton ui = UIButton {role, label: ui, mainClass: L.head classes}

toUIElement (DetachedA {content, href}) = map toLink $ runParser content uiStringP
  where toLink ui = UILink href ui

wrapInUiElement ∷ String → Either ParseError UIElement
wrapInUiElement string = map UIElement $ runParser string uiStringP
