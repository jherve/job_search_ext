module LinkedIn.Profile.Utils where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse)
import Effect (Effect)
import LinkedIn.DetachedNode (DetachedNode(..), toDetached)
import LinkedIn.UIElements.Parser (uiStringP)
import LinkedIn.UIElements.Types (UIElement(..))
import Parsing (ParseError(..), initialPos, runParser)
import Web.DOM (Node)

fromNodeToDetached ∷ ∀ t. Traversable t ⇒ t Node → Effect (t DetachedNode)
fromNodeToDetached = traverse toDetached

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

toUIElement (DetachedButton {content, role}) =  map toButton $ runParser content uiStringP
  where toButton ui = UIButton role ui

toUIElement (DetachedA {content, href}) = map toLink $ runParser content uiStringP
  where toLink ui = UILink href ui

wrapInUiElement ∷ String → Either ParseError UIElement
wrapInUiElement string = map UIElement $ runParser string uiStringP
