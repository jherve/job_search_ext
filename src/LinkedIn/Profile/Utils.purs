module LinkedIn.Profile.Utils where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import LinkedIn.DetachedNode (DetachedNode(..))
import LinkedIn.UIElements.Parser (uiStringP)
import LinkedIn.UIElements.Types (UIElement(..))
import Parsing (ParseError(..), initialPos, runParser)

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
