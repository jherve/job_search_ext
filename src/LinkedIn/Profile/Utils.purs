module LinkedIn.Profile.Utils where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..), hush)
import Data.Foldable (class Foldable, findMap)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..))
import LinkedIn.DetachedNode (DetachedNode(..))
import LinkedIn.UIElements.Parser (uiElementP)
import LinkedIn.UIElements.Types (UIElement(..))
import Parsing (ParseError(..), initialPos, runParser)

maybeGetInList ::
  ∀ a. (UIElement → Maybe a)
  -> List (Either ParseError UIElement)
  -> Int
  -> Maybe a
maybeGetInList extract idx list = L.index idx list >>= hush >>= extract

maybeExtractFromMaybe ∷
  ∀ a. (UIElement → Maybe a)
  → Maybe (Either ParseError UIElement)
  → Maybe a
maybeExtractFromMaybe extract maybeNode = maybeNode >>= hush >>= extract

maybeFindInMaybeNEL ∷
  ∀ a f. Foldable f ⇒
  (UIElement → Maybe a)
  → Maybe (f (Either ParseError UIElement))
  → Maybe a
maybeFindInMaybeNEL extract = case _ of
  Just nel -> findMap (hush >>> (extract =<< _)) nel
  Nothing -> Nothing

-- TODO : should certainly use another type than ParseError here
toUIElement ∷ DetachedNode → Either ParseError UIElement
toUIElement (DetachedElement {content}) = runParser content uiElementP
toUIElement (DetachedComment str) = runParser str uiElementP
toUIElement (DetachedText str) = runParser str uiElementP

toUIElement (DetachedSvgElement {id, dataTestIcon, tag: "svg"}) = case id <|> dataTestIcon of
  Just i -> Right (UIIcon i)
  Nothing -> Left (ParseError "SVG element could not be identified" initialPos)
toUIElement (DetachedSvgElement _) = Left (ParseError "SVG element could not be identified" initialPos)

toUIElement (DetachedLiIcon i) = Right (UIIcon i)

toUIElement (DetachedButton {content, role}) =  map toButton $ runParser content uiElementP
  where toButton ui = UIButton role ui

toUIElement (DetachedA {content, href}) = map toLink $ runParser content uiElementP
  where toLink ui = UILink href ui
