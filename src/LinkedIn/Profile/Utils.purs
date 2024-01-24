module LinkedIn.Profile.Utils where

import Prelude

import Data.Either (Either, hush)
import Data.Foldable (class Foldable, findMap)
import Data.List (List)
import Data.List as L
import Data.Maybe (Maybe(..))
import LinkedIn.DetachedNode (DetachedNode(..))
import LinkedIn.UIElements.Parser (uiElementP)
import LinkedIn.UIElements.Types (UIElement(..))
import Parsing (runParser, ParseError)

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

toUIElement ∷ DetachedNode → Either ParseError UIElement
toUIElement (DetachedElement {content}) = runParser content uiElementP
toUIElement (DetachedComment str) = runParser str uiElementP
toUIElement (DetachedText str) = runParser str uiElementP
toUIElement (DetachedButton {content, role}) =  map toButton $ runParser content uiElementP
  where toButton ui = UIButton role ui
toUIElement (DetachedA {content, href}) = map toLink $ runParser content uiElementP
  where toLink ui = UILink href ui
