module LinkedIn.DetachedNode where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as S
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (sequence)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Node)
import Web.DOM.Element (Element, getAttribute)
import Web.DOM.Element as E
import Web.DOM.Node (nodeType, textContent)
import Web.DOM.Node as N
import Web.DOM.NodeList as NL
import Web.DOM.NodeType (NodeType(..))
import Yoga.Tree (Tree, leaf, mkTree)


data DetachedNode =
  DetachedElement {tag :: String, content :: String, id :: Maybe String, classes :: List String}
  | DetachedSvgElement {tag :: String, id :: Maybe String, dataTestIcon :: Maybe String}
  | DetachedA {content :: String, href :: String}
  | DetachedButton {content :: String, role :: Maybe String, classes :: List String}
  | DetachedComment String
  | DetachedText String

derive instance Generic DetachedNode _
derive instance Eq DetachedNode
instance Show DetachedNode where
  show = genericShow

toDetached :: Node -> Effect DetachedNode
toDetached node = unsafePartial $ toDetached' (nodeType node) node where
  toDetached' :: Partial => NodeType -> Node -> Effect DetachedNode
  toDetached' CommentNode n = do
    txt <- textContent n
    pure $ DetachedComment $ normalize txt

  toDetached' TextNode n = do
    txt <- textContent n
    pure $ DetachedText $ normalize txt

  toDetached' ElementNode n = do
    txt <- textContent n
    let
      el = unsafePartial $ fromJust $ E.fromNode n
      tag = E.tagName el
    elementToDetached el tag txt

elementToDetached :: Element -> String -> String -> Effect DetachedNode
elementToDetached el tag text = case tag of
  "A" -> do
    href <- getAttribute "href" el

    pure $ DetachedA {
      content: normalize text,
      href: unsafePartial $ fromJust href
    }

  "BUTTON" -> do
    role <- getAttribute "role" el
    classes <- getClassList el

    pure $ DetachedButton {
      content: normalize text,
      role,
      classes
    }

  -- On SVG elements "className" returns a weird "SVGString" type that cannot be trimmed
  tag' | tag' == "svg" || tag' == "use" || tag' == "path" -> do
    id <- E.id el
    data_ <- getAttribute "data-test-icon" el

    pure $ DetachedSvgElement {
      tag: tag',
      id: if S.null id then Nothing else Just id,
      dataTestIcon: data_
    }

  tag' -> do
    id <- E.id el
    classes <- getClassList el

    pure $ DetachedElement {
      tag: tag',
      content: normalize text,
      id: if S.null id then Nothing else Just id,
      classes
    }

  where
    getClassList el' = do
      classStr <- E.className el'
      pure $ A.toUnfoldable $ S.split (Pattern " ") (normalize classStr)

normalize :: String -> String
normalize = normaliseSpace >>> S.trim

normaliseSpace :: String -> String
normaliseSpace s = fromCharArray $ A.fromFoldable $ normaliseSpace' $ A.toUnfoldable $ toCharArray s
  where
    badSequence ' ' ' ' = true
    badSequence ' ' '\n' = true
    badSequence '\n' '\n' = true
    badSequence '\n' ' ' = true
    badSequence _ _ = false

    normaliseSpace':: List Char -> List Char
    normaliseSpace' Nil = Nil
    normaliseSpace' (c1 : xs@(c2 : _)) | badSequence c1 c2 = normaliseSpace' xs
    normaliseSpace' (x:xs) = x : normaliseSpace' xs

cutBranches :: forall a. (Tree a -> Boolean) -> Tree a -> Tree a
cutBranches filterIn tree = case head tree, tail tree of
  h, [] -> mkTree h []

  h, t ->
    mkTree h (A.filter filterIn tail) where
      tail = map (cutBranches filterIn) t :: Array (Tree a)

filterEmpty ∷ Tree DetachedNode → Boolean
filterEmpty t = case head t of
  DetachedComment _ -> false
  DetachedText "" -> false
  DetachedElement {tag: "SPAN", classes}
    | L.any (_ == "white-space-pre") classes  -> false
  DetachedElement {classes} -> L.all (\c -> c /= "visually-hidden" && c /= "a11y-text") classes
  _ -> true

asTree' :: Node -> Effect (Tree DetachedNode)
asTree' n = do
  children <- N.childNodes n
  childArray <- NL.toArray children :: Effect (Array Node)
  detached <- toDetached n

  case childArray of
    [] -> pure $ leaf detached
    arr -> do
      a <- sequence (map asTree' arr) :: Effect (Array (Tree DetachedNode))
      pure $ mkTree detached a
