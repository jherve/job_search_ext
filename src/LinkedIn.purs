module LinkedIn where

import Prelude
import Yoga.Tree

import Control.Comonad.Cofree (head, tail)
import Data.Array as A
import Data.List (List(..), (:))
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (Pattern(..), joinWith)
import Data.String as S
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Document, Element, Node)
import Web.DOM.Document (origin)
import Web.DOM.Document as D
import Web.DOM.Element as E
import Web.DOM.Node (nodeName, nodeType, textContent)
import Web.DOM.Node as N
import Web.DOM.NodeList as NL
import Web.DOM.NodeType (NodeType(..))
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Yoga.Tree.Zipper as Z

-- A light abstraction layer above the DOM manipulation API

fromDocument ∷ Document → Node
fromDocument doc = D.toNode doc

queryOne :: String -> Document -> Effect (Maybe Node)
queryOne selector doc = do
  found <- querySelector (QuerySelector selector) $ D.toParentNode doc
  pure case found of
    Nothing -> Nothing
    Just el -> Just $ E.toNode el

queryAll :: String -> Document -> Effect (Maybe (NonEmptyList Node))
queryAll selector doc = do
  found <- querySelectorAll (QuerySelector selector) $ D.toParentNode doc
  liftA1 NEL.fromFoldable $ NL.toArray found

-- First pass of naming ; from here we know what we are looking for

data LinkedInUIElementType = LinkedInUIArtDecoCard | LinkedInUIArtDecoTab | LinkedInUIJobsUnifiedTopCard

instance Show LinkedInUIElementType where
  show LinkedInUIArtDecoCard = "ArtDecoCard"
  show LinkedInUIArtDecoTab = "ArtDecoTab"
  show LinkedInUIJobsUnifiedTopCard = "JobsUnifiedTopCard"

data LinkedInUIElement = LinkedInUIElement LinkedInUIElementType Node

instance Show LinkedInUIElement where
  show (LinkedInUIElement typ n) = "LinkedInUIElement(" <> show typ <> ", " <> nodeName n <> ")"

getArtDecoCards ∷ Document → Effect (Maybe (NonEmptyList LinkedInUIElement))
getArtDecoCards = queryAll' LinkedInUIArtDecoCard "section.artdeco-card > div ~ div > div > div > ul > li"

getArtDecoTabs ∷ Document → Effect (Maybe (NonEmptyList LinkedInUIElement))
getArtDecoTabs = queryAll' LinkedInUIArtDecoTab "div.artdeco-tabs > div > div > div > div > ul > li"

getJobsUnifiedTopCard ∷ Document → Effect (Maybe (NonEmptyList LinkedInUIElement))
getJobsUnifiedTopCard = queryAll' LinkedInUIJobsUnifiedTopCard "div.jobs-unified-top-card"

queryAll' ∷ LinkedInUIElementType → String → Document → Effect (Maybe (NonEmptyList LinkedInUIElement))
queryAll' constructor selector doc = do
  nodes <- queryAll selector doc
  pure case nodes of
    Nothing -> Nothing
    Just cards -> Just $ map (LinkedInUIElement constructor) cards

data DetachedNode =
  DetachedElement {tag :: String, content :: String, id :: Maybe String, classes :: List String}
  | DetachedComment String
  | DetachedText String

instance Show DetachedNode where
  show (DetachedElement n) = "DetachedElement(" <> n.tag <> id' <> classes' <> ")"
    where
      id' = case n.id of
        Nothing -> ""
        Just i -> "#" <> i
      classes' = joinWith " " $ A.fromFoldable (map (\c -> "." <> c) n.classes)
  show (DetachedComment c) = "DetachedComment(" <> c <> ")"
  show (DetachedText t) = "DetachedText(" <> t <> ")"

asTree :: LinkedInUIElement -> Effect (Tree DetachedNode)
asTree (LinkedInUIElement _ n) = asTree' n

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

toDetached :: Node -> Effect DetachedNode
toDetached node = unsafePartial $ toDetached' (nodeType node) node where
  toDetached' :: Partial => NodeType -> Node -> Effect DetachedNode
  toDetached' ElementNode n = do
    txt <- textContent n
    let
      el = unsafePartial $ fromJust $ E.fromNode n
      tag = E.tagName el
    id <- E.id el
    -- On SVG elements "className" returns a weird "SVGString" type that cannot be trimmed
    classStr <- if tag /= "svg" && tag /= "use" && tag /= "path" then E.className el else pure $ ""

    pure $ DetachedElement {
      tag: E.tagName el,
      content: normalize txt,
      id: if S.null id then Nothing else Just id,
      classes: A.toUnfoldable $ S.split (Pattern " ") (normalize classStr)
    }

  toDetached' CommentNode n = do
    txt <- textContent n
    pure $ DetachedComment $ normalize txt
  toDetached' TextNode n = do
    txt <- textContent n
    pure $ DetachedText $ normalize txt

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

asPrunedTrees :: Maybe (NonEmptyList LinkedInUIElement) → Effect (Maybe (NonEmptyList (Tree DetachedNode)))
asPrunedTrees =
  case _ of
    Nothing -> pure Nothing
    Just els -> do
      trees <- traverse asPrunedTree els
      pure $ Just $ trees

asPrunedTree :: LinkedInUIElement → Effect (Tree DetachedNode)
asPrunedTree el =  do
  tree <- asTree el
  pure $ cutBranches filterEmpty tree
