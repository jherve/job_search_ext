module Node.JsDom where

import Prelude

import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Web.DOM.Document (Document)

foreign import jsDomParse :: String -> Effect Document

jsDomFromFile ∷ String -> Effect Document
jsDomFromFile path = do
  content <- readTextFile UTF8 path
  jsDomParse content
