module LinkedIn.Extractible where

import Data.Either (Either)
import LinkedIn.QueryRunner (QueryRunner')
import LinkedIn.UI.Elements.Types (UIElement)
import Web.DOM (Document, Node)

class Extractible t out | t -> out where
  query :: QueryRunner' Document (t Node)
  extract ∷ t UIElement → Either String out
