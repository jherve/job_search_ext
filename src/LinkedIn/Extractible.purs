module LinkedIn.Extractible where

import Data.Either (Either)
import LinkedIn.Output.Types (Output)
import LinkedIn.QueryRunner (QueryRunner')
import LinkedIn.UI.Elements.Types (UIElement)
import Web.DOM (Document, Node)

class Extractible t where
  query :: QueryRunner' Document (t Node)
  extract ∷ t UIElement → Either String Output
