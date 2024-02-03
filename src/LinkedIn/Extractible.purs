module LinkedIn.Extractible where

import Data.Either (Either)
import LinkedIn.Output.Types (Output)
import LinkedIn.UI.Elements.Types (UIElement)

class Extractible t where
  extract ∷ t UIElement → Either String Output
