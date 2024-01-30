module LinkedIn (module LinkedIn.Output, getContext) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import LinkedIn.Output (run, runToDetached, toOutput)
import LinkedIn.PageUrl (PageUrl, pageUrlP)
import Parsing (runParser)
import Web.DOM (Document)
import Web.DOM.Document (url)
import Web.URL as U

getContext ∷ Document → Effect (Either String PageUrl)
getContext dom = do
  u <- url dom
  pure $ case U.fromAbsolute u of
    Nothing -> Left "No URL found"
    Just u' -> case runParser (U.pathname u') pageUrlP of
      Left _ -> Left "Unexpected URL"
      Right page -> Right page
