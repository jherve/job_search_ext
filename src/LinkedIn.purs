module LinkedIn (encodeToJson, getContext, extractFromDocument, extractFromDocumentInContext) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import LinkedIn.Output (toOutput)
import LinkedIn.Output.Types (Output)
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

extractFromDocument :: Document -> Effect (Either String Output)
extractFromDocument dom = do
  ctx <- getContext dom

  case ctx of
    Left l -> pure $ Left l
    Right ctx' -> toOutput ctx' dom

extractFromDocumentInContext :: PageUrl -> Document -> Effect (Either String Output)
extractFromDocumentInContext = toOutput

encodeToJson :: Either String Output -> Json
encodeToJson = encodeJson
