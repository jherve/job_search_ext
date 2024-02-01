module LinkedIn (encodeToJson, getContext, extractFromDocument, extractFromDocumentInContext) where

import Prelude

import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
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
getContext = runExceptT <<< getContext'

getContext' ∷ Document → ExceptT String Effect PageUrl
getContext' dom = do
  u <- lift $ url dom
  case U.fromAbsolute u of
    Nothing -> throwError "No URL found"
    Just u' ->  case runParser (U.pathname u') pageUrlP of
      Left _ -> throwError "Unexpected URL"
      Right page -> pure page

extractFromDocument :: Document -> Effect (Either String Output)
extractFromDocument = runExceptT <<< extractFromDocument'

extractFromDocument' ∷ Document → ExceptT String Effect Output
extractFromDocument' dom = do
  ctx <- getContext' dom
  toOutput ctx dom

extractFromDocumentInContext :: PageUrl -> Document -> Effect (Either String Output)
extractFromDocumentInContext url dom = runExceptT $ toOutput url dom

encodeToJson :: Either String Output -> Json
encodeToJson = encodeJson
