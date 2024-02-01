module LinkedIn (APIError(..), encodeToJson, getContext, extractFromDocument, extractFromDocumentInContext) where

import Prelude

import Control.Monad.Except (ExceptT, lift, runExceptT, throwError, withExceptT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import LinkedIn.Output (OutputError, toOutput)
import LinkedIn.Output.Types (Output)
import LinkedIn.PageUrl (PageUrl, pageUrlP)
import Parsing (runParser)
import Web.DOM (Document)
import Web.DOM.Document (url)
import Web.URL as U

data APIError =
  ErrorExtraction OutputError
  | ErrorInvalidUrl
  | ErrorUnexpectedUrl

derive instance Generic APIError _
instance Show APIError where
  show = genericShow
instance EncodeJson APIError where
  encodeJson a = genericEncodeJson a

getContext ∷ Document → Effect (Either APIError PageUrl)
getContext = runExceptT <<< getContext'

getContext' ∷ Document → ExceptT APIError Effect PageUrl
getContext' dom = do
  u <- lift $ url dom
  case U.fromAbsolute u of
    Nothing -> throwError ErrorInvalidUrl
    Just u' ->  case runParser (U.pathname u') pageUrlP of
      Left _ -> throwError ErrorUnexpectedUrl
      Right page -> pure page

extractFromDocument :: Document -> Effect (Either APIError Output)
extractFromDocument = runExceptT <<< extractFromDocument'

extractFromDocument' ∷ Document → ExceptT APIError Effect Output
extractFromDocument' dom = do
  ctx <- getContext' dom
  toOutput' ctx dom

extractFromDocumentInContext :: PageUrl -> Document -> Effect (Either APIError Output)
extractFromDocumentInContext url dom = runExceptT $ toOutput' url dom

toOutput' ∷ PageUrl → Document → ExceptT APIError Effect Output
toOutput' ctx dom = withExceptT (\err -> ErrorExtraction err) $ toOutput ctx dom

encodeToJson :: Either String Output -> Json
encodeToJson = encodeJson
