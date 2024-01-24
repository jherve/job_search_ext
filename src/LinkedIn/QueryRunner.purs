module LinkedIn.QueryRunner where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT, throwError)
import Data.Array as A
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect (Effect)
import LinkedIn.Utils as U
import Web.DOM (Node)
import Web.DOM.Node as N
import Web.DOM.NodeList as NL
import Web.DOM.Text as T

data QueryError =
  QNodeNotFoundError String
  | QNodeListNotFoundError String
  | QNodeUnexpectedType String String
  | QTextNotFoundError
  | QChooseError

derive instance Generic QueryError _
derive instance Eq QueryError
instance Show QueryError where
  show = genericShow

type QueryRunner a = Node → ExceptT QueryError Effect a

runQuery ∷ ∀ a. ExceptT QueryError Effect a → Effect (Either QueryError a)
runQuery = runExceptT

ignoreNotFound ∷ ∀ a f. Functor f ⇒ ExceptT QueryError f a → ExceptT QueryError f (Maybe a)
ignoreNotFound = mapExceptT (map ignoreNotFound')
  where
    ignoreNotFound' = case _ of
      (Left (QNodeNotFoundError _ )) -> Right Nothing
      (Left q) -> Left q
      (Right n') -> Right (Just n')

ignoreErrors ∷ ∀ a f. Functor f ⇒ ExceptT QueryError f a → ExceptT QueryError f (Maybe a)
ignoreErrors = mapExceptT (map ignoreErrors')
  where
    ignoreErrors' = case _ of
      (Left q) -> Right Nothing
      (Right n') -> Right (Just n')

queryOne ∷ String → QueryRunner Node
queryOne selector node = ExceptT $ do
  maybeNode <- U.queryOne selector node
  pure $ note (QNodeNotFoundError selector) maybeNode

queryText ∷ Int -> QueryRunner Node
queryText idx n = ExceptT $ do
  children <- N.childNodes n
  childrenArr <- NL.toArray children
  let
    maybeText n' = do
      _ <- T.fromNode n'
      pure $ n'
    allTexts = A.mapMaybe maybeText childrenArr

  pure $ note QTextNotFoundError $ A.index allTexts idx

queryAll ∷ String → QueryRunner (NonEmptyList Node)
queryAll selector node = ExceptT $ do
  maybeNodes <- U.queryAll selector node
  pure $ note (QNodeListNotFoundError selector) maybeNodes

subQueryMany ∷ ∀ a. QueryRunner a → String → QueryRunner (NonEmptyList a)
subQueryMany query selector n = traverse query =<< queryAll selector n

subQueryOne ∷ ∀ a. QueryRunner a → String → QueryRunner a
subQueryOne query selector n = query =<< queryOne selector n

chooseOne ∷ ∀ a t m. Monad m ⇒ (t → ExceptT QueryError m a) → (t → ExceptT QueryError m a) → (t → ExceptT QueryError m a)
chooseOne q1 q2 n = do
  maybeN1 <- (ignoreErrors <<< q1) n
  maybeN2 <- (ignoreErrors <<< q2) n

  case maybeN1 <|> maybeN2 of
    Nothing -> throwError QChooseError
    Just n' -> pure n'

chooseOne3 ∷ ∀ a t m.
  Monad m
  ⇒ (t → ExceptT QueryError m a)
  → (t → ExceptT QueryError m a)
  → (t → ExceptT QueryError m a)
  → (t → ExceptT QueryError m a)
chooseOne3 q1 q2 q3 n = do
  maybeN1 <- (ignoreErrors <<< q1) n
  maybeN2 <- (ignoreErrors <<< q2) n
  maybeN3 <- (ignoreErrors <<< q3) n

  case maybeN1 <|> maybeN2 <|> maybeN3 of
    Nothing -> throwError QChooseError
    Just n' -> pure n'
