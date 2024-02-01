module LinkedIn.QueryRunner where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT, throwError)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array as A
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect (Effect)
import LinkedIn.Queryable (class Queryable, getChildrenArray, queryAllNodes, queryOneNode)
import Web.DOM (Node)
import Web.DOM.Text as T

data QueryError =
  QNodeNotFoundError String
  | QNodeListNotFoundError String
  | QNodeUnexpectedType String String
  | QTextNotFoundError
  | QChooseError
instance EncodeJson QueryError where
  encodeJson a = genericEncodeJson a

derive instance Generic QueryError _
derive instance Eq QueryError
instance Show QueryError where
  show = genericShow

-- QueryRunner' is a generalization of QueryRunner for all Queryable instances (e.g. Document)
type QueryRunner' q a = q → ExceptT QueryError Effect a

type QueryRunner a = QueryRunner' Node a

runQuery ∷ ∀ a. ExceptT QueryError Effect a → Effect (Either QueryError a)
runQuery = runExceptT

ignoreNotFound ∷ ∀ a f. Functor f ⇒ ExceptT QueryError f a → ExceptT QueryError f (Maybe a)
ignoreNotFound = mapExceptT (map ignoreNotFound')
  where
    ignoreNotFound' = case _ of
      (Left (QNodeNotFoundError _ )) -> Right Nothing
      (Left (QNodeListNotFoundError _ )) -> Right Nothing
      (Left q) -> Left q
      (Right n') -> Right (Just n')

ignoreErrors ∷ ∀ a f. Functor f ⇒ ExceptT QueryError f a → ExceptT QueryError f (Maybe a)
ignoreErrors = mapExceptT (map ignoreErrors')
  where
    ignoreErrors' = case _ of
      (Left _) -> Right Nothing
      (Right n') -> Right (Just n')

queryOne ∷ forall q. Queryable q => String → QueryRunner' q Node
queryOne selector node = ExceptT $ do
  maybeNode <- queryOneNode selector node
  pure $ note (QNodeNotFoundError selector) maybeNode

queryText ∷ forall q. Queryable q => Int -> QueryRunner' q Node
queryText idx n = ExceptT $ do
  childrenArr <- getChildrenArray n
  let
    maybeText n' = do
      _ <- T.fromNode n'
      pure $ n'
    allTexts = A.mapMaybe maybeText childrenArr

  pure $ note QTextNotFoundError $ A.index allTexts idx

queryAll ∷ forall q. Queryable q => String → QueryRunner' q (NonEmptyList Node)
queryAll selector node = ExceptT $ do
  maybeNodes <- queryAllNodes selector node
  pure $ note (QNodeListNotFoundError selector) maybeNodes

subQueryMany ∷ ∀ a q. Queryable q ⇒ QueryRunner a → String → QueryRunner' q (NonEmptyList a)
subQueryMany query selector n = traverse query =<< queryAll selector n

subQueryOne ∷ ∀ a q. Queryable q ⇒ QueryRunner a → String → QueryRunner' q a
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
