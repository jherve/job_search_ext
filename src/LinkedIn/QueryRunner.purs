module LinkedIn.QueryRunner where

import Prelude

import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect (Effect)
import LinkedIn.Utils as U
import Web.DOM (Node)

data QueryError =
  QNodeNotFoundError String
  | QNodeListNotFoundError String
  | QTextNotFoundError

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

queryOne ∷ String → QueryRunner Node
queryOne selector node = ExceptT $ do
  maybeNode <- U.queryOne selector node
  pure $ note (QNodeNotFoundError selector) maybeNode

queryAll ∷ String → QueryRunner (NonEmptyList Node)
queryAll selector node = ExceptT $ do
  maybeNodes <- U.queryAll selector node
  pure $ note (QNodeListNotFoundError selector) maybeNodes

subQueryMany ∷ ∀ a. QueryRunner a → String → QueryRunner (NonEmptyList a)
subQueryMany query selector n = traverse query =<< queryAll selector n

subQueryOne ∷ ∀ a. QueryRunner a → String → QueryRunner a
subQueryOne query selector n = query =<< queryOne selector n
