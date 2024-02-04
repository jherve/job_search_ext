module LinkedIn.QueryRunner where

import Prelude

import Control.Monad.Except (ExceptT(..), except, mapExceptT, runExceptT)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array as A
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import LinkedIn.Queryable (class Queryable, getChildrenArray, queryAllNodes, queryOneNode, toNode)
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
instance Semigroup QueryError where
  append a _ = a

type Query q a = q → ExceptT QueryError Effect a

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

querySelf ∷ forall q. Queryable q => Query q Node
querySelf node = except $ Right $ toNode node

queryOne ∷ forall q. Queryable q => String → Query q Node
queryOne selector node = ExceptT $ do
  maybeNode <- queryOneNode selector node
  pure $ note (QNodeNotFoundError selector) maybeNode

queryText ∷ forall q. Queryable q => Int -> Query q Node
queryText idx n = ExceptT $ do
  childrenArr <- getChildrenArray n
  let
    maybeText n' = do
      _ <- T.fromNode n'
      pure $ n'
    allTexts = A.mapMaybe maybeText childrenArr

  pure $ note QTextNotFoundError $ A.index allTexts idx

queryAll ∷ forall q. Queryable q => String → Query q (NonEmptyList Node)
queryAll selector node = ExceptT $ do
  maybeNodes <- queryAllNodes selector node
  pure $ note (QNodeListNotFoundError selector) maybeNodes
