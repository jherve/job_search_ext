module LinkedIn.CanBeQueried where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Traversable (traverse)
import LinkedIn.QueryRunner (QueryRunner', queryAll, queryOne)
import LinkedIn.Queryable (class Queryable)
import Web.DOM (Node)

class Queryable root <= CanBeQueried root t where
  query :: QueryRunner' root (t Node)

subQueryOne ∷ ∀ q t. CanBeQueried Node t ⇒ Queryable q ⇒ String → QueryRunner' q (t Node)
subQueryOne selector n = query =<< queryOne selector n

subQueryMany ∷ ∀ q t. CanBeQueried Node t ⇒ Queryable q ⇒ String → QueryRunner' q (NonEmptyList (t Node))
subQueryMany selector n = traverse query =<< queryAll selector n
