module LinkedIn.CanBeQueried where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Traversable (traverse)
import LinkedIn.QueryRunner (Query, queryAll, queryOne)
import LinkedIn.Queryable (class Queryable)
import Web.DOM (Node)

class Queryable root <= CanBeQueried root t where
  query :: Query root (t Node)

subQueryOne ∷ ∀ q t. CanBeQueried Node t ⇒ Queryable q ⇒ String → Query q (t Node)
subQueryOne selector n = query =<< queryOne selector n

subQueryMany ∷ ∀ q t. CanBeQueried Node t ⇒ Queryable q ⇒ String → Query q (NonEmptyList (t Node))
subQueryMany selector n = traverse query =<< queryAll selector n
