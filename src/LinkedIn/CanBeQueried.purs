module LinkedIn.CanBeQueried where

import Prelude

import Data.List (List)
import Data.List.Types (NonEmptyList)
import Data.Traversable (traverse)
import LinkedIn.QueryRunner (Query, queryNEL, queryList, queryOne)
import LinkedIn.Queryable (class Queryable)
import Web.DOM (Node)

class Queryable root <= CanBeQueried root t where
  query :: Query root (t Node)

subQueryOne ∷ ∀ q t. CanBeQueried Node t ⇒ Queryable q ⇒ String → Query q (t Node)
subQueryOne selector n = query =<< queryOne selector n

subQueryNEL ∷ ∀ q t. CanBeQueried Node t ⇒ Queryable q ⇒ String → Query q (NonEmptyList (t Node))
subQueryNEL selector n = traverse query =<< queryNEL selector n

subQueryList ∷ ∀ q t. CanBeQueried Node t ⇒ Queryable q ⇒ String → Query q (List (t Node))
subQueryList selector n = traverse query =<< queryList selector n
