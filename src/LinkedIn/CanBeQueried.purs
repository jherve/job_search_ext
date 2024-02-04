module LinkedIn.CanBeQueried where

import Prelude

import LinkedIn.QueryRunner (QueryRunner', queryOne)
import LinkedIn.Queryable (class Queryable)
import Web.DOM (Node)

class Queryable root <= CanBeQueried root t where
  query :: QueryRunner' root (t Node)

subQueryOne ∷ ∀ q t. CanBeQueried Node t ⇒ Queryable q ⇒ String → QueryRunner' q (t Node)
subQueryOne selector n = query =<< queryOne selector n
