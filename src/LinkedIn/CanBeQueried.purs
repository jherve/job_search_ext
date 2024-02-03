module LinkedIn.CanBeQueried where

import LinkedIn.QueryRunner (QueryRunner')
import LinkedIn.Queryable (class Queryable)
import Web.DOM (Node)

class Queryable root <= CanBeQueried root t where
  query' :: QueryRunner' root (t Node)
