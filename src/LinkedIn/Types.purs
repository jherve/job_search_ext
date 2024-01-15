module LinkedIn.Types where

import Prelude
import Data.Either (Either)
import Effect (Effect)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Web.DOM (Node)


data ParseError =
  NodeNotFoundError String
  | NodeListNotFoundError String

derive instance Generic ParseError _
derive instance Eq ParseError
instance Show ParseError where
  show = genericShow

type Parser a = Node → Effect (Either ParseError a)
