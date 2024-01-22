module LinkedIn.Types where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Web.DOM (Node)


data ParseError =
  NodeNotFoundError String
  | NodeListNotFoundError String
  | TextNotFoundError

derive instance Generic ParseError _
derive instance Eq ParseError
instance Show ParseError where
  show = genericShow

type Parser a = Node → Effect (Either ParseError a)
