module ExampleWebExt.Content where

import Data.List.NonEmpty
import LinkedIn
import Prelude

import Browser.DOM (getBrowserDom)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Yoga.Tree (showTree)

main :: Effect Unit
main = do
  dom <- getBrowserDom
  artDecoCards <- getArtDecoCards dom
  artDecoTabs <- getArtDecoTabs dom

  log "[content] starting up"
  logShow artDecoCards
  logShow artDecoTabs

  case artDecoCards of 
    Nothing -> log "no card found"
    Just cards -> do
      tree <- asTree $ head cards
      log $ showTree tree
