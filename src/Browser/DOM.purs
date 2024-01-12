module Browser.DOM where

import Effect (Effect)
import Web.DOM.Document (Document)

foreign import getBrowserDom :: Effect Document

