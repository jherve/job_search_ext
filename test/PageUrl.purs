module Test.PageUrl
  ( main
  )
  where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Int64 (Int64)
import Data.Int64 as I64
import Data.Maybe (fromJust)
import Effect (Effect)
import LinkedIn.PageUrl (PageUrl(..), pageUrlP)
import LinkedIn.UI.Basic.Types (JobOfferId(..))
import Parsing (runParser)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert, assertEqual)

toI64 ∷ String → Int64
toI64 s = unsafePartial $ fromJust $ I64.fromString s

main :: Effect Unit
main = do
  assertEqual {
    actual: runParser "/in/username/details/projects/" pageUrlP,
    expected: Right(UrlProjects "username")
  }

  assertEqual {
    actual: runParser "/in/username/details/skills/" pageUrlP,
    expected: Right(UrlSkills "username")
  }

  assertEqual {
    actual: runParser "/in/username/details/experience/" pageUrlP,
    expected: Right(UrlWorkExperience "username")
  }

  assertEqual {
    actual: runParser "/in/username/details/languages/" pageUrlP,
    expected: Right(UrlLanguage "username")
  }

  assertEqual {
    actual: runParser "/in/username/details/education/" pageUrlP,
    expected: Right(UrlEducation "username")
  }

  assertEqual {
    actual: runParser "/jobs/view/3764313323/" pageUrlP,
    expected: Right(UrlJobOffer (JobOfferId (toI64 "3764313323")))
  }

  assertEqual {
    actual: runParser "/in/username/" pageUrlP,
    expected: Right(UrlProfileMain "username")
  }

  assert $ isLeft $ runParser "/not/an/url/" pageUrlP
