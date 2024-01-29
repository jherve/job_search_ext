module Test.ArtDecoCard where

import Prelude

import Data.Date (Month(..))
import Data.Either (Either(..), hush, isRight)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty(..))
import Data.Traversable (traverse)
import Effect (Effect)
import LinkedIn.UI.Components.ArtDeco (ArtDecoCenter(..), ArtDecoCenterContent(..), ArtDecoCenterHeader(..), ArtDecoPvsEntity(..), ArtDecoPvsEntitySubComponent(..))
import LinkedIn.UI.Components.ArtDecoCard (ArtDecoCardElement(..))
import LinkedIn.DetachedNode (DetachedNode(..), toDetached)
import LinkedIn.Page.WorkExperiences (WorkExperiencesPage(..))
import LinkedIn.Page.WorkExperiences as PageWE
import LinkedIn.Profile.WorkExperience (WorkExperience(..))
import LinkedIn.Profile.WorkExperience as PWE
import LinkedIn.QueryRunner (runQuery)
import LinkedIn.UI.Basic.Types (Duration(..), TimeSpan(..))
import LinkedIn.UI.Elements.Parser (fromDetachedToUI)
import Node.JsDom (jsDomFromFile)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert, assertEqual)
import Test.Utils (toMonthYear')

testArtDecoCards :: Effect Unit
testArtDecoCards = do
  dom <- jsDomFromFile "test/examples/andrew_ng_experiences.html"
  wep <- runQuery $ PageWE.query dom

  assert $ isRight wep

  let
    WorkExperiencesPage cards = unsafePartial $ fromJust $ hush wep
    head = NEL.head cards

  headCard <- traverse toDetached head

  assertEqual {
    actual: headCard,
    expected:
      ArtDecoCardElement {
        pvs_entity: (ArtDecoPvsEntity {
          center: (ArtDecoCenter {
            content: (ArtDecoCenterContent
              (NonEmptyList (NonEmpty
                (ArtDecoPvsEntitySubComponent (Just (
                  DetachedElement {
                    classes: Nil,
                    content: "DeepLearning.AI provides\ntechnical training on Generative AI, Machine Learning, Deep Learning,\nand other topics. We also offer a widely read newsletter, The Batch\n(thebatch.ai), that covers what matters in AI right now. Our courses are often created with industry-leading AI companies (AWS,\nGoogle, OpenAI, etc.), and we offer both short courses that can be\ncompleted in an hour, and longer courses and specializations hosted on\nCoursera that give you a solid foundation in some aspect of AI. These\ncourses are designed to offer hands-on practice with AI technologies,\nand you will gain practical, job-ready skills. Whether you are just starting out in AI or seeking to further an existing\ncareer, come see if we can help, at http://deeplearning.ai!",
                    id: Nothing,
                    tag: "SPAN"
                    }))) Nil))),
            header: (ArtDecoCenterHeader {
              bold: (DetachedElement {
                classes: Nil,
                content: "Founder",
                id: Nothing,
                tag: "SPAN" }
              ), light: (Just (NonEmptyList (
                NonEmpty (
                  DetachedElement {
                    classes: ("pvs-entity__caption-wrapper" : Nil),
                    content: "juin 2017 - aujourd’hui · 6 ans 7 mois",
                    id: Nothing,
                    tag: "SPAN"
                  }) ((DetachedElement {
                    classes: Nil,
                    content: "Palo Alto, California, United States",
                    id: Nothing,
                    tag: "SPAN"
                  }) : Nil)))),
              normal: (Just (DetachedElement {
                classes: Nil,
                content: "DeepLearning.AI",
                id: Nothing,
                tag: "SPAN" }))
              }) }),
          side: unit
        })
      }
  }

  assertEqual {
    actual: (PWE.fromUI <=< fromDetachedToUI) headCard,
    expected:
      Right (WorkExperience {
        company: Just "DeepLearning.AI",
        contractType: Nothing,
        description: Just "DeepLearning.AI provides\ntechnical training on Generative AI, Machine Learning, Deep Learning,\nand other topics. We also offer a widely read newsletter, The Batch\n(thebatch.ai), that covers what matters in AI right now. Our courses are often created with industry-leading AI companies (AWS,\nGoogle, OpenAI, etc.), and we offer both short courses that can be\ncompleted in an hour, and longer courses and specializations hosted on\nCoursera that give you a solid foundation in some aspect of AI. These\ncourses are designed to offer hands-on practice with AI technologies,\nand you will gain practical, job-ready skills. Whether you are just starting out in AI or seeking to further an existing\ncareer, come see if we can help, at http://deeplearning.ai!",
        duration: Just (YearsMonth 6 7),
        position: "Founder",
        timeSpan: Just (TimeSpanToToday (toMonthYear' June 2017))
      })
  }

main :: Effect Unit
main = do
  testArtDecoCards
