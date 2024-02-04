module Test.ArtDecoCard where

import Prelude

import Data.Date (Month(..))
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import LinkedIn.DetachedNode (DetachedNode(..))
import LinkedIn.Output.Types (Output(..))
import LinkedIn.Page.WorkExperiences (WorkExperiencesPage(..))
import LinkedIn.Profile.WorkExperience (WorkExperience(..))
import LinkedIn.Profile.WorkExperience as PWE
import LinkedIn.UI.Basic.Types (Duration(..), TimeSpan(..))
import LinkedIn.UI.Components.ArtDeco (ArtDecoCenter(..), ArtDecoCenterContent(..), ArtDecoCenterHeader(..), ArtDecoPvsEntity(..), ArtDecoPvsEntitySubComponent(..))
import LinkedIn.UI.Components.ArtDecoCard (ArtDecoCardElement(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Utils (detachFromFile, fromDetachedToUI, getOutputFromFile, toMonthYear')
import Type.Proxy (Proxy(..))

artDecoCardsSpec :: Spec Unit
artDecoCardsSpec = do
  describe "Art deco cards parsing" do
    it "works" do
      cards <- detachFromFile (Proxy :: Proxy WorkExperiencesPage) "test/examples/andrew_ng_experiences.html"

      case cards of
        Left _ -> fail "Detach operation failed"
        Right (WorkExperiencesPage c) -> do
          let head = NEL.head c

          head `shouldEqual` ArtDecoCardElement {
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

          let
            jobOffer = (PWE.fromUI <=< fromDetachedToUI) head

          jobOffer `shouldEqual` Right (WorkExperience {
            company: Just "DeepLearning.AI",
            contractType: Nothing,
            description: Just "DeepLearning.AI provides\ntechnical training on Generative AI, Machine Learning, Deep Learning,\nand other topics. We also offer a widely read newsletter, The Batch\n(thebatch.ai), that covers what matters in AI right now. Our courses are often created with industry-leading AI companies (AWS,\nGoogle, OpenAI, etc.), and we offer both short courses that can be\ncompleted in an hour, and longer courses and specializations hosted on\nCoursera that give you a solid foundation in some aspect of AI. These\ncourses are designed to offer hands-on practice with AI technologies,\nand you will gain practical, job-ready skills. Whether you are just starting out in AI or seeking to further an existing\ncareer, come see if we can help, at http://deeplearning.ai!",
            duration: Just (YearsMonth 6 7),
            position: "Founder",
            timeSpan: Just (TimeSpanToToday (toMonthYear' June 2017))
          })


    it "reads the work experience" do
      wxpPage <- getOutputFromFile (Proxy :: Proxy WorkExperiencesPage) "test/examples/andrew_ng_experiences.html"

      case wxpPage of
        Left _ -> fail "Conversion to UI failed"
        Right (OutWorkExperiences weps) -> do
          let head = NEL.head weps
          head `shouldEqual` WorkExperience {
            company: Just "DeepLearning.AI",
            contractType: Nothing,
            description: Just "DeepLearning.AI provides\ntechnical training on Generative AI, Machine Learning, Deep Learning,\nand other topics. We also offer a widely read newsletter, The Batch\n(thebatch.ai), that covers what matters in AI right now. Our courses are often created with industry-leading AI companies (AWS,\nGoogle, OpenAI, etc.), and we offer both short courses that can be\ncompleted in an hour, and longer courses and specializations hosted on\nCoursera that give you a solid foundation in some aspect of AI. These\ncourses are designed to offer hands-on practice with AI technologies,\nand you will gain practical, job-ready skills. Whether you are just starting out in AI or seeking to further an existing\ncareer, come see if we can help, at http://deeplearning.ai!",
            duration: Just (YearsMonth 6 7),
            position: "Founder",
            timeSpan: Just (TimeSpanToToday (toMonthYear' June 2017))
          }
        Right _ -> fail "Conversion to UI failed"
