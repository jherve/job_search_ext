module Test.ArtDecoCard where

import LinkedIn.ArtDecoCard
import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NEL
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isJust)
import Data.NonEmpty (NonEmpty(..))
import Effect (Effect)
import LinkedIn (DetachedNode(..), LinkedInUIElement(..), getArtDecoCards)
import Node.JsDom (jsDomFromFile)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert, assertEqual)

testArtDecoCards :: Effect Unit
testArtDecoCards = do
  dom <- jsDomFromFile "test/examples/andrew_ng_experiences.html"
  artDecoCards <- getArtDecoCards dom
  assert $ isJust artDecoCards
  headCard <- unsafePartial $ parseHeadCard artDecoCards
  assertEqual {
    actual: headCard,
    expected: Right (
      ArtDecoCardElement {
        pvs_entity: (ArtDecoPvsEntity {
          center: (ArtDecoCenter {
            content: (ArtDecoCenterContent
              (NonEmptyList (NonEmpty
                (ArtDecoPvsEntitySubComponent (
                  DetachedElement {
                    classes: ("" : Nil),
                    content: "DeepLearning.AI provides\ntechnical training on Generative AI, Machine Learning, Deep Learning,\nand other topics. We also offer a widely read newsletter, The Batch\n(thebatch.ai), that covers what matters in AI right now. Our courses are often created with industry-leading AI companies (AWS,\nGoogle, OpenAI, etc.), and we offer both short courses that can be\ncompleted in an hour, and longer courses and specializations hosted on\nCoursera that give you a solid foundation in some aspect of AI. These\ncourses are designed to offer hands-on practice with AI technologies,\nand you will gain practical, job-ready skills. Whether you are just starting out in AI or seeking to further an existing\ncareer, come see if we can help, at http://deeplearning.ai!",
                    id: Nothing,
                    tag: "SPAN"
                    })) Nil))),
            header: (ArtDecoCenterHeader {
              bold: (DetachedElement {
                classes: ("" : Nil),
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
                    classes: ("" : Nil),
                    content: "Palo Alto, California, United States",
                    id: Nothing,
                    tag: "SPAN"
                  }) : Nil)))),
              normal: (Just (DetachedElement {
                classes: ("" : Nil),
                content: "DeepLearning.AI",
                id: Nothing,
                tag: "SPAN" }))
              }) }),
          side: unit
        })
      }
    )
  }

parseHeadCard ∷ Partial => Maybe (NonEmptyList LinkedInUIElement) → Effect (Either ParseError ArtDecoCardElement)
parseHeadCard (Just l) = do
  parsed <- (\(LinkedInUIElement _ n) -> parseArtDecoCard n) $ NEL.head l
  pure $ parsed

testArtDecoCard :: Effect Unit
testArtDecoCard = do
  testArtDecoCards
