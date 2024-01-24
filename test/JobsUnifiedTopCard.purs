module Test.JobsUnifiedTopCard where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), isJust)
import Data.NonEmpty (NonEmpty(..))
import Data.Traversable (traverse)
import Effect (Effect)
import LinkedIn.DetachedNode (DetachedNode(..), toDetached)
import LinkedIn.JobsUnifiedTopCard (JobsUnifiedTopCardElement(..), TopCardAction(..), TopCardInsight(..), TopCardInsightContent(..), TopCardPrimaryDescription(..), TopCardSecondaryInsight(..), queryJobsUnifiedTopCardElement)
import LinkedIn (LinkedInUIElement(..), getJobsUnifiedTopCard)
import LinkedIn.QueryRunner (QueryError, runQuery)
import Node.JsDom (jsDomFromFile)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert, assertEqual)

testJobsUnifiedTopCard :: Effect Unit
testJobsUnifiedTopCard = do
  dom <- jsDomFromFile "test/examples/job_offer.html"
  topCard <- getJobsUnifiedTopCard dom
  assert $ isJust topCard
  headCard <- unsafePartial $ parseHeadCard topCard
  assertEqual {
    actual: headCard,
    expected:  Right (JobsUnifiedTopCardElement {
      actions: (Just (NonEmptyList
        (NonEmpty (TopCardActionApplyButton (DetachedButton {
          classes: ("jobs-apply-button" : "artdeco-button" : "artdeco-button--3" : "artdeco-button--primary" : "ember-view" : Nil),
          content: "Candidature simplifiée",
          role: Nothing
        })) ((TopCardActionApplyButton (DetachedButton {
          classes: ("jobs-save-button" : "artdeco-button" : "artdeco-button--3" : "artdeco-button--secondary" : Nil),
          content: "Enregistrer Enregistrer Data Engineer H/F - Secteur Energie chez LINCOLN",
          role: Nothing
        })) : Nil)))),
      header: (DetachedElement {
        classes: ("t-24" : "t-bold" : "job-details-jobs-unified-top-card__job-title" : Nil),
        content: "Data Engineer H/F - Secteur Energie",
        id: Nothing,
        tag: "H1"
      }),
      insights: (Just (NonEmptyList
        (NonEmpty (TopCardInsight {
          content: (TopCardInsightContentSecondary {
            primary: (DetachedElement {
              classes: ("" : Nil),
              content: "Sur site",
              id: Nothing,
              tag: "SPAN"
            }),
            secondary: (NonEmptyList (NonEmpty (TopCardSecondaryInsightNested
              (DetachedElement {
                classes: ("" : Nil),
                content: "Temps plein",
                id: Nothing,
                tag: "SPAN"
              })) ((TopCardSecondaryInsightPlain
              (DetachedElement {
                classes: ("job-details-jobs-unified-top-card__job-insight-view-model-secondary" : Nil),
                content: "Confirmé",
                id: (Just "undefined"),
                tag: "SPAN"
              })) : Nil))) }),
            icon: (DetachedElement {
              classes: ("" : Nil),
              content: "",
              id: Nothing,
              tag: "LI-ICON"
            })
        }) ((TopCardInsight {
          content: (TopCardInsightContentSingle (DetachedElement {
            classes: ("" : Nil),
            content: "201-500 employés · Technologies et services de l’information",
            id: (Just "undefined"),
            tag: "SPAN" })),
          icon: (DetachedElement {
            classes: ("" : Nil),
            content: "",
            id: Nothing,
            tag: "LI-ICON" })
        }) : (TopCardInsight {
            content: (TopCardInsightContentSingle (DetachedElement {
              classes: ("" : Nil),
              content: "2 anciens élèves travaillent ici",
              id: (Just "undefined"),
              tag: "SPAN" })),
            icon: (DetachedElement {
              classes: ("" : Nil),
              content: "",
              id: Nothing,
              tag: "LI-ICON" })
            }) : (TopCardInsight {
              content: (TopCardInsightContentSingle (DetachedElement {
                classes: ("" : Nil),
                content: "Découvrez comment vous vous positionnez par rapport à 87 candidats. Essai Premium pour 0 EUR",
                id: (Just "undefined"),
                tag: "SPAN" })),
              icon: (DetachedElement {
                classes: Nil,
                content: "",
                id: Nothing,
                tag: "svg"
              })
            }) : (TopCardInsight {
              content: (TopCardInsightContentButton (DetachedButton {
                classes: ("job-details-jobs-unified-top-card__job-insight-text-button" : Nil),
                content: "9 compétences sur 11 correspondent à votre profil, vous pourriez bien convenir pour ce poste",
                role: Nothing
              })),
              icon: (DetachedElement {
                classes: Nil,
                content: "",
                id: Nothing,
                tag: "svg"
              })
            }) : Nil)))),
      primaryDescription: (TopCardPrimaryDescription {
        link: (DetachedA { content: "LINCOLN", href: "https://www.linkedin.com/company/lincoln-/life" }),
        text: (DetachedText "· Boulogne-Billancourt, Île-de-France, France"),
        tvmText: (Just (NonEmptyList
          (NonEmpty (DetachedElement {
            classes: ("tvm__text" : "tvm__text--neutral" : Nil),
            content: "il y a 2 semaines",
            id: Nothing,
            tag: "SPAN"
          }) ((DetachedElement {
            classes: ("tvm__text" : "tvm__text--neutral" : Nil),
            content: "·",
            id: Nothing,
            tag: "SPAN"
          }) : (DetachedElement {
            classes: ("tvm__text" : "tvm__text--neutral" : Nil),
            content: "87 candidats",
            id: Nothing,
            tag: "SPAN"
          }) : Nil
          ))
        ))
      })
    })
  }


parseHeadCard ∷ Partial ⇒ Maybe (NonEmptyList LinkedInUIElement) → Effect (Either QueryError (JobsUnifiedTopCardElement DetachedNode))
parseHeadCard (Just l) = do
  queried <- (\(LinkedInUIElement _ n) -> runQuery $ queryJobsUnifiedTopCardElement n) $ NEL.head l
  case queried of
    Left l' -> pure $ Left l'
    Right q -> do
      parsed <- traverse toDetached q
      pure $ Right parsed

