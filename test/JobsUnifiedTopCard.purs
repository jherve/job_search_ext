module Test.JobsUnifiedTopCard where

import Prelude

import Data.Either (Either(..), hush, isRight)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty(..))
import Data.Traversable (traverse)
import Effect (Effect)
import LinkedIn.DetachedNode (DetachedNode(..), toDetached)
import LinkedIn.Jobs.JobOffer (JobOffer(..))
import LinkedIn.Jobs.JobOffer as JJO
import LinkedIn.JobsUnifiedTopCard (JobsUnifiedTopCardElement(..), TopCardAction(..), TopCardInsight(..), TopCardInsightContent(..), TopCardPrimaryDescription(..), TopCardSecondaryInsight(..))
import LinkedIn.Page.JobOffer (JobOfferPage(..))
import LinkedIn.Page.JobOffer as PageJO
import LinkedIn.Profile.Utils (fromDetachedToUI)
import LinkedIn.QueryRunner (runQuery)
import Node.JsDom (jsDomFromFile)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert, assertEqual)

main :: Effect Unit
main = do
  dom <- jsDomFromFile "test/examples/job_offer.html"

  wep <- runQuery $ PageJO.query dom

  assert $ isRight wep

  let
    JobOfferPage jobCard = unsafePartial $ fromJust $ hush wep

  topCard <- traverse toDetached jobCard

  assertEqual {
    actual: topCard,
    expected:  JobsUnifiedTopCardElement {
      actions: (Just (NonEmptyList
        (NonEmpty (TopCardActionButton (DetachedButton {
          classes: ("jobs-apply-button" : "artdeco-button" : "artdeco-button--3" : "artdeco-button--primary" : "ember-view" : Nil),
          content: "Candidature simplifiée",
          role: Nothing
        })) ((TopCardActionButton (DetachedButton {
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
              classes: Nil,
              content: "Sur site",
              id: Nothing,
              tag: "SPAN"
            }),
            secondary: (NonEmptyList (NonEmpty (TopCardSecondaryInsightNested
              (DetachedElement {
                classes: Nil,
                content: "Temps plein",
                id: Nothing,
                tag: "SPAN"
              })) ((TopCardSecondaryInsightPlain
              (DetachedElement {
                classes: ("job-details-jobs-unified-top-card__job-insight-view-model-secondary" : Nil),
                content: "Confirmé",
                id: Nothing,
                tag: "SPAN"
              })) : Nil))) }),
            icon: DetachedLiIcon "job"
        }) ((TopCardInsight {
          content: (TopCardInsightContentSingle (DetachedElement {
            classes: Nil,
            content: "201-500 employés · Technologies et services de l’information",
            id: Nothing,
            tag: "SPAN" })),
          icon: DetachedLiIcon "company"
        }) : (TopCardInsight {
            content: (TopCardInsightContentSingle (DetachedElement {
              classes: Nil,
              content: "2 anciens élèves travaillent ici",
              id: Nothing,
              tag: "SPAN" })),
            icon: DetachedLiIcon "people"
            }) : (TopCardInsight {
              content: (TopCardInsightContentSingle (DetachedElement {
                classes: Nil,
                content: "Découvrez comment vous vous positionnez par rapport à 87 candidats. Essai Premium pour 0 EUR",
                id: Nothing,
                tag: "SPAN" })),
              icon: (DetachedSvgElement { dataTestIcon: (Just "lightbulb-medium"), id: Nothing, tag: "svg" })
            }) : (TopCardInsight {
              content: (TopCardInsightContentButton (DetachedButton {
                classes: ("job-details-jobs-unified-top-card__job-insight-text-button" : Nil),
                content: "9 compétences sur 11 correspondent à votre profil, vous pourriez bien convenir pour ce poste",
                role: Nothing
              })),
              icon: (DetachedSvgElement { dataTestIcon: (Just "checklist-medium"), id: Nothing, tag: "svg" })
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
    }
  }


  assertEqual {
    actual: (JJO.fromUI <=< fromDetachedToUI) topCard,
    expected:
      Right (JobOffer {
        companyDomain: (Just "Technologies et services de l’information"),
        companyLink: "https://www.linkedin.com/company/lincoln-/life",
        companyName: "LINCOLN",
        companySize: (Just "201-500 employés"),
        hasSimplifiedApplicationProcess: true,
        location: (Just "Boulogne-Billancourt, Île-de-France, France"),
        remote: (Just "Sur site"),
        title: "Data Engineer H/F - Secteur Energie"
      })
  }
