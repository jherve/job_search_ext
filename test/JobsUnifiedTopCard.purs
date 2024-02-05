module Test.JobsUnifiedTopCard where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Effect.Aff (Aff)
import LinkedIn.DetachedNode (DetachedNode(..))
import LinkedIn.Jobs.JobOffer (JobOffer(..))
import LinkedIn.Output.Types (Output(..))
import LinkedIn.Page.JobOffer (JobOfferPage(..))
import LinkedIn.UI.Basic.Types (JobFlexibility(..))
import LinkedIn.UI.Components.JobsUnifiedTopCard (JobsUnifiedTopCardElement(..), TopCardAction(..), TopCardInsight(..), TopCardInsightContent(..), TopCardPrimaryDescription(..), TopCardSecondaryInsight(..))
import Test.Spec (Spec, SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (detachFromFile, getOutputFromFile)
import Type.Proxy (Proxy(..))

type TestCase a = {
  detached ∷ a DetachedNode,
  filePath ∷ String,
  id ∷ String,
  output ∷ Output,
  url ∷ String
}

jobsUnifiedTopCardSpec :: Spec Unit
jobsUnifiedTopCardSpec = do
  describe "Jobs top card parsing" do
    for_ [jobOfferPage_3786945580, jobOfferPage_3797662873] runTest

runTest ∷ ∀ m. Monad m ⇒ TestCase JobOfferPage → SpecT Aff Unit m Unit
runTest {detached, filePath, id, output} = do
  it ("reads well as a JobOfferPage DetachedNode " <> show id) do
    topCard <- detachFromFile (Proxy :: Proxy JobOfferPage) filePath
    topCard `shouldEqual` Right(detached)

  it ("reads the JobOffer " <> show id) do
    jobOffer <- getOutputFromFile (Proxy :: Proxy JobOfferPage) filePath
    jobOffer `shouldEqual` Right (output)

jobOfferPage_3786945580 ∷ TestCase JobOfferPage
jobOfferPage_3786945580 = {
  id: "3786945580",
  filePath: "test/examples/job_offer_3786945580.html",
  url: "https://www.linkedin.com/jobs/view/3786945580/",
  detached: JobOfferPage (JobsUnifiedTopCardElement {
    actions: TopCardActionButton
      (DetachedButton {
        classes: ("jobs-apply-button" : "artdeco-button" : "artdeco-button--3" : "artdeco-button--primary" : "ember-view" : Nil),
        content: "Candidature simplifiée",
        role: Nothing
      })
      : TopCardActionButton (DetachedButton {
        classes: ("jobs-save-button" : "artdeco-button" : "artdeco-button--3" : "artdeco-button--secondary" : Nil),
        content: "Enregistrer Enregistrer Data Engineer H/F - Secteur Energie chez LINCOLN",
        role: Nothing
      })
      : Nil,
    header: (DetachedElement {
      classes: ("t-24" : "t-bold" : "job-details-jobs-unified-top-card__job-title" : Nil),
      content: "Data Engineer H/F - Secteur Energie",
      id: Nothing,
      tag: "H1"
    }),
    insights: (TopCardInsight {
        content: (TopCardInsightContentSecondary {
          primary: (DetachedElement {classes: Nil, content: "Sur site", id: Nothing, tag: "SPAN"}),
          secondary: (NonEmptyList (NonEmpty (TopCardSecondaryInsightNested
            (DetachedElement {classes: Nil, content: "Temps plein", id: Nothing, tag: "SPAN"}))
            ((TopCardSecondaryInsightPlain
            (DetachedElement {classes: ("job-details-jobs-unified-top-card__job-insight-view-model-secondary" : Nil), content: "Confirmé", id: Nothing,tag: "SPAN" })) : Nil))
          )
        }),
        icon: DetachedLiIcon "job"
      }) : (TopCardInsight {
          content: (TopCardInsightContentSingle (
            DetachedElement {classes: Nil, content: "201-500 employés · Technologies et services de l’information", id: Nothing, tag: "SPAN" }
          )),
          icon: DetachedLiIcon "company"
        }) : (TopCardInsight {
            content: (TopCardInsightContentSingle (
              DetachedElement {classes: Nil, content: "2 anciens élèves travaillent ici", id: Nothing, tag: "SPAN" }
            )),
            icon: DetachedLiIcon "people"
            }) : (TopCardInsight {
              content: (TopCardInsightContentSingle (
                DetachedElement {classes: Nil, content: "Découvrez comment vous vous positionnez par rapport à 87 candidats. Essai Premium pour 0 EUR", id: Nothing, tag: "SPAN" })),
              icon: (DetachedSvgElement { dataTestIcon: (Just "lightbulb-medium"), id: Nothing, tag: "svg" })
            }) : (TopCardInsight {
              content: (TopCardInsightContentButton (
                DetachedButton {classes: ("job-details-jobs-unified-top-card__job-insight-text-button" : Nil), content: "9 compétences sur 11 correspondent à votre profil, vous pourriez bien convenir pour ce poste", role: Nothing}
              )),
              icon: (DetachedSvgElement { dataTestIcon: (Just "checklist-medium"), id: Nothing, tag: "svg" })
            }) : Nil,
    primaryDescription: (TopCardPrimaryDescription {
      link: (DetachedA { content: "LINCOLN", href: "https://www.linkedin.com/company/lincoln-/life" }),
      text: (DetachedText "· Boulogne-Billancourt, Île-de-France, France"),
      tvmText:
        DetachedElement {classes: ("tvm__text" : "tvm__text--neutral" : Nil), content: "il y a 2 semaines", id: Nothing, tag: "SPAN"}
        : DetachedElement {classes: ("tvm__text" : "tvm__text--neutral" : Nil), content: "·", id: Nothing, tag: "SPAN"}
        : DetachedElement {classes: ("tvm__text" : "tvm__text--neutral" : Nil), content: "87 candidats", id: Nothing, tag: "SPAN"}
        : Nil
    })
  }),
  output: OutJobOffer (JobOffer {
    companyDomain: (Just "Technologies et services de l’information"),
    companyLink: "https://www.linkedin.com/company/lincoln-/life",
    companyName: "LINCOLN",
    companySize: (Just "201-500 employés"),
    hasSimplifiedApplicationProcess: true,
    location: (Just "Boulogne-Billancourt, Île-de-France, France"),
    flexibility: (Just JobFlexOnSite),
    title: "Data Engineer H/F - Secteur Energie"
  })
}

jobOfferPage_3797662873 ∷ TestCase JobOfferPage
jobOfferPage_3797662873 = {
  id: "3797662873",
  filePath: "test/examples/job_offer_3797662873.html",
  url: "https://www.linkedin.com/jobs/view/3797662873/",
  detached: JobOfferPage (JobsUnifiedTopCardElement {
    actions: (TopCardActionButton
      (DetachedButton { classes: ("jobs-apply-button" : "artdeco-button" : "artdeco-button--3" : "artdeco-button--primary" : "ember-view" : Nil), content: "Candidature simplifiée", role: Nothing }))
      : ((TopCardActionButton (DetachedButton {
        classes: ("jobs-save-button" : "artdeco-button" : "artdeco-button--3" : "artdeco-button--secondary" : Nil),
        content: "Enregistrer Enregistrer Data Engineer Confirmé(e)/Senior (CDI) chez CBTW",
        role: Nothing
      })) : Nil),
    header: (DetachedElement {
      classes: ("t-24" : "t-bold" : "job-details-jobs-unified-top-card__job-title" : Nil),
      content: "Data Engineer Confirmé(e)/Senior (CDI)",
      id: Nothing,
      tag: "H1"
    }),
    insights: TopCardInsight {
        content: (TopCardInsightContentSecondary {
          primary: (DetachedElement { classes: Nil, content: "Hybride", id: Nothing, tag: "SPAN" }),
          secondary: (NonEmptyList (NonEmpty (TopCardSecondaryInsightNested
          (DetachedElement { classes: Nil, content: "Temps plein", id: Nothing, tag: "SPAN" })
          ) ((TopCardSecondaryInsightPlain (
            DetachedElement { classes: ("job-details-jobs-unified-top-card__job-insight-view-model-secondary" : Nil), content: "Confirmé", id: Nothing, tag: "SPAN" })) : Nil)))
        }),
        icon: (DetachedLiIcon "job")
      } : TopCardInsight {
        content: (TopCardInsightContentSingle (DetachedElement { classes: Nil, content: "1 001-5 000 employés · Développement de logiciels", id: Nothing, tag: "SPAN" })),
        icon: (DetachedLiIcon "company")
      } : TopCardInsight {
        content: (TopCardInsightContentSingle (DetachedElement { classes: Nil, content: "Amina Berkane recrute à ce poste", id: Nothing, tag: "SPAN" })),
        icon: (DetachedImg { classes: ("ivm-view-attr__img--centered" : "EntityPhoto-circle-1" : "evi-image" : "lazy-image" : "ember-view" : Nil), src: "job_offer_2_files/1706175960816.jpg" })
      } : TopCardInsight {
        content: (TopCardInsightContentButton (DetachedButton { classes: ("job-details-jobs-unified-top-card__job-insight-text-button" : Nil), content: "8 compétences sur 10 correspondent à votre profil, vous pourriez bien convenir pour ce poste", role: Nothing })),
        icon: (DetachedSvgElement { dataTestIcon: (Just "checklist-medium"), id: Nothing, tag: "svg" })
      } : TopCardInsight {
        content: (TopCardInsightContentSingle (DetachedElement { classes: Nil, content: "Le délai d’évaluation des candidatures est généralement de 4 jours", id: Nothing, tag: "SPAN" })),
        icon: (DetachedSvgElement { dataTestIcon: (Just "responsive-medium"), id: Nothing, tag: "svg" })
      } : TopCardInsight {
        content: (TopCardInsightContentSingle (DetachedElement { classes: Nil, content: "Découvrez comment vous vous positionnez par rapport à 39 candidats. Essai Premium pour 0 EUR", id: Nothing, tag: "SPAN" })),
        icon: (DetachedSvgElement { dataTestIcon: (Just "lightbulb-medium"), id: Nothing, tag: "svg" })
      } : Nil,
    primaryDescription: (TopCardPrimaryDescription {
      link: (DetachedA { content: "CBTW", href: "https://www.linkedin.com/company/collaboration-betters-the-world/life" }),
      text: (DetachedText "· Paris et périphérie ·"),
      tvmText: DetachedElement { classes: ("tvm__text" : "tvm__text--neutral" : Nil), content: "il y a 4 semaines", id: Nothing, tag: "SPAN" }
        : (DetachedElement { classes: ("tvm__text" : "tvm__text--neutral" : Nil), content: "·", id: Nothing, tag: "SPAN" })
        : (DetachedElement { classes: ("tvm__text" : "tvm__text--neutral" : Nil), content: "39 candidats", id: Nothing, tag: "SPAN" })
        : Nil
    })
  }),
  output: OutJobOffer (JobOffer {
    companyDomain: Just "Développement de logiciels",
    companyLink: "https://www.linkedin.com/company/collaboration-betters-the-world/life",
    companyName: "CBTW",
    companySize: Just "1 001-5 000 employés",
    flexibility: Just JobFlexHybrid,
    hasSimplifiedApplicationProcess: true,
    location: (Just "Paris et périphérie ·"),
    title: "Data Engineer Confirmé(e)/Senior (CDI)"
  })
}
