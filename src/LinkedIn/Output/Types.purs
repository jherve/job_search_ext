module LinkedIn.Output.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Show.Generic (genericShow)
import LinkedIn.Jobs.JobOffer as JO
import LinkedIn.Profile.Project (Project)
import LinkedIn.Profile.Skill (Skill)
import LinkedIn.Profile.WorkExperience (WorkExperience)

data Output =
  Projects (NonEmptyList Project)
  | Skills (NonEmptyList Skill)
  | WorkExperiences (NonEmptyList WorkExperience)
  | JobOffer JO.JobOffer

derive instance Generic Output _
instance Show Output where
  show = genericShow
