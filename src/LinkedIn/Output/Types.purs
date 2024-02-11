module LinkedIn.Output.Types where

import Prelude

import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.List.Types (NonEmptyList)
import Data.Show.Generic (genericShow)
import LinkedIn.Jobs.JobOffer as JO
import LinkedIn.PageUrl (PageUrl)
import LinkedIn.Profile.Project (Project)
import LinkedIn.Profile.Skill (Skill)
import LinkedIn.Profile.WorkExperience (WorkExperience)
import LinkedIn.QueryRunner (QueryError)

data Output =
  OutProjects (NonEmptyList Project)
  | OutSkills (NonEmptyList Skill)
  | OutWorkExperiences (NonEmptyList WorkExperience)
  | OutJobOffer JO.JobOffer

derive instance Generic Output _
derive instance Eq Output
instance Show Output where show = genericShow
instance EncodeJson Output where encodeJson a = genericEncodeJson a


data OutputError =
  ErrorOnDetach QueryError
  | ErrorOnUIConversion String
  | ErrorOnExtract String
  | ErrorUrlNotSupported PageUrl

derive instance Generic OutputError _
derive instance Eq OutputError
instance Show OutputError where show = genericShow
instance EncodeJson OutputError where encodeJson a = genericEncodeJson a
