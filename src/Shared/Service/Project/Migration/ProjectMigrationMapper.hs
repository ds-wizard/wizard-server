module Shared.Service.Project.Migration.ProjectMigrationMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Api.Resource.Project.Migration.ProjectMigrationResultDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Model.Project.Event.ProjectEvent

toProjectPhaseEvent :: U.UUID -> Maybe U.UUID -> U.UUID -> U.UUID -> Maybe UserDTO -> UTCTime -> ProjectEvent
toProjectPhaseEvent phaseEventUuid kmPhaseUuid projectUuid tenantUuid mCurrentUserUuid now =
  SetPhaseEvent' $
    SetPhaseEvent
      { uuid = phaseEventUuid
      , phaseUuid = kmPhaseUuid
      , projectUuid = projectUuid
      , tenantUuid = tenantUuid
      , createdBy = fmap (.uuid) mCurrentUserUuid
      , createdAt = now
      }

toMigrationResultDTO :: ProjectDetailQuestionnaireDTO -> ProjectMigrationResultDTO
toMigrationResultDTO project =
  ProjectMigrationResultDTO
    { uuid = project.uuid
    , name = project.name
    , knowledgeModelPackage = project.knowledgeModelPackage
    , selectedQuestionTagUuids = project.selectedQuestionTagUuids
    , phaseUuid = project.phaseUuid
    }
