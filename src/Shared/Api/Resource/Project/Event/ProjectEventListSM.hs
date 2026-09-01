module Shared.Api.Resource.Project.Event.ProjectEventListSM where

import Data.Swagger

import Shared.Api.Resource.Project.Event.ProjectEventListJM ()
import Shared.Api.Resource.Project.ProjectReplySM ()
import Shared.Api.Resource.User.UserSuggestionSM ()
import Shared.Database.Migration.Development.Project.Data.ProjectEvents
import Shared.Database.Migration.Development.Project.Data.Projects
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.Project.Event.ProjectEventList
import Shared.Model.Project.Project
import Shared.Service.Project.Event.ProjectEventMapper
import Shared.Util.Swagger

instance ToSchema ProjectEventList where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema SetReplyEventList where
  declareNamedSchema = toSwagger (toSetReplyEventList (sre_rQ1 project1.uuid) (Just userAlbert))

instance ToSchema ClearReplyEventList where
  declareNamedSchema = toSwagger (toClearReplyEventList (cre_rQ1 project1.uuid) (Just userAlbert))

instance ToSchema SetPhaseEventList where
  declareNamedSchema = toSwagger (toSetPhaseEventList (sphse_1 project1.uuid) (Just userAlbert))

instance ToSchema SetLabelsEventList where
  declareNamedSchema = toSwagger (toSetLabelsEventList (slble_rQ2 project1.uuid) (Just userAlbert))
