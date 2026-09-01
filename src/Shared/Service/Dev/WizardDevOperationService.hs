module Shared.Service.Dev.WizardDevOperationService where

import Shared.Api.Resource.Dev.DevExecutionDTO
import Shared.Api.Resource.Dev.DevExecutionResultDTO
import Shared.Api.Resource.Dev.DevSectionDTO
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Service.Common
import Shared.Service.Dev.DevOperationDefinitions
import Shared.Service.Dev.DevOperationService

getDevOperations' :: WizardRequestContextC s m => m [DevSectionDTO]
getDevOperations' = do
  checkIfAdminIsDisabled
  getDevOperations sections

executeOperation' :: WizardRequestContextC s m => DevExecutionDTO -> m AdminExecutionResultDTO
executeOperation' reqDto = do
  checkIfAdminIsDisabled
  executeOperation sections reqDto

checkIfAdminIsDisabled :: WizardRequestContextC s m => m ()
checkIfAdminIsDisabled =
  checkIfServerFeatureIsEnabled "Dev Operation Endpoints" (\s -> not s.admin.enabled)
