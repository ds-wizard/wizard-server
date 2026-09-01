module Shared.Service.Tenant.Config.ConfigValidation where

import Control.Monad.Except (throwError)
import Data.Foldable (forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Text.Regex (matchRegex, mkRegex)

import Shared.Api.Resource.Tenant.Config.WizardTenantConfigChangeDTO
import Shared.Localization.Messages.Coordinate.Public
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Service.Project.ProjectValidation

validateTenantConfig :: WizardRequestContextC s m => TenantConfigChangeDTO -> m ()
validateTenantConfig reqDto = do
  validateOrganization reqDto.organization
  validateProject reqDto.project

validateOrganization :: WizardRequestContextC s m => TenantConfigOrganizationChangeDTO -> m ()
validateOrganization config = forM_ (isValidOrganizationId config.organizationId) throwError

isValidOrganizationId :: String -> Maybe AppError
isValidOrganizationId kmId =
  if isJust $ matchRegex validationRegex kmId
    then Nothing
    else Just $ ValidationError [] (M.singleton "organizationId" [_ERROR_VALIDATION__INVALID_ORG_ID_FORMAT])
  where
    validationRegex = mkRegex "^[a-zA-Z0-9_.-]+$"

validateProject :: WizardRequestContextC s m => TenantConfigProjectChangeDTO -> m ()
validateProject config = validateProjectTags config.projectTagging.tags
