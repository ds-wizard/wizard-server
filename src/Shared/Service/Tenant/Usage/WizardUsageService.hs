module Shared.Service.Tenant.Usage.WizardUsageService where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U

import Shared.Api.Resource.Tenant.Usage.WizardUsageDTO
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import qualified Shared.Service.Tenant.Usage.UsageService as Usage

getUsage :: WizardRequestContextC s m => U.UUID -> m WizardUsageDTO
getUsage = Usage.getUsage

getUsageForCurrentTenant :: WizardRequestContextC s m => m WizardUsageDTO
getUsageForCurrentTenant = do
  checkPermission _SETTINGS_MANAGE_ROLE_PERMISSION
  tenantUuid <- asks (.tenantUuid')
  Usage.getUsage tenantUuid
