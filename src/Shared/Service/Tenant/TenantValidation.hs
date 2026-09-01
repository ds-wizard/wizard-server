module Shared.Service.Tenant.TenantValidation where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Data.Foldable (forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import GHC.Unicode (isAlphaNum)
import Text.Regex (matchRegex, mkRegex)

import Shared.Api.Resource.Tenant.TenantChangeDTO
import Shared.Api.Resource.Tenant.TenantCreateDTO
import Shared.Database.DAO.Tenant.WizardTenantDAO
import Shared.Localization.Messages.Public
import Shared.Localization.Messages.WizardPublic
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.Tenant.Tenant
import Shared.Service.Common

validateTenantCreateDTO :: WizardRequestContextC s m => TenantCreateDTO -> Bool -> m ()
validateTenantCreateDTO reqDto isAdmin = do
  unless isAdmin validatePublicRegistrationEnabled
  validateTenantId reqDto.tenantId

validatePublicRegistrationEnabled :: WizardRequestContextC s m => m ()
validatePublicRegistrationEnabled = checkIfServerFeatureIsEnabled "Tenant Registration" (\s -> s.cloud.publicRegistrationEnabled)

validateTenantId :: WizardRequestContextC s m => String -> m ()
validateTenantId tenantId = do
  validateTenantIdFormat tenantId
  validateTenantIdUniqueness tenantId

validateTenantChangeDTO :: WizardRequestContextC s m => Tenant -> TenantChangeDTO -> m ()
validateTenantChangeDTO tenant reqDto = do
  validateTenantIdFormat reqDto.tenantId
  when (tenant.tenantId /= reqDto.tenantId) (validateTenantIdUniqueness reqDto.tenantId)

validateTenantIdFormat :: WizardRequestContextC s m => String -> m ()
validateTenantIdFormat tenantId = forM_ (isValidTenantIdFormat tenantId) throwError

isValidTenantIdFormat :: String -> Maybe AppError
isValidTenantIdFormat tenantId =
  if not (null tenantId) && isAlphaNum (head tenantId) && isAlphaNum (last tenantId) && isJust (matchRegex validationRegex tenantId)
    then Nothing
    else Just $ ValidationError [] (M.singleton "tenantId" [_ERROR_VALIDATION__FORBIDDEN_CHARACTERS tenantId])
  where
    validationRegex = mkRegex "^[a-z0-9-]+$"

validateTenantIdUniqueness :: WizardRequestContextC s m => String -> m ()
validateTenantIdUniqueness tenantId = do
  tenants <- findTenants
  let usedTenantIds = fmap (.tenantId) tenants ++ forbiddenTenantIds
  when
    (tenantId `elem` usedTenantIds)
    (throwError . ValidationError [] $ M.singleton "tenantId" [_ERROR_VALIDATION__TENANT_ID_UNIQUENESS])

forbiddenTenantIds =
  [ "app"
  , "chronograf"
  , "cloud"
  , "czech"
  , "dashboard"
  , "datenzee"
  , "dev"
  , "docker"
  , "e2e"
  , "files"
  , "fujtajbl"
  , "grafana"
  , "ideas"
  , "integrations"
  , "keyclock"
  , "kibana"
  , "ldap"
  , "mockserver"
  , "n8n"
  , "ppe"
  , "provisioning"
  , "rabbitmq"
  , "registry"
  , "registry"
  , "registry-ppe"
  , "registry-staging"
  , "registry-test"
  , "retro"
  , "s3"
  , "staging"
  , "status"
  , "storage-costs-evaluator"
  , "submit"
  , "swarmpit"
  , "www"
  ]
