module Wizard.Service.Tenant.TenantMapper where

import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U
import GHC.Records

import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.String
import Wizard.Api.Resource.Tenant.TenantChangeDTO
import Wizard.Api.Resource.Tenant.TenantCreateDTO
import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Api.Resource.Tenant.TenantDetailDTO
import Wizard.Model.Config.ServerConfig
import Wizard.Model.User.User
import qualified Wizard.Service.User.UserMapper as U_Mapper
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageDTO
import WizardLib.Public.Model.Tenant.Tenant
import WizardLib.Public.Model.Tenant.TenantSuggestion

toDTO :: Tenant -> Maybe String -> Maybe String -> TenantDTO
toDTO tenant mLogoUrl mPrimaryColor =
  TenantDTO
    { uuid = tenant.uuid
    , tenantId = tenant.tenantId
    , name = tenant.name
    , serverDomain = tenant.serverDomain
    , serverUrl = tenantServerUrl tenant
    , clientUrl = tenantClientUrl tenant
    , state = tenant.state
    , enabled = tenant.enabled
    , logoUrl = mLogoUrl
    , primaryColor = mPrimaryColor
    , createdAt = tenant.createdAt
    , updatedAt = tenant.updatedAt
    }

toDetailDTO :: Tenant -> Maybe String -> Maybe String -> WizardUsageDTO -> [User] -> TenantDetailDTO
toDetailDTO tenant mLogoUrl mPrimaryColor usage users =
  TenantDetailDTO
    { uuid = tenant.uuid
    , tenantId = tenant.tenantId
    , name = tenant.name
    , serverDomain = tenant.serverDomain
    , serverUrl = tenantServerUrl tenant
    , clientUrl = tenantClientUrl tenant
    , state = tenant.state
    , enabled = tenant.enabled
    , logoUrl = mLogoUrl
    , primaryColor = mPrimaryColor
    , usage = usage
    , users = fmap U_Mapper.toDTO users
    , createdAt = tenant.createdAt
    , updatedAt = tenant.updatedAt
    }

toChangeDTO :: Tenant -> TenantChangeDTO
toChangeDTO tenant = TenantChangeDTO {tenantId = tenant.tenantId, name = tenant.name}

fromRegisterCreateDTO :: TenantCreateDTO -> U.UUID -> ServerConfig -> UTCTime -> Tenant
fromRegisterCreateDTO reqDto aUuid serverConfig now =
  let url = createUrl serverConfig reqDto.tenantId
   in Tenant
        { uuid = aUuid
        , tenantId = reqDto.tenantId
        , name = reqDto.tenantId
        , serverDomain = createServerDomain serverConfig reqDto.tenantId
        , serverUrl = url
        , clientUrl = url
        , enabled = True
        , state = ReadyForUseTenantState
        , createdAt = now
        , updatedAt = now
        }

fromAdminCreateDTO :: TenantCreateDTO -> U.UUID -> ServerConfig -> UTCTime -> Tenant
fromAdminCreateDTO reqDto aUuid serverConfig now =
  let url = createUrl serverConfig reqDto.tenantId
   in Tenant
        { uuid = aUuid
        , tenantId = reqDto.tenantId
        , name = reqDto.tenantName
        , serverDomain = createServerDomain serverConfig reqDto.tenantId
        , serverUrl = url
        , clientUrl = url
        , enabled = True
        , state = ReadyForUseTenantState
        , createdAt = now
        , updatedAt = now
        }

fromChangeDTO :: Tenant -> TenantChangeDTO -> ServerConfig -> Tenant
fromChangeDTO tenant reqDto serverConfig =
  let (serverDomain, url) =
        if serverConfig.admin.enabled
          then (tenant.serverDomain, tenant.serverUrl)
          else (createServerDomain serverConfig reqDto.tenantId, createUrl serverConfig reqDto.tenantId)
   in Tenant
        { uuid = tenant.uuid
        , tenantId = reqDto.tenantId
        , name = reqDto.name
        , serverDomain = serverDomain
        , serverUrl = url
        , clientUrl = url
        , enabled = tenant.enabled
        , state = tenant.state
        , createdAt = tenant.createdAt
        , updatedAt = tenant.updatedAt
        }

toSuggestionUrls :: TenantSuggestion -> TenantSuggestion
toSuggestionUrls suggestion = suggestion {clientUrl = tenantClientUrl suggestion}

tenantServerUrl :: HasField "serverUrl" entity String => entity -> String
tenantServerUrl entity = f' "%s/wizard-api" [entity.serverUrl]

tenantClientUrl :: HasField "clientUrl" entity String => entity -> String
tenantClientUrl entity = f' "%s/wizard" [entity.clientUrl]

toClientUrlBase :: String -> String
toClientUrlBase = stripSuffixIfExists "/wizard"

createServerDomain :: ServerConfig -> String -> String
createServerDomain serverConfig tenantId = f' "%s.%s" [tenantId, fromMaybe "" serverConfig.cloud.domain]

createUrl :: ServerConfig -> String -> String
createUrl serverConfig tenantId = f' "https://%s.%s" [tenantId, fromMaybe "" serverConfig.cloud.domain]
