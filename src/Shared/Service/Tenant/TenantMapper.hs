module Shared.Service.Tenant.TenantMapper where

import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U
import GHC.Records

import Shared.Api.Resource.Tenant.TenantChangeDTO
import Shared.Api.Resource.Tenant.TenantCreateDTO
import Shared.Api.Resource.Tenant.TenantDTO
import Shared.Api.Resource.Tenant.TenantDetailDTO
import Shared.Api.Resource.Tenant.Usage.WizardUsageDTO
import Shared.Constant.Api
import Shared.Model.Config.ServerConfig
import Shared.Model.Config.WizardServerConfig
import Shared.Model.Tenant.Tenant
import Shared.Model.User.User
import qualified Shared.Service.User.WizardUserMapper as U_Mapper
import Shared.Util.String

toDTO :: Tenant -> Maybe String -> Maybe String -> TenantDTO
toDTO tenant mLogoUrl mPrimaryColor =
  TenantDTO
    { uuid = tenant.uuid
    , tenantId = tenant.tenantId
    , name = tenant.name
    , serverDomain = tenant.serverDomain
    , serverUrl = tenantServerUrl tenant
    , clientUrl = tenant.clientUrl
    , state = tenant.state
    , enabled = tenant.enabled
    , multiWorkspace = tenant.multiWorkspace
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
    , clientUrl = tenant.clientUrl
    , state = tenant.state
    , enabled = tenant.enabled
    , multiWorkspace = tenant.multiWorkspace
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
        , multiWorkspace = False
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
        , multiWorkspace = False
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
        , multiWorkspace = tenant.multiWorkspace
        }

tenantServerUrl :: HasField "serverUrl" entity String => entity -> String
tenantServerUrl entity = f' "%s%s" [entity.serverUrl, apiPrefix]

createServerDomain :: ServerConfig -> String -> String
createServerDomain serverConfig tenantId = f' "%s.%s" [tenantId, fromMaybe "" serverConfig.cloud.domain]

createUrl :: ServerConfig -> String -> String
createUrl serverConfig tenantId = f' "https://%s.%s" [tenantId, fromMaybe "" serverConfig.cloud.domain]
