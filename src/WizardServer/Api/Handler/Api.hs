module WizardServer.Api.Handler.Api where

import Servant

import Shared.Api.Handler.WizardCommon
import WizardServer.Api.Handler.Config.Api
import WizardServer.Api.Handler.Locale.Api
import WizardServer.Api.Handler.OpenIdClient.Api
import WizardServer.Api.Handler.Role.Api
import WizardServer.Api.Handler.Tenant.Api
import WizardServer.Api.Handler.User.Api
import WizardServer.Api.Handler.UserEmailLink.Api
import WizardServer.Api.Handler.UserGroup.Api

type ManagementAPI =
  UserEmailLinkAPI
    :<|> ConfigAPI
    :<|> LocaleAPI
    :<|> OpenIdClientAPI
    :<|> RoleAPI
    :<|> TenantAPI
    :<|> UserAPI
    :<|> UserGroupAPI

managementApi :: Proxy ManagementAPI
managementApi = Proxy

managementServer :: WizardHandlerC s sm r rm => ServerT ManagementAPI sm
managementServer =
  userEmailLinkServer
    :<|> configServer
    :<|> localeServer
    :<|> openIdClientServer
    :<|> roleServer
    :<|> tenantServer
    :<|> userServer
    :<|> userGroupServer
