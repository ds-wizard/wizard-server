module WizardServer.Api.Handler.Settings.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Settings.List_GET
import Shared.Api.Handler.Settings.List_PUT
import Shared.Api.Handler.WizardCommon
import Shared.Model.Settings.Settings
import Shared.Service.Settings.OrganizationSettingsService

type SettingsRegistryAPI =
  Tags "Settings"
    :> ( List_GET "registry" SettingsRegistry
           :<|> List_PUT "registry" SettingsRegistry
       )

settingsRegistryApi :: Proxy SettingsRegistryAPI
settingsRegistryApi = Proxy

settingsRegistryServer :: WizardHandlerC s sm r rm => ServerT SettingsRegistryAPI sm
settingsRegistryServer = list_GET getSettingsRegistry :<|> list_PUT modifySettingsRegistry
