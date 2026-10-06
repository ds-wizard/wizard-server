module Shared.Api.Handler.Settings.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Settings.List_DELETE
import Shared.Api.Handler.Settings.List_GET
import Shared.Api.Handler.Settings.List_PUT
import Shared.Api.Handler.WizardCommon
import Shared.Model.Settings.Settings
import Shared.Service.Settings.OrganizationSettingsService
import Shared.Service.Settings.SettingsRolesService
import Shared.Service.Settings.WorkspaceSettingsService

type SettingsAPI =
  Tags "Settings"
    :> ( List_GET "authentication" SettingsAuthentication
           :<|> List_PUT "authentication" SettingsAuthentication
           :<|> List_GET "users" SettingsUsers
           :<|> List_PUT "users" SettingsUsers
           :<|> List_GET "roles" SettingsRoles
           :<|> List_PUT "roles" SettingsRoles
           :<|> List_GET "login-screen" SettingsLoginScreen
           :<|> List_PUT "login-screen" SettingsLoginScreen
           :<|> List_GET "features" SettingsFeatures
           :<|> List_PUT "features" SettingsFeatures
           :<|> List_GET "look-and-feel" SettingsLookAndFeel
           :<|> List_PUT "look-and-feel" SettingsLookAndFeel
           :<|> List_GET "dashboard-and-menu" SettingsDashboardAndMenu
           :<|> List_PUT "dashboard-and-menu" SettingsDashboardAndMenu
           :<|> List_DELETE "dashboard-and-menu"
           :<|> List_GET "projects" SettingsProjects
           :<|> List_PUT "projects" SettingsProjects
           :<|> List_DELETE "projects"
           :<|> List_GET "support" SettingsSupport
           :<|> List_PUT "support" SettingsSupport
           :<|> List_DELETE "support"
           :<|> List_GET "submission" SettingsSubmission
           :<|> List_PUT "submission" SettingsSubmission
           :<|> List_DELETE "submission"
       )

settingsApi :: Proxy SettingsAPI
settingsApi = Proxy

settingsServer :: WizardHandlerC s sm r rm => ServerT SettingsAPI sm
settingsServer =
  list_GET getSettingsAuthentication
    :<|> list_PUT modifySettingsAuthentication
    :<|> list_GET getSettingsUsers
    :<|> list_PUT modifySettingsUsers
    :<|> list_GET getSettingsRoles
    :<|> list_PUT modifySettingsRoles
    :<|> list_GET getSettingsLoginScreen
    :<|> list_PUT modifySettingsLoginScreen
    :<|> list_GET getSettingsFeatures
    :<|> list_PUT modifySettingsFeatures
    :<|> list_GET getSettingsLookAndFeel
    :<|> list_PUT modifySettingsLookAndFeel
    :<|> list_GET getSettingsDashboardAndMenu
    :<|> list_PUT modifySettingsDashboardAndMenu
    :<|> list_DELETE deleteSettingsDashboardAndMenu
    :<|> list_GET getSettingsProjects
    :<|> list_PUT modifySettingsProjects
    :<|> list_DELETE deleteSettingsProjects
    :<|> list_GET getSettingsSupport
    :<|> list_PUT modifySettingsSupport
    :<|> list_DELETE deleteSettingsSupport
    :<|> list_GET getSettingsSubmission
    :<|> list_PUT modifySettingsSubmission
    :<|> list_DELETE deleteSettingsSubmission
