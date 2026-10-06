module Shared.Service.Plugin.PluginAuditLog where

import qualified Data.UUID as U

import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.SettingsAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Plugin.Plugin
import Shared.Service.AuditLog.AuditLogService

al_enablePlugin :: WizardRequestContextC s m => Maybe U.UUID -> Plugin -> Bool -> m ()
al_enablePlugin mWorkspaceUuid plugin enabled =
  logAuditLogNoData (if enabled then pluginsEnable else pluginsDisable) (pluginEntity mWorkspaceUuid plugin)

al_configurePlugin :: WizardRequestContextC s m => Maybe U.UUID -> Plugin -> m ()
al_configurePlugin mWorkspaceUuid plugin = logAuditLogNoData pluginsConfigure (pluginEntity mWorkspaceUuid plugin)

al_changePluginOverrideAllowed :: WizardRequestContextC s m => Plugin -> Bool -> m ()
al_changePluginOverrideAllowed plugin overrideAllowed =
  logAuditLogChanges
    pluginsOverrideAllowedChange
    (pluginEntity Nothing plugin)
    AuditLogOverrideAllowedChanges {workspaceOverrideAllowed = change plugin.workspaceOverrideAllowed overrideAllowed}

al_resetPluginOverride :: WizardRequestContextC s m => U.UUID -> Plugin -> m ()
al_resetPluginOverride workspaceUuid plugin = logAuditLogNoData pluginsOverrideReset (pluginEntity (Just workspaceUuid) plugin)

pluginEntity :: Maybe U.UUID -> Plugin -> AuditLogEntity
pluginEntity mWorkspaceUuid plugin = AuditLogEntity {uuid = plugin.uuid, name = plugin.url, workspaceUuid = mWorkspaceUuid}
