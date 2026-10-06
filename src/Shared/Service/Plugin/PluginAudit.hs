module Shared.Service.Plugin.PluginAudit where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Plugin.PluginChange
import Shared.Service.Audit.AuditService

auditPluginChange :: WizardRequestContextC s m => PluginChange -> m ()
auditPluginChange change =
  logAuditWithBody "plugin" (pluginChangeAction change) (U.toString (pluginChangePluginUuid change)) (pluginChangeBody change)

pluginChangeAction :: PluginChange -> String
pluginChangeAction PluginUpdated {} = "update"
pluginChangeAction PluginSettingsUpdated {} = "updateSettings"
pluginChangeAction PluginUpdatedInWorkspace {} = "updateInWorkspace"
pluginChangeAction PluginSettingsUpdatedInWorkspace {} = "updateSettingsInWorkspace"
pluginChangeAction PluginSettingsResetInWorkspace {} = "resetSettingsInWorkspace"

pluginChangePluginUuid :: PluginChange -> U.UUID
pluginChangePluginUuid (PluginUpdated pluginUuid _ _) = pluginUuid
pluginChangePluginUuid (PluginSettingsUpdated pluginUuid) = pluginUuid
pluginChangePluginUuid (PluginUpdatedInWorkspace _ pluginUuid _) = pluginUuid
pluginChangePluginUuid (PluginSettingsUpdatedInWorkspace _ pluginUuid) = pluginUuid
pluginChangePluginUuid (PluginSettingsResetInWorkspace _ pluginUuid) = pluginUuid

pluginChangeBody :: PluginChange -> M.Map String String
pluginChangeBody (PluginUpdated _ enabled mWorkspaceOverrideAllowed) =
  M.fromList $ ("enabled", show enabled) : maybe [] (\allowed -> [("workspaceOverrideAllowed", show allowed)]) mWorkspaceOverrideAllowed
pluginChangeBody (PluginSettingsUpdated _) = M.empty
pluginChangeBody (PluginUpdatedInWorkspace workspaceUuid _ enabled) = M.fromList [("workspaceUuid", U.toString workspaceUuid), ("enabled", show enabled)]
pluginChangeBody (PluginSettingsUpdatedInWorkspace workspaceUuid _) = M.singleton "workspaceUuid" (U.toString workspaceUuid)
pluginChangeBody (PluginSettingsResetInWorkspace workspaceUuid _) = M.singleton "workspaceUuid" (U.toString workspaceUuid)
