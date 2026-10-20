module Shared.Service.Mcp.Tool.McpToolRegistry where

import Data.Aeson (ToJSON)

import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpSettingsSection
import Shared.Model.Mcp.McpTool
import Shared.Service.Mcp.Tool.ApiKeyTools
import Shared.Service.Mcp.Tool.AuthoringGuideTools
import Shared.Service.Mcp.Tool.DocumentContextTools
import Shared.Service.Mcp.Tool.DocumentTemplateDraftTools
import Shared.Service.Mcp.Tool.DocumentTemplateFileTools
import Shared.Service.Mcp.Tool.DocumentTemplateLibraryTools
import Shared.Service.Mcp.Tool.DocumentTemplateLocaleTools
import Shared.Service.Mcp.Tool.DocumentTemplatePreviewTools
import Shared.Service.Mcp.Tool.DocumentTemplatePublishTools
import Shared.Service.Mcp.Tool.DocumentTools
import Shared.Service.Mcp.Tool.KnowledgeModelEditorReplyTools
import Shared.Service.Mcp.Tool.KnowledgeModelEditorTools
import Shared.Service.Mcp.Tool.KnowledgeModelEventTools
import Shared.Service.Mcp.Tool.KnowledgeModelIntegrationTools
import Shared.Service.Mcp.Tool.KnowledgeModelLocaleTools
import Shared.Service.Mcp.Tool.KnowledgeModelMigrationTools
import Shared.Service.Mcp.Tool.KnowledgeModelPackageTools
import Shared.Service.Mcp.Tool.KnowledgeModelPublishTools
import Shared.Service.Mcp.Tool.KnowledgeModelSecretTools
import Shared.Service.Mcp.Tool.KnowledgeModelTreeTools
import Shared.Service.Mcp.Tool.LocaleTools
import Shared.Service.Mcp.Tool.MailTemplateLocaleTools
import Shared.Service.Mcp.Tool.McpExecutableServices
import Shared.Service.Mcp.Tool.PluginSettingsTools
import Shared.Service.Mcp.Tool.PrefabTools
import Shared.Service.Mcp.Tool.ProjectCommentTools
import Shared.Service.Mcp.Tool.ProjectFileTools
import Shared.Service.Mcp.Tool.ProjectHistoryTools
import Shared.Service.Mcp.Tool.ProjectReportTools
import Shared.Service.Mcp.Tool.ProjectTools
import Shared.Service.Mcp.Tool.SettingsTools
import Shared.Service.Mcp.Tool.SubmissionPropTools
import Shared.Service.Mcp.Tool.TenantContextTools
import Shared.Service.Mcp.Tool.UserAccountTools
import Shared.Service.Mcp.Tool.UserGroupTools
import Shared.Service.Mcp.Tool.WorkspaceTools
import Shared.Util.JsonSchema

mcpTools :: (WizardRequestContextC s m, ToJSON usage, JsonSchema usage) => m usage -> [McpSettingsSection m] -> [McpTool m]
mcpTools getUsage settingsSections =
  tenantContextTools getUsage
    ++ authoringGuideTools
    ++ [describeKnowledgeModelEvents]
    ++ knowledgeModelEditorTools
    ++ knowledgeModelTreeTools
    ++ knowledgeModelEventTools
    ++ knowledgeModelPublishTools
    ++ knowledgeModelMigrationTools
    ++ knowledgeModelIntegrationTools
    ++ knowledgeModelPackageTools
    ++ knowledgeModelLocaleTools
    ++ knowledgeModelSecretTools
    ++ prefabTools
    ++ documentTemplateDraftTools
    ++ documentTemplateFileTools
    ++ documentContextTools
    ++ documentTemplatePreviewTools
    ++ documentTemplatePublishTools
    ++ documentTemplateLibraryTools
    ++ documentTemplateLocaleTools
    ++ knowledgeModelEditorReplyTools
    ++ projectTools
    ++ documentTools
    ++ projectHistoryTools
    ++ projectCommentTools
    ++ projectFileTools
    ++ projectReportTools
    ++ settingsTools (sharedSettingsSections ++ settingsSections)
    ++ pluginSettingsTools
    ++ localeTools
    ++ mailTemplateLocaleTools
    ++ workspaceTools
    ++ userGroupTools
    ++ submissionPropTools

accountTools :: WizardRequestContextC s m => McpExecutableServices m -> [McpTool m]
accountTools services =
  userAccountTools services.setUserPassword services.sendPasswordLink
    ++ apiKeyTools services.createApiKey services.deleteToken
