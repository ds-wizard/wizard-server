module WizardServer.Api.Handler.Api where

import Servant

import Shared.Api.Handler.ApiKey.Api
import Shared.Api.Handler.Dev.Api
import Shared.Api.Handler.Document.Api
import Shared.Api.Handler.DocumentTemplate.Api
import Shared.Api.Handler.DocumentTemplateDraft.Api
import Shared.Api.Handler.DocumentTemplateDraft.Asset.Api
import Shared.Api.Handler.DocumentTemplateDraft.File.Api
import Shared.Api.Handler.DocumentTemplateDraft.Folder.Api
import Shared.Api.Handler.Domain.Api
import Shared.Api.Handler.ExternalLink.Api
import Shared.Api.Handler.Info.Api
import Shared.Api.Handler.KnowledgeModel.Api
import Shared.Api.Handler.KnowledgeModelEditor.Api
import Shared.Api.Handler.KnowledgeModelPackage.Api
import Shared.Api.Handler.KnowledgeModelSecret.Api
import Shared.Api.Handler.PersistentCommand.Api
import Shared.Api.Handler.PluginSettings.Api
import Shared.Api.Handler.Prefab.Api
import Shared.Api.Handler.Project.Api
import Shared.Api.Handler.ProjectCommentThread.Api
import Shared.Api.Handler.ProjectFile.Api
import Shared.Api.Handler.Settings.Api
import Shared.Api.Handler.Submission.Api
import Shared.Api.Handler.Token.Api
import Shared.Api.Handler.TypeHint.Api
import Shared.Api.Handler.WizardCommon
import Shared.Api.Handler.Workspace.Api
import Shared.Service.Plugin.PluginAudit
import WizardServer.Api.Handler.Bootstrap.Api
import WizardServer.Api.Handler.Locale.Api
import WizardServer.Api.Handler.OpenIdClient.Api
import WizardServer.Api.Handler.Role.Api
import WizardServer.Api.Handler.Settings.Api
import WizardServer.Api.Handler.Tenant.Api
import WizardServer.Api.Handler.User.Api
import WizardServer.Api.Handler.UserEmailLink.Api
import WizardServer.Api.Handler.UserGroup.Api

type ApplicationAPI =
  InfoAPI
    :<|> ApiKeyAPI
    :<|> BootstrapAPI
    :<|> DevAPI
    :<|> DocumentAPI
    :<|> DocumentTemplateAPI
    :<|> DocumentTemplateAssetAPI
    :<|> DocumentTemplateDraftAPI
    :<|> DocumentTemplateFileAPI
    :<|> DocumentTemplateFolderAPI
    :<|> DomainAPI
    :<|> ExternalLinkAPI
    :<|> KnowledgeModelAPI
    :<|> KnowledgeModelEditorAPI
    :<|> KnowledgeModelPackageAPI
    :<|> KnowledgeModelSecretAPI
    :<|> LocaleAPI
    :<|> LocalePullAPI
    :<|> OpenIdClientAPI
    :<|> PersistentCommandAPI
    :<|> PluginSettingsAPI
    :<|> PrefabAPI
    :<|> ProjectAPI
    :<|> ProjectCommentThreadAPI
    :<|> ProjectFileAPI
    :<|> RoleAPI
    :<|> SettingsAPI
    :<|> SettingsRegistryAPI
    :<|> SubmissionAPI
    :<|> TenantAPI
    :<|> TokenAPI
    :<|> TokenSystemAPI
    :<|> TypeHintAPI
    :<|> UserAPI
    :<|> UserEmailLinkAPI
    :<|> UserGroupAPI
    :<|> UserGroupSuggestionsAPI
    :<|> UserSubmissionPropsAPI
    :<|> WorkspaceAPI

applicationApi :: Proxy ApplicationAPI
applicationApi = Proxy

applicationServer :: WizardHandlerC s sm r rm => ServerT ApplicationAPI sm
applicationServer =
  infoServer
    :<|> apiKeyServer
    :<|> bootstrapServer
    :<|> devServer
    :<|> documentServer
    :<|> documentTemplateServer
    :<|> documentTemplateAssetServer
    :<|> documentTemplateDraftServer
    :<|> documentTemplateFileServer
    :<|> documentTemplateFolderServer
    :<|> domainServer
    :<|> externalLinkServer
    :<|> knowledgeModelServer
    :<|> knowledgeModelEditorServer
    :<|> knowledgeModelPackageServer
    :<|> knowledgeModelSecretServer
    :<|> localeServer
    :<|> localePullServer
    :<|> openIdClientServer
    :<|> persistentCommandServer
    :<|> pluginSettingsServer auditPluginChange
    :<|> prefabServer
    :<|> projectServer
    :<|> projectCommentThreadServer
    :<|> projectFileServer
    :<|> roleServer
    :<|> settingsServer
    :<|> settingsRegistryServer
    :<|> submissionServer
    :<|> tenantServer
    :<|> tokenServer
    :<|> tokenSystemServer
    :<|> typeHintServer
    :<|> userServer
    :<|> userEmailLinkServer
    :<|> userGroupServer
    :<|> userGroupSuggestionsServer
    :<|> userSubmissionPropsServer
    :<|> workspaceServer
