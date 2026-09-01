module Shared.Api.Handler.Api where

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
import Shared.Api.Handler.Prefab.Api
import Shared.Api.Handler.Project.Api
import Shared.Api.Handler.ProjectCommentThread.Api
import Shared.Api.Handler.ProjectFile.Api
import Shared.Api.Handler.Registry.Api
import Shared.Api.Handler.Submission.Api
import Shared.Api.Handler.Token.Api
import Shared.Api.Handler.TypeHint.Api
import Shared.Api.Handler.WizardCommon

type ApplicationAPI =
  ApiKeyAPI
    :<|> DevAPI
    :<|> DocumentTemplateAPI
    :<|> DocumentTemplateDraftAPI
    :<|> DocumentTemplateFolderAPI
    :<|> DocumentTemplateAssetAPI
    :<|> DocumentTemplateFileAPI
    :<|> DocumentAPI
    :<|> DomainAPI
    :<|> ExternalLinkAPI
    :<|> InfoAPI
    :<|> KnowledgeModelAPI
    :<|> KnowledgeModelEditorAPI
    :<|> KnowledgeModelPackageAPI
    :<|> KnowledgeModelSecretAPI
    :<|> PersistentCommandAPI
    :<|> PrefabAPI
    :<|> ProjectAPI
    :<|> ProjectCommentThreadAPI
    :<|> ProjectFileAPI
    :<|> RegistryAPI
    :<|> SubmissionAPI
    :<|> TokenAPI
    :<|> TypeHintAPI

applicationApi :: Proxy ApplicationAPI
applicationApi = Proxy

applicationServer :: WizardHandlerC s sm r rm => ServerT ApplicationAPI sm
applicationServer =
  apiKeyServer
    :<|> devServer
    :<|> documentTemplateServer
    :<|> documentTemplateDraftServer
    :<|> documentTemplateFolderServer
    :<|> documentTemplateAssetServer
    :<|> documentTemplateFileServer
    :<|> documentServer
    :<|> domainServer
    :<|> externalLinkServer
    :<|> infoServer
    :<|> knowledgeModelServer
    :<|> knowledgeModelEditorServer
    :<|> knowledgeModelPackageServer
    :<|> knowledgeModelSecretServer
    :<|> persistentCommandServer
    :<|> prefabServer
    :<|> projectServer
    :<|> projectCommentThreadServer
    :<|> projectFileServer
    :<|> registryServer
    :<|> submissionServer
    :<|> tokenServer
    :<|> typeHintServer
