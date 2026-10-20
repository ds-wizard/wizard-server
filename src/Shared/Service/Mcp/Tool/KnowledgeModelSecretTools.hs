module Shared.Service.Mcp.Tool.KnowledgeModelSecretTools where

import Data.Aeson
import Data.Proxy
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeJM ()
import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretJM ()
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Service.KnowledgeModel.Secret.KnowledgeModelSecretMapper (toChangeDTO)
import Shared.Service.KnowledgeModel.Secret.KnowledgeModelSecretService
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Util.JsonSchema
import Shared.Util.Secret

knowledgeModelSecretTools :: WizardRequestContextC s m => [McpTool m]
knowledgeModelSecretTools = [listKnowledgeModelSecretsTool, saveKnowledgeModelSecretTool, deleteKnowledgeModelSecretTool]

listKnowledgeModelSecretsTool :: forall s m. WizardRequestContextC s m => McpTool m
listKnowledgeModelSecretsTool =
  McpTool
    { name = "list_knowledge_model_secrets"
    , title = "List knowledge model secrets"
    , description =
        "List the knowledge model secrets with masked values and their updatedAt, those of the organization first; "
          ++ "with workspace_uuid or tenant only those that apply there"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ objectSchema ["secrets"] [("secrets", arraySchema (jsonSchema (Proxy :: Proxy KnowledgeModelSecret)))]
    , annotations = readOnlyAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION, _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = NoTransaction
    , run = withArguments arguments $ \inScope ->
        inScope ((\secrets -> object ["secrets" .= fmap maskKnowledgeModelSecret secrets]) <$> getKnowledgeModelSecrets)
    }
  where
    arguments = scopeArguments :: Arguments (m Value -> m Value)

saveKnowledgeModelSecretTool :: forall s m. WizardRequestContextC s m => McpTool m
saveKnowledgeModelSecretTool =
  McpTool
    { name = "save_knowledge_model_secret"
    , title = "Save knowledge model secret"
    , description =
        "Create a secret that the Jinja templates of API integrations read as secrets.<name>, or change its name or value; "
          ++ secretDescription
          ++ ". A new secret needs name and value and belongs to the organization (tenant: true) or to a workspace (workspace_uuid). "
          ++ uuidDescription "a secret"
          ++ ". "
          ++ expectedUpdatedAtDescription "secret"
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ jsonSchema (Proxy :: Proxy KnowledgeModelSecret)
    , annotations = writeAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION, _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments $ \(inScope, mUuid, mExpectedUpdatedAt, changes) ->
        inScope (toJSON . maskKnowledgeModelSecret <$> saveKnowledgeModelSecret mUuid mExpectedUpdatedAt changes)
    }
  where
    arguments =
      (,,,)
        <$> scopeArguments
        <*> uuidArgument secretEntityName
        <*> expectedUpdatedAtArgument
        <*> changesArguments (jsonSchema (Proxy :: Proxy KnowledgeModelSecretChangeDTO))
        :: Arguments (m Value -> m Value, Maybe U.UUID, Maybe UTCTime, Object)

saveKnowledgeModelSecret :: WizardRequestContextC s m => Maybe U.UUID -> Maybe UTCTime -> Object -> m KnowledgeModelSecret
saveKnowledgeModelSecret mUuid mExpectedUpdatedAt changes =
  runInTransaction $
    saveByUuid findKnowledgeModelSecretByUuid' (createSecretFromChanges changes) (updateSecretFromChanges mExpectedUpdatedAt changes) mUuid

createSecretFromChanges :: WizardRequestContextC s m => Object -> U.UUID -> m KnowledgeModelSecret
createSecretFromChanges changes uuid = createFromChanges secretEntityName ["name", "value"] parseChanges (createKnowledgeModelSecretWithUuid uuid) changes

updateSecretFromChanges :: WizardRequestContextC s m => Maybe UTCTime -> Object -> KnowledgeModelSecret -> m KnowledgeModelSecret
updateSecretFromChanges mExpectedUpdatedAt changes secret = do
  let current = toChangeDTO secret
  changed <- mergeChangesSince secretEntityName mExpectedUpdatedAt secret.updatedAt changes current
  let unmasked = changed {value = unmaskSecret current.value changed.value} :: KnowledgeModelSecretChangeDTO
  modifyIfChanged current unmasked (return secret) (modifyKnowledgeModelSecret secret.uuid)

maskKnowledgeModelSecret :: KnowledgeModelSecret -> KnowledgeModelSecret
maskKnowledgeModelSecret secret = secret {value = maskSecret secret.value}

deleteKnowledgeModelSecretTool :: WizardRequestContextC s m => McpTool m
deleteKnowledgeModelSecretTool =
  McpTool
    { name = "delete_knowledge_model_secret"
    , title = "Delete knowledge model secret"
    , description = "Delete a knowledge model secret; the integrations that read it no longer get its value. It cannot be undone"
    , inputSchema = argumentsSchema secretUuidArgument
    , outputSchema = Just $ objectSchema [] []
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION, _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION]
    , toolset = Just KmMcpToolset
    , transactionState = Transactional
    , run = withArguments secretUuidArgument $ \uuid -> object [] <$ deleteKnowledgeModelSecret uuid
    }

secretUuidArgument :: Arguments U.UUID
secretUuidArgument = requiredArgument "knowledge_model_secret_uuid" "The knowledge model secret"

secretEntityName :: String
secretEntityName = "knowledge model secret"
