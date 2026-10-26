module Shared.Service.KnowledgeModel.Secret.KnowledgeModelSecretService where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Shared.Database.DAO.WizardCommon
import Shared.Localization.Messages.Public
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Service.KnowledgeModel.Secret.KnowledgeModelSecretAuditLog
import Shared.Service.KnowledgeModel.Secret.KnowledgeModelSecretMapper
import Shared.Service.Workspace.WorkspaceScopeService
import Shared.Util.Date (getCurrentTimeInMicroseconds)
import Shared.Util.Uuid

getKnowledgeModelSecrets :: WizardRequestContextC s m => m [KnowledgeModelSecret]
getKnowledgeModelSecrets = do
  checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
  findKnowledgeModelSecrets

getKnowledgeModelSecretsDto :: WizardRequestContextC s m => m [KnowledgeModelSecretDTO]
getKnowledgeModelSecretsDto = fmap toDTO <$> getKnowledgeModelSecrets

createKnowledgeModelSecret :: WizardRequestContextC s m => KnowledgeModelSecretChangeDTO -> m KnowledgeModelSecretDTO
createKnowledgeModelSecret reqDto = do
  uuid <- liftIO generateUuid
  toDTO <$> createKnowledgeModelSecretWithUuid uuid reqDto

createKnowledgeModelSecretWithUuid :: WizardRequestContextC s m => U.UUID -> KnowledgeModelSecretChangeDTO -> m KnowledgeModelSecret
createKnowledgeModelSecretWithUuid uuid reqDto =
  runInTransaction $ do
    workspaceUuid <- requireTenantOrWorkspaceScope
    checkPermissionInWorkspace _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION workspaceUuid
    value <- maybe (throwError $ ValidationError [] (M.singleton "value" [_ERROR_VALIDATION__FIELDS_ABSENCE])) return reqDto.value
    now <- liftIO getCurrentTimeInMicroseconds
    tenantUuid <- asks (.tenantUuid')
    let kmSecret = fromCreateDTO reqDto value uuid tenantUuid workspaceUuid now
    insertKnowledgeModelSecret kmSecret
    al_createKnowledgeModelSecret kmSecret
    return kmSecret

modifyKnowledgeModelSecret :: WizardRequestContextC s m => U.UUID -> KnowledgeModelSecretChangeDTO -> m KnowledgeModelSecret
modifyKnowledgeModelSecret uuid reqDto =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
    kmSecret <- findKnowledgeModelSecretByUuid uuid
    checkPermissionInWorkspace _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION kmSecret.workspaceUuid
    now <- liftIO getCurrentTimeInMicroseconds
    tenantUuid <- asks (.tenantUuid')
    let kmSecretUpdated = fromChangeDTO kmSecret reqDto now
    updateKnowledgeModelSecretByUuid kmSecretUpdated
    al_updateKnowledgeModelSecret kmSecret kmSecretUpdated
    return kmSecretUpdated

modifyKnowledgeModelSecretDto :: WizardRequestContextC s m => U.UUID -> KnowledgeModelSecretChangeDTO -> m KnowledgeModelSecretDTO
modifyKnowledgeModelSecretDto uuid reqDto = toDTO <$> modifyKnowledgeModelSecret uuid reqDto

deleteKnowledgeModelSecret :: WizardRequestContextC s m => U.UUID -> m ()
deleteKnowledgeModelSecret uuid = do
  checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
  kmSecret <- findKnowledgeModelSecretByUuid uuid
  checkPermissionInWorkspace _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION kmSecret.workspaceUuid
  void $ deleteKnowledgeModelSecretByUuid uuid
  al_deleteKnowledgeModelSecret kmSecret
