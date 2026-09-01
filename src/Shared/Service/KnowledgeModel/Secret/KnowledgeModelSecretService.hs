module Shared.Service.KnowledgeModel.Secret.KnowledgeModelSecretService where

import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Shared.Database.DAO.WizardCommon
import Shared.Model.Context.AclContext
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Service.KnowledgeModel.Secret.KnowledgeModelSecretMapper
import Shared.Util.Uuid

getKnowledgeModelSecrets :: WizardRequestContextC s m => m [KnowledgeModelSecret]
getKnowledgeModelSecrets = do
  checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
  findKnowledgeModelSecrets

createKnowledgeModelSecret :: WizardRequestContextC s m => KnowledgeModelSecretChangeDTO -> m KnowledgeModelSecret
createKnowledgeModelSecret reqDto =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    tenantUuid <- asks (.tenantUuid')
    let kmSecret = fromCreateDTO reqDto uuid tenantUuid now
    insertKnowledgeModelSecret kmSecret
    return kmSecret

modifyKnowledgeModelSecret :: WizardRequestContextC s m => U.UUID -> KnowledgeModelSecretChangeDTO -> m KnowledgeModelSecret
modifyKnowledgeModelSecret uuid reqDto =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
    kmSecret <- findKnowledgeModelSecretByUuid uuid
    now <- liftIO getCurrentTime
    tenantUuid <- asks (.tenantUuid')
    let kmSecretUpdated = fromChangeDTO kmSecret reqDto now
    updateKnowledgeModelSecretByUuid kmSecretUpdated
    return kmSecretUpdated

deleteKnowledgeModelSecret :: WizardRequestContextC s m => U.UUID -> m ()
deleteKnowledgeModelSecret uuid = do
  checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
  _ <- findKnowledgeModelSecretByUuid uuid
  void $ deleteKnowledgeModelSecretByUuid uuid
