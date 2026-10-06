module Shared.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.KnowledgeModel.Secret.KnowledgeModelSecret ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.KnowledgeModelSecret
import Shared.Model.User.RolePermission
import Shared.Util.String

entityName = "knowledge_model_secret"

findKnowledgeModelSecrets :: WizardRequestContextC s m => m [KnowledgeModelSecret]
findKnowledgeModelSecrets = do
  tenantUuid <- asks (.tenantUuid')
  workspaceCondition <- tenantOrWorkspaceCondition (Just _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION) "workspace_uuid"
  let sql = fromString $ f' "SELECT * FROM %s WHERE tenant_uuid = ? %s ORDER BY workspace_uuid NULLS FIRST, name" [entityName, workspaceCondition]
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findKnowledgeModelSecretsForWorkspace :: WizardRequestContextC s m => Maybe U.UUID -> m [KnowledgeModelSecret]
findKnowledgeModelSecretsForWorkspace mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ f' "SELECT * FROM %s WHERE tenant_uuid = ? AND %s ORDER BY workspace_uuid IS NOT NULL, name" [entityName, workspaceVisibleCondition]
  let params = [toField tenantUuid, toField mWorkspaceUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findKnowledgeModelSecretByUuid :: WizardRequestContextC s m => U.UUID -> m KnowledgeModelSecret
findKnowledgeModelSecretByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertKnowledgeModelSecret :: WizardRequestContextC s m => KnowledgeModelSecret -> m Int64
insertKnowledgeModelSecret = createInsertFn entityName

updateKnowledgeModelSecretByUuid :: WizardRequestContextC s m => KnowledgeModelSecret -> m Int64
updateKnowledgeModelSecretByUuid kmSecret = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "UPDATE knowledge_model_secret SET uuid = ?, name = ?, value = ?, tenant_uuid = ?, created_at = ?, updated_at = ?, workspace_uuid = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow kmSecret ++ [toField tenantUuid, toField kmSecret.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteKnowledgeModelSecrets :: WizardRequestContextC s m => m Int64
deleteKnowledgeModelSecrets = createDeleteEntitiesFn entityName

deleteKnowledgeModelSecretByUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteKnowledgeModelSecretByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
