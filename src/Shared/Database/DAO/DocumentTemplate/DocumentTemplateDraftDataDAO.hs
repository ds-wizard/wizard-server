module Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateDraftData ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplateDraftData

entityName = "document_template_draft_data"

findDraftDataByUuid :: WizardRequestContextC s m => U.UUID -> m DocumentTemplateDraftData
findDraftDataByUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)]

insertDraftData :: WizardRequestContextC s m => DocumentTemplateDraftData -> m Int64
insertDraftData = createInsertFn entityName

updateDraftDataById :: WizardRequestContextC s m => DocumentTemplateDraftData -> m Int64
updateDraftDataById draftData = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "UPDATE document_template_draft_data SET document_template_uuid = ?, project_uuid = ?, format_uuid = ?, tenant_uuid = ?, created_at = ?, updated_at = ?, knowledge_model_editor_uuid = ? WHERE tenant_uuid = ? AND document_template_uuid = ?"
  let params = toRow draftData ++ [toField draftData.tenantUuid, toField draftData.documentTemplateUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDraftData :: WizardRequestContextC s m => m Int64
deleteDraftData = createDeleteEntitiesFn entityName

deleteDraftDataByDocumentTemplateUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteDraftDataByDocumentTemplateUuid dtUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString dtUuid)]
