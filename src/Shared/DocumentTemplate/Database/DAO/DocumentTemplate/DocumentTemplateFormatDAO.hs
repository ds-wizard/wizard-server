module Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFormat ()
import Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFormatSimple ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple

findDocumentTemplateFormats :: AppContextC s sc m => U.UUID -> m [DocumentTemplateFormat]
findDocumentTemplateFormats documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName "document_template_format"
  formats <- createFindEntitiesBySortedFn table [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)] [Sort "name" Ascending]
  traverse
    ( \format -> do
        steps <- findDocumentTemplateFormatSteps documentTemplateUuid format.uuid
        return $ format {steps = steps}
    )
    formats

findDocumentTemplateFormatSteps :: AppContextC s sc m => U.UUID -> U.UUID -> m [DocumentTemplateFormatStep]
findDocumentTemplateFormatSteps documentTemplateUuid formatUuid = do
  tenantUuid <- asks (.tenantUuid')
  stepTable <- tableName "document_template_format_step"
  createFindEntitiesByFn stepTable [("tenant_uuid", U.toString tenantUuid), ("document_template_uuid", U.toString documentTemplateUuid), ("format_uuid", U.toString formatUuid)]

findDocumentTemplateFormatByDocumentTemplateIdAndUuid :: AppContextC s sc m => U.UUID -> U.UUID -> m DocumentTemplateFormatSimple
findDocumentTemplateFormatByDocumentTemplateIdAndUuid documentTemplateUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName "document_template_format"
  createFindEntityWithFieldsByFn "uuid, name, icon" False table [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid), ("uuid", U.toString uuid)]

insertDocumentTemplateFormat :: AppContextC s sc m => DocumentTemplateFormat -> m ()
insertDocumentTemplateFormat format = do
  table <- tableName "document_template_format"
  stepTable <- tableName "document_template_format_step"
  createInsertFn table format
  traverse_ (createInsertFn stepTable) format.steps

insertOrUpdateDocumentTemplateFormat :: AppContextC s sc m => DocumentTemplateFormat -> m Int64
insertOrUpdateDocumentTemplateFormat format = do
  table <- tableName "document_template_format"
  stepTable <- tableName "document_template_format_step"
  let sql =
        fromString $
          f''
            "INSERT INTO ${format} \
            \VALUES (?, ?, ?, ?, ?, ?, ?) \
            \ON CONFLICT (document_template_uuid, uuid) DO UPDATE SET document_template_uuid = ?, \
            \                                                         uuid                   = ?, \
            \                                                         name                   = ?, \
            \                                                         icon                   = ?, \
            \                                                         tenant_uuid            = ?, \
            \                                                         created_at             = ?, \
            \                                                         updated_at             = ?; \
            \DELETE FROM ${step} WHERE tenant_uuid = ? AND document_template_uuid = ? AND format_uuid = ?;"
            [("format", table), ("step", stepTable)]
            ++ concatMap (const (f' "INSERT INTO %s VALUES (?, ?, ?, ?, ?, ?, ?, ?);" [stepTable])) format.steps
  let params =
        toRow format
          ++ toRow format
          ++ [toField format.tenantUuid, toField format.documentTemplateUuid, toField format.uuid]
          ++ concatMap toRow format.steps
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDocumentTemplateFormatsExcept :: AppContextC s sc m => U.UUID -> [U.UUID] -> m Int64
deleteDocumentTemplateFormatsExcept documentTemplateUuid formatUuids = do
  let formatUuidsCondition =
        case formatUuids of
          [] -> ""
          _ -> f' "AND uuid NOT IN (%s)" [generateQuestionMarks formatUuids]
  tenantUuid <- asks (.tenantUuid')
  table <- tableName "document_template_format"
  let sql =
        fromString $
          f'
            "DELETE FROM %s \
            \WHERE tenant_uuid = ? AND document_template_uuid = ? %s"
            [table, formatUuidsCondition]
  let params = [U.toString tenantUuid, U.toString documentTemplateUuid] ++ fmap U.toString formatUuids
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteFormats :: AppContextC s sc m => m Int64
deleteFormats = do
  table <- tableName "document_template_format"
  createDeleteEntitiesFn table
