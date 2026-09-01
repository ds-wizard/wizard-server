module Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateFormat ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateFormatSimple ()
import Shared.Model.Common.Sort
import Shared.Model.Context.RequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.Util.String

entityName = "document_template_format"

stepEntityName = "document_template_format_step"

findDocumentTemplateFormats :: RequestContextC s sc m => U.UUID -> m [DocumentTemplateFormat]
findDocumentTemplateFormats documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  formats <- createFindEntitiesBySortedFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)] [Sort "name" Ascending]
  traverse
    ( \format -> do
        steps <- findDocumentTemplateFormatSteps documentTemplateUuid format.uuid
        return $ format {steps = steps}
    )
    formats

findDocumentTemplateFormatSteps :: RequestContextC s sc m => U.UUID -> U.UUID -> m [DocumentTemplateFormatStep]
findDocumentTemplateFormatSteps documentTemplateUuid formatUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn stepEntityName [("tenant_uuid", U.toString tenantUuid), ("document_template_uuid", U.toString documentTemplateUuid), ("format_uuid", U.toString formatUuid)]

findDocumentTemplateFormatByDocumentTemplateIdAndUuid :: RequestContextC s sc m => U.UUID -> U.UUID -> m DocumentTemplateFormatSimple
findDocumentTemplateFormatByDocumentTemplateIdAndUuid documentTemplateUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityWithFieldsByFn "uuid, name, icon" False entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid), ("uuid", U.toString uuid)]

insertDocumentTemplateFormat :: RequestContextC s sc m => DocumentTemplateFormat -> m ()
insertDocumentTemplateFormat format = do
  createInsertFn entityName format
  traverse_ (createInsertFn stepEntityName) format.steps

insertOrUpdateDocumentTemplateFormat :: RequestContextC s sc m => DocumentTemplateFormat -> m Int64
insertOrUpdateDocumentTemplateFormat format = do
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
            [("format", entityName), ("step", stepEntityName)]
            ++ concatMap (const (f' "INSERT INTO %s VALUES (?, ?, ?, ?, ?, ?, ?, ?);" [stepEntityName])) format.steps
  let params =
        toRow format
          ++ toRow format
          ++ [toField format.tenantUuid, toField format.documentTemplateUuid, toField format.uuid]
          ++ concatMap toRow format.steps
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDocumentTemplateFormatsExcept :: RequestContextC s sc m => U.UUID -> [U.UUID] -> m Int64
deleteDocumentTemplateFormatsExcept documentTemplateUuid formatUuids = do
  let formatUuidsCondition =
        case formatUuids of
          [] -> ""
          _ -> f' "AND uuid NOT IN (%s)" [generateQuestionMarks formatUuids]
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "DELETE FROM %s \
            \WHERE tenant_uuid = ? AND document_template_uuid = ? %s"
            [entityName, formatUuidsCondition]
  let params = [U.toString tenantUuid, U.toString documentTemplateUuid] ++ fmap U.toString formatUuids
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteFormats :: RequestContextC s sc m => m Int64
deleteFormats = do
  createDeleteEntitiesFn entityName
