module Shared.Database.DAO.Document.DocumentDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Document.Document ()
import Shared.Database.Mapping.Document.DocumentList ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.Document.DocumentList
import Shared.Util.Logger
import Shared.Util.String

entityName = "document"

pageLabel = "documents"

findDocuments :: WizardRequestContextC s m => m [Document]
findDocuments = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findDocumentsFiltered :: WizardRequestContextC s m => [(String, String)] -> m [Document]
findDocumentsFiltered = createFindEntitiesByFn entityName

findDocumentsForCurrentTenantFiltered :: WizardRequestContextC s m => [(String, String)] -> m [Document]
findDocumentsForCurrentTenantFiltered params = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : params)

findDocumentsPage :: WizardRequestContextC s m => Maybe U.UUID -> Maybe String -> Maybe U.UUID -> Maybe String -> Pageable -> [Sort] -> m (Page DocumentList)
findDocumentsPage mProjectUuid mProjectName mDocumentTemplateUuid mQuery pageable sort = do
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    let (projectSelect, projectSelectParams, projectJoin, projectCondition, projectParam) =
          case (mProjectUuid, mProjectName) of
            (Just projectUuid, Just projectName) -> ("?, ", [projectName], "", "AND doc.project_uuid = ?", [U.toString projectUuid])
            (Just projectUuid, Nothing) -> ("project.name, ", [], "LEFT JOIN project ON project.uuid = doc.project_uuid", "AND doc.project_uuid = ?", [U.toString projectUuid])
            _ -> ("project.name, ", [], "LEFT JOIN project ON project.uuid = doc.project_uuid", "", [])
    let (documentTemplateUuidCondition, documentTemplateUuidParam) =
          case mDocumentTemplateUuid of
            Just documentTemplateUuid -> (" AND doc.document_template_uuid = ? ", [U.toString documentTemplateUuid])
            Nothing -> ("", [])
    let condition = "WHERE doc.tenant_uuid = ? AND doc.name ~* ? AND doc.durability = 'PersistentDocumentDurability' " ++ projectCondition ++ documentTemplateUuidCondition
    let baseParams = [U.toString tenantUuid, regexM mQuery] ++ projectParam ++ documentTemplateUuidParam
    let params = projectSelectParams ++ baseParams
    -- 2. Get total count
    count <- createCountByFn "document doc" condition baseParams
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT doc.uuid, \
              \       doc.name, \
              \       doc.state, \
              \       doc.project_uuid, \
              \       ${projectSelect} \
              \       doc.project_event_uuid, \
              \       project_version.name, \
              \       doc_tml.uuid, \
              \       doc_tml.name, \
              \       doc_tml.organization_id, \
              \       doc_tml.template_id, \
              \       doc_tml.version, \
              \       dt_format.uuid, \
              \       dt_format.name, \
              \       dt_format.icon, \
              \       doc.language, \
              \       doc.file_size, \
              \       doc.worker_log, \
              \       doc.created_by, \
              \       doc.created_at \
              \FROM document doc \
              \${projectJoin} \
              \LEFT JOIN document_template doc_tml ON doc_tml.uuid = doc.document_template_uuid AND doc_tml.tenant_uuid = doc.tenant_uuid \
              \LEFT JOIN document_template_format dt_format ON dt_format.tenant_uuid = doc.tenant_uuid AND dt_format.document_template_uuid = doc.document_template_uuid AND dt_format.uuid = doc.format_uuid \
              \LEFT JOIN project_version ON project_version.event_uuid = doc.project_event_uuid AND project_version.tenant_uuid = doc.tenant_uuid \
              \${condition} \
              \${sort} \
              \OFFSET ${offset} \
              \LIMIT ${limit}"
              [ ("projectSelect", projectSelect)
              , ("projectJoin", projectJoin)
              , ("condition", condition)
              , ("sort", mapSortWithPrefix "doc" sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
              ]
    logQuery sql params
    let action conn = query conn sql params
    entities <- runDB action
    -- 4. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page pageLabel metadata entities

findDocumentsByDocumentTemplateUuid :: WizardRequestContextC s m => U.UUID -> m [Document]
findDocumentsByDocumentTemplateUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid), ("durability", "PersistentDocumentDurability")]

findDocumentByUuid :: WizardRequestContextC s m => U.UUID -> m Document
findDocumentByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

countDocuments :: WizardRequestContextC s m => m Int
countDocuments = do
  tenantUuid <- asks (.tenantUuid')
  countDocumentsWithTenant tenantUuid

countDocumentsWithTenant :: WizardRequestContextC s m => U.UUID -> m Int
countDocumentsWithTenant tenantUuid = createCountByFn entityName tenantCondition [U.toString tenantUuid]

sumDocumentFileSize :: WizardRequestContextC s m => m Int64
sumDocumentFileSize = do
  tenantUuid <- asks (.tenantUuid')
  sumDocumentFileSizeWithTenant tenantUuid

sumDocumentFileSizeWithTenant :: WizardRequestContextC s m => U.UUID -> m Int64
sumDocumentFileSizeWithTenant tenantUuid = createSumByFn entityName "file_size" tenantCondition [U.toString tenantUuid]

insertDocument :: WizardRequestContextC s m => Document -> m Int64
insertDocument = createInsertFn entityName

deleteDocuments :: WizardRequestContextC s m => m Int64
deleteDocuments = createDeleteEntitiesFn entityName

deleteDocumentsFiltered :: WizardRequestContextC s m => [(String, String)] -> m Int64
deleteDocumentsFiltered params = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntitiesByFn entityName (tenantQueryUuid tenantUuid : params)

deleteDocumentByUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteDocumentByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

deleteDocumentByUuidAndTenantUuid :: WizardRequestContextC s m => U.UUID -> U.UUID -> m Int64
deleteDocumentByUuidAndTenantUuid uuid tenantUuid = createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

deleteTemporalDocumentsByProjectUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteTemporalDocumentsByProjectUuid projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  deleteDocumentsFiltered
    [tenantQueryUuid tenantUuid, ("project_uuid", U.toString projectUuid), ("durability", "TemporallyDocumentDurability")]

deleteTemporalDocumentsByDocumentTemplateUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteTemporalDocumentsByDocumentTemplateUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  deleteDocumentsFiltered [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid), ("durability", "TemporallyDocumentDurability")]

deleteTemporalDocumentsByAssetUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteTemporalDocumentsByAssetUuid = deleteTemporalDocumentsByTableAndUuid "document_template_asset"

deleteTemporalDocumentsByFileUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteTemporalDocumentsByFileUuid = deleteTemporalDocumentsByTableAndUuid "document_template_file"

-- --------------------------------
-- PRIVATE
-- --------------------------------
deleteTemporalDocumentsByTableAndUuid :: WizardRequestContextC s m => String -> U.UUID -> m Int64
deleteTemporalDocumentsByTableAndUuid joinTableName entityUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "DELETE \
            \FROM document \
            \WHERE tenant_uuid = ? AND uuid IN ( \
            \    SELECT d.uuid \
            \    FROM %s join_table \
            \             JOIN document d ON join_table.document_template_uuid = d.document_template_uuid \
            \    WHERE join_table.uuid = '%s' \
            \      AND d.durability = 'TemporallyDocumentDurability' \
            \)"
            [joinTableName, U.toString entityUuid]
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
