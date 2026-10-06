module Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO (deleteDraftData)
import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateDraftList ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateDraftList
import Shared.Model.User.RolePermission
import Shared.Util.Logger

entityName = "document_template"

pageLabel = "documentTemplateDrafts"

findDrafts :: WizardRequestContextC s m => m [DocumentTemplate]
findDrafts = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("phase", "DraftDocumentTemplatePhase")]

findDraftsPage :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page DocumentTemplateDraftList)
findDraftsPage mQuery pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    workspaceCondition <- tenantOrWorkspaceCondition (Just _DOCUMENT_TEMPLATE_EDITORS_USE_ROLE_PERMISSION) "workspace_uuid"
    let condition = "WHERE phase = 'DraftDocumentTemplatePhase' AND (name ~* ? OR id ~* ?) AND tenant_uuid = ?" ++ workspaceCondition
    let conditionParams = [regexM mQuery, regexM mQuery, U.toString tenantUuid]
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    count <- createCountByFn entityName condition conditionParams
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "SELECT uuid, \
              \       name, \
              \       id, \
              \       version, \
              \       description, \
              \       created_at, \
              \       updated_at, \
              \       workspace_uuid \
              \FROM document_template \
              \WHERE phase = 'DraftDocumentTemplatePhase' AND (name ~* ? OR id ~* ?) AND tenant_uuid = ? %s \
              \%s OFFSET %s LIMIT %s"
              [workspaceCondition, mapSort sort, show skip, show sizeI]
    logQuery sql conditionParams
    let action conn = query conn sql conditionParams
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

findDraftByUuid :: WizardRequestContextC s m => U.UUID -> m DocumentTemplate
findDraftByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid), ("phase", "DraftDocumentTemplatePhase")]

countDraftsGroupedById :: WizardRequestContextC s m => m Int
countDraftsGroupedById = do
  tenantUuid <- asks (.tenantUuid')
  countDraftsGroupedByIdWithTenant tenantUuid

countDraftsGroupedByIdWithTenant :: WizardRequestContextC s m => U.UUID -> m Int
countDraftsGroupedByIdWithTenant tenantUuid = do
  let sql =
        "SELECT COUNT(*) \
        \FROM (SELECT 1 \
        \      FROM document_template \
        \      WHERE tenant_uuid = ? AND phase = 'DraftDocumentTemplatePhase' \
        \      GROUP BY id) nested;"
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

moveFolder :: WizardRequestContextC s m => U.UUID -> String -> String -> m Int64
moveFolder documentTemplateUuid currentFolder newFolder = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "UPDATE document_template_asset \
          \SET file_name = concat(?, substr(file_name, length(?) + 1)) \
          \WHERE tenant_uuid = ?  \
          \  AND document_template_uuid = ? \
          \  AND starts_with(file_name, ?); \
          \ \
          \UPDATE document_template_file \
          \SET file_name = concat(?, substr(file_name, length(?) + 1)) \
          \WHERE tenant_uuid = ?  \
          \  AND document_template_uuid = ? \
          \  AND starts_with(file_name, ?);"
  let paramsForOneUpdate =
        [ toField newFolder
        , toField currentFolder
        , toField tenantUuid
        , toField documentTemplateUuid
        , toField currentFolder
        ]
  let params = paramsForOneUpdate ++ paramsForOneUpdate
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteFolder :: WizardRequestContextC s m => U.UUID -> String -> m Int64
deleteFolder documentTemplateUuid path = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "DELETE FROM document_template_asset \
          \WHERE tenant_uuid = ?  \
          \  AND document_template_uuid = ? \
          \  AND starts_with(file_name, ?); \
          \ \
          \DELETE FROM document_template_file \
          \WHERE tenant_uuid = ?  \
          \  AND document_template_uuid = ? \
          \  AND starts_with(file_name, ?);"
  let paramsForOneUpdate =
        [ toField tenantUuid
        , toField documentTemplateUuid
        , toField path
        ]
  let params = paramsForOneUpdate ++ paramsForOneUpdate
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDrafts :: WizardRequestContextC s m => m Int64
deleteDrafts = do
  tenantUuid <- asks (.tenantUuid')
  deleteDraftData
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("phase", "DraftDocumentTemplatePhase")]

deleteDraftByUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteDraftByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid), ("phase", "DraftDocumentTemplatePhase")]
