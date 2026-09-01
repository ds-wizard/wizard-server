module Shared.Database.DAO.Project.ProjectFileDAO where

import Control.Monad.Reader (asks)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Project.File.ProjectFile ()
import Shared.Database.Mapping.Project.File.ProjectFileList ()
import Shared.Database.Mapping.Project.File.ProjectFileSimple ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.File.ProjectFile
import Shared.Model.Project.File.ProjectFileList
import Shared.Model.Project.File.ProjectFileSimple
import Shared.Util.String

entityName = "project_file"

pageLabel = "projectFiles"

findProjectFilesPage :: WizardRequestContextC s m => Maybe String -> Maybe U.UUID -> Pageable -> [Sort] -> m (Page ProjectFileList)
findProjectFilesPage mQuery mProjectUuid pageable sort = do
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    let (queryCondition, queryParam) =
          case mQuery of
            Nothing -> ("", [])
            Just query -> (" AND file_name ~* ?", [query])
    let (projectUuidCondition, projectUuidParam) =
          case mProjectUuid of
            Nothing -> ("", [])
            Just projectUuid -> (" AND project_uuid = ?", [U.toString projectUuid])
    let condition =
          f''
            "WHERE file.tenant_uuid = ? ${queryCondition} ${projectUuidCondition}"
            [ ("queryCondition", queryCondition)
            , ("projectUuidCondition", projectUuidCondition)
            ]
    let conditionParams =
          [U.toString tenantUuid]
            ++ queryParam
            ++ projectUuidParam
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    count <- createCountByFn "project_file file" condition conditionParams
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT file.uuid, \
              \       file.file_name, \
              \       file.content_type, \
              \       file.file_size, \
              \       file.created_at, \
              \       project.uuid, \
              \       project.name, \
              \       created_by.uuid, \
              \       created_by.first_name, \
              \       created_by.last_name, \
              \       created_by.email, \
              \       created_by.image_url, \
              \       created_by.affiliation \
              \FROM project_file file \
              \LEFT JOIN user_entity created_by ON created_by.uuid = file.created_by AND created_by.tenant_uuid = file.tenant_uuid \
              \LEFT JOIN project ON project.uuid = file.project_uuid AND project.tenant_uuid = file.tenant_uuid \
              \${condition} \
              \${sort} \
              \OFFSET ${offset} \
              \LIMIT ${limit}"
              [ ("condition", condition)
              , ("sort", mapSort sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
              ]
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

findProjectFilesByProject :: WizardRequestContextC s m => U.UUID -> m [ProjectFile]
findProjectFilesByProject projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsByFn "*" entityName [tenantQueryUuid tenantUuid, ("project_uuid", U.toString projectUuid)]

findProjectFilesSimpleByProject :: WizardRequestContextC s m => U.UUID -> m [ProjectFileSimple]
findProjectFilesSimpleByProject projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsByFn "uuid, file_name, content_type, file_size" entityName [tenantQueryUuid tenantUuid, ("project_uuid", U.toString projectUuid)]

findProjectFileByUuid :: WizardRequestContextC s m => U.UUID -> m ProjectFile
findProjectFileByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

sumProjectFileSize :: WizardRequestContextC s m => m Int64
sumProjectFileSize = do
  tenantUuid <- asks (.tenantUuid')
  sumProjectFileSizeWithTenant tenantUuid

sumProjectFileSizeWithTenant :: WizardRequestContextC s m => U.UUID -> m Int64
sumProjectFileSizeWithTenant tenantUuid = createSumByFn entityName "file_size" tenantCondition [U.toString tenantUuid]

insertProjectFile :: WizardRequestContextC s m => ProjectFile -> m Int64
insertProjectFile = createInsertFn entityName

deleteProjectFiles :: WizardRequestContextC s m => m Int64
deleteProjectFiles = createDeleteEntitiesFn entityName

deleteProjectFilesNewerThen :: WizardRequestContextC s m => U.UUID -> UTCTime -> m Int64
deleteProjectFilesNewerThen projectUuid timestamp = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "DELETE FROM project_file \
          \WHERE tenant_uuid = ? \
          \  AND project_uuid = ? \
          \  AND created_at > ?"
  let params = [U.toString tenantUuid, U.toString projectUuid, show timestamp]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteProjectFileByUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteProjectFileByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
