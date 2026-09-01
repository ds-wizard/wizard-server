module Shared.Database.DAO.Project.ProjectVersionDAO where

import Control.Monad (unless, void)
import Control.Monad.Reader (asks)
import Data.String (fromString)
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Project.Version.ProjectVersion ()
import Shared.Database.Mapping.Project.Version.ProjectVersionList ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.Version.ProjectVersion
import Shared.Model.Project.Version.ProjectVersionList
import Shared.Util.Logger
import Shared.Util.String

entityName = "project_version"

pageLabel = "projectVersions"

findProjectVersionsByProjectUuid :: WizardRequestContextC s m => U.UUID -> m [ProjectVersion]
findProjectVersionsByProjectUuid projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsByFn "*" entityName [tenantQueryUuid tenantUuid, ("project_uuid", U.toString projectUuid)]

findProjectVersionListByProjectUuidAndCreatedAt :: WizardRequestContextC s m => U.UUID -> Maybe UTCTime -> m [ProjectVersionList]
findProjectVersionListByProjectUuidAndCreatedAt projectUuid mCreatedAt = do
  tenantUuid <- asks (.tenantUuid')
  let (createdAtCondition, createdAtParams) =
        case mCreatedAt of
          Just createdAt -> ("AND v.created_at <= ?", [toField createdAt])
          Nothing -> ("", [])
  let sql =
        fromString $
          f''
            "SELECT v.uuid, \
            \       v.name, \
            \       v.description, \
            \       v.event_uuid, \
            \       v.created_at, \
            \       v.updated_at, \
            \       u.uuid, \
            \       u.first_name, \
            \       u.last_name, \
            \       u.email, \
            \       u.image_url, \
            \       u.affiliation \
            \FROM project_version v \
            \LEFT JOIN user_entity u ON u.uuid = v.created_by AND u.tenant_uuid = v.tenant_uuid \
            \WHERE v.tenant_uuid = ? AND v.project_uuid = ? ${createdAtCondition} \
            \ORDER BY v.created_at"
            [("createdAtCondition", createdAtCondition)]
  let params = [toField tenantUuid, toField projectUuid] ++ createdAtParams
  logInfoI _CMP_DATABASE sql
  let action conn = query conn (fromString sql) params
  runDB action

findProjectVersionByUuid :: WizardRequestContextC s m => U.UUID -> m ProjectVersion
findProjectVersionByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityWithFieldsByFn "*" False entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findProjectVersionByEventUuid' :: WizardRequestContextC s m => U.UUID -> U.UUID -> m (Maybe ProjectVersion)
findProjectVersionByEventUuid' projectUuid eventUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("project_uuid", U.toString projectUuid), ("event_uuid", U.toString eventUuid)]

insertProjectVersion :: WizardRequestContextC s m => ProjectVersion -> m Int64
insertProjectVersion = createInsertFn entityName

updateProjectVersionByUuid :: WizardRequestContextC s m => ProjectVersion -> m Int64
updateProjectVersionByUuid version = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE project_version SET uuid = ?, name = ?, description = ?, event_uuid = ?, project_uuid = ?, tenant_uuid = ?, created_by = ?, created_at = ?, updated_at = ? WHERE uuid = ? AND tenant_uuid = ?"
  let params = toRow version ++ [toField version.uuid, toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteProjectVersions :: WizardRequestContextC s m => m Int64
deleteProjectVersions = createDeleteEntitiesFn entityName

deleteProjectVersionsByUuids :: WizardRequestContextC s m => [U.UUID] -> m ()
deleteProjectVersionsByUuids versionUuids =
  unless
    (null versionUuids)
    (void $ createDeleteEntityWhereInFn entityName "uuid" (fmap U.toString versionUuids))

deleteProjectVersionByUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteProjectVersionByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
