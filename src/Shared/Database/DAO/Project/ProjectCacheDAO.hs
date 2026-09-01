module Shared.Database.DAO.Project.ProjectCacheDAO where

import Control.Monad.Reader (asks)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Shared.Api.Resource.Project.Detail.ProjectDetailReportDTO
import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Project.Cache.ProjectCache ()
import Shared.Database.Mapping.Project.Cache.ProjectCacheSource ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.Cache.ProjectCache
import Shared.Model.Project.Cache.ProjectCacheSource
import Shared.Model.Project.Version.ProjectVersionList

entityName = "project_cache"

findOutdatedProjectCacheSources :: WizardRequestContextC s m => m [ProjectCacheSource]
findOutdatedProjectCacheSources = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT uuid, questionnaire_source_updated_at, versions_source_updated_at \
          \FROM (SELECT project.uuid, \
          \             GREATEST(project.updated_at, \
          \                      (SELECT max(created_at) FROM project_event WHERE project_uuid = project.uuid), \
          \                      (SELECT max(created_at) FROM project_file WHERE project_uuid = project.uuid), \
          \                      (SELECT max(updated_at) FROM project_comment_thread WHERE project_uuid = project.uuid), \
          \                      (SELECT max(project_comment.updated_at) \
          \                       FROM project_comment \
          \                       JOIN project_comment_thread ON project_comment_thread.uuid = project_comment.comment_thread_uuid \
          \                       WHERE project_comment_thread.project_uuid = project.uuid), \
          \                      (SELECT max(updated_at) \
          \                       FROM knowledge_model_locale \
          \                       WHERE knowledge_model_package_uuid = project.knowledge_model_package_uuid AND code = project.language)) AS questionnaire_source_updated_at, \
          \             GREATEST(project.updated_at, \
          \                      (SELECT max(updated_at) FROM project_version WHERE project_uuid = project.uuid)) AS versions_source_updated_at, \
          \             project_cache.questionnaire_source_updated_at AS cached_questionnaire_source_updated_at, \
          \             project_cache.versions_source_updated_at AS cached_versions_source_updated_at \
          \      FROM project \
          \      LEFT JOIN project_cache ON project_cache.project_uuid = project.uuid \
          \      WHERE project.tenant_uuid = ?) source \
          \WHERE cached_questionnaire_source_updated_at IS NULL \
          \   OR questionnaire_source_updated_at > cached_questionnaire_source_updated_at \
          \   OR versions_source_updated_at > cached_versions_source_updated_at"
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findProjectCaches :: WizardRequestContextC s m => m [ProjectCache]
findProjectCaches = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findProjectCacheWatermarksByProjectUuid' :: WizardRequestContextC s m => U.UUID -> m (Maybe (UTCTime, UTCTime))
findProjectCacheWatermarksByProjectUuid' projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "SELECT questionnaire_source_updated_at, versions_source_updated_at FROM project_cache WHERE tenant_uuid = ? AND project_uuid = ? FOR UPDATE"
  let params = [toField tenantUuid, toField projectUuid]
  logQuery sql params
  let action conn = query conn sql params
  runOneEntityDB' entityName action [tenantQueryUuid tenantUuid, ("project_uuid", U.toString projectUuid)]

findProjectCacheByProjectUuid' :: WizardRequestContextC s m => U.UUID -> m (Maybe ProjectCache)
findProjectCacheByProjectUuid' projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("project_uuid", U.toString projectUuid)]

insertProjectCache :: WizardRequestContextC s m => ProjectCache -> m Int64
insertProjectCache cache = do
  let sql =
        fromString
          "INSERT INTO project_cache VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
          \ON CONFLICT (project_uuid) DO UPDATE SET questionnaire = EXCLUDED.questionnaire, \
          \                                        report = EXCLUDED.report, \
          \                                        versions = EXCLUDED.versions, \
          \                                        questionnaire_source_updated_at = EXCLUDED.questionnaire_source_updated_at, \
          \                                        versions_source_updated_at = EXCLUDED.versions_source_updated_at, \
          \                                        updated_at = EXCLUDED.updated_at"
  let params = toRow cache
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateProjectCacheQuestionnaireByProjectUuid :: WizardRequestContextC s m => U.UUID -> ProjectDetailQuestionnaireDTO -> ProjectDetailReportDTO -> UTCTime -> UTCTime -> m Int64
updateProjectCacheQuestionnaireByProjectUuid projectUuid questionnaire report sourceUpdatedAt now = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE project_cache SET questionnaire = ?, report = ?, questionnaire_source_updated_at = ?, updated_at = ? WHERE tenant_uuid = ? AND project_uuid = ?"
  let params = [toJSONField questionnaire, toJSONField report, toField sourceUpdatedAt, toField now, toField tenantUuid, toField projectUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateProjectCacheVersionsByProjectUuid :: WizardRequestContextC s m => U.UUID -> [ProjectVersionList] -> UTCTime -> UTCTime -> m Int64
updateProjectCacheVersionsByProjectUuid projectUuid versions sourceUpdatedAt now = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE project_cache SET versions = ?, versions_source_updated_at = ?, updated_at = ? WHERE tenant_uuid = ? AND project_uuid = ?"
  let params = [toJSONField versions, toField sourceUpdatedAt, toField now, toField tenantUuid, toField projectUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteProjectCaches :: WizardRequestContextC s m => m Int64
deleteProjectCaches = createDeleteEntitiesFn entityName

deleteProjectCacheByProjectUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteProjectCacheByProjectUuid projectUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("project_uuid", U.toString projectUuid)]

deleteProjectCachesByUserGroupUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteProjectCachesByUserGroupUuid userGroupUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "DELETE FROM project_cache \
          \WHERE tenant_uuid = ? \
          \  AND project_uuid IN (SELECT project_uuid FROM project_perm_group WHERE user_group_uuid = ?)"
  let params = [toField tenantUuid, toField userGroupUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteProjectCachesByKnowledgeModelLocale :: WizardRequestContextC s m => U.UUID -> String -> m Int64
deleteProjectCachesByKnowledgeModelLocale pkgUuid code = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "DELETE FROM project_cache \
          \WHERE tenant_uuid = ? \
          \  AND project_uuid IN (SELECT uuid FROM project WHERE knowledge_model_package_uuid = ? AND language = ?)"
  let params = [toField tenantUuid, toField pkgUuid, toField code]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
