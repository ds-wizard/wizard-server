module Shared.Database.DAO.Package.KnowledgeModelPackageDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackage ()
import Shared.Model.Context.RequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Util.String (f', f'')

entityName = "knowledge_model_package"

findPackages :: RequestContextC s sc m => m [KnowledgeModelPackage]
findPackages = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findPackagesFiltered :: RequestContextC s sc m => [(String, String)] -> m [KnowledgeModelPackage]
findPackagesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

findPackagesByOrganizationIdAndKmId :: RequestContextC s sc m => String -> String -> Maybe U.UUID -> m [KnowledgeModelPackage]
findPackagesByOrganizationIdAndKmId organizationId kmId mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ f' "SELECT * FROM %s WHERE tenant_uuid = ? AND organization_id = ? AND km_id = ? AND %s" [entityName, workspacePlaneCondition]
  let params = [toField tenantUuid, toField organizationId, toField kmId, toField mWorkspaceUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findPackagesByOrganizationIdAndKmIdInPlane :: RequestContextC s sc m => String -> String -> Maybe U.UUID -> m [KnowledgeModelPackage]
findPackagesByOrganizationIdAndKmIdInPlane organizationId kmId mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ f' "SELECT * FROM %s WHERE tenant_uuid = ? AND organization_id = ? AND km_id = ? AND %s" [entityName, workspaceExactCondition]
  let params = [toField tenantUuid, toField organizationId, toField kmId, toField mWorkspaceUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findPackagesByPreviousPackageUuid :: RequestContextC s sc m => U.UUID -> m [KnowledgeModelPackage]
findPackagesByPreviousPackageUuid previousPackageUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("previous_package_uuid", U.toString previousPackageUuid)]

findPackagesByForkOfPackageId :: RequestContextC s sc m => String -> m [KnowledgeModelPackage]
findPackagesByForkOfPackageId forkOfPackageId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("fork_of_package_id", forkOfPackageId)]

findPackagesByUnsupportedMetamodelVersion :: RequestContextC s sc m => Int -> m [KnowledgeModelPackage]
findPackagesByUnsupportedMetamodelVersion metamodelVersion = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ f' "SELECT * FROM %s WHERE metamodel_version != ? AND tenant_uuid = ?" [entityName]
  let params = [toField metamodelVersion, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findSeriesOfPackagesRecursiveByUuid :: (RequestContextC s sc m, FromRow packageWithEvents) => U.UUID -> m [packageWithEvents]
findSeriesOfPackagesRecursiveByUuid pkgUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f''
            "WITH RECURSIVE recursive AS ( \
            \  SELECT *, 1 as level \
            \  FROM ${package} \
            \  WHERE tenant_uuid = ? AND uuid = ? \
            \  UNION ALL \
            \  SELECT pkg.*, level + 1 as level \
            \  FROM ${package} pkg \
            \  INNER JOIN recursive r ON pkg.uuid = r.previous_package_uuid AND pkg.tenant_uuid = ? \
            \) \
            \SELECT concat(organization_id, ':', km_id, ':', version) AS id, \
            \       name, \
            \       organization_id, \
            \       km_id, \
            \       version, \
            \       phase, \
            \       metamodel_version, \
            \       description, \
            \       readme, \
            \       license, \
            \       (SELECT concat(pp.organization_id, ':', pp.km_id, ':', pp.version) FROM ${package} pp WHERE pp.tenant_uuid = recursive.tenant_uuid AND pp.uuid = recursive.previous_package_uuid) AS previous_package_uuid, \
            \       fork_of_package_id, \
            \       merge_checkpoint_package_id, \
            \       (SELECT coalesce(jsonb_agg(jsonb_build_object( \
            \                        'uuid', pkg_event.uuid, \
            \                        'parentUuid', pkg_event.parent_uuid, \
            \                        'entityUuid', pkg_event.entity_uuid, \
            \                        'content', pkg_event.content, \
            \                        'createdAt', to_char(pkg_event.created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') \
            \               ) \
            \           ), '[]'::jsonb) \
            \               FROM (SELECT * \
            \                     FROM ${packageEvent} \
            \                     WHERE ${packageEvent}.tenant_uuid = recursive.tenant_uuid \
            \                       AND ${packageEvent}.package_uuid = recursive.uuid \
            \                     ORDER BY ${packageEvent}.created_at) pkg_event), \
            \       non_editable, \
            \       created_at, \
            \       language \
            \FROM recursive \
            \ORDER BY level DESC;"
            [("package", entityName), ("packageEvent", "knowledge_model_package_event")]
  let params = [U.toString tenantUuid, U.toString pkgUuid, U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findUsablePackagesForDocumentTemplate :: (RequestContextC s sc m, FromRow KnowledgeModelPackage) => U.UUID -> m [KnowledgeModelPackage]
findUsablePackagesForDocumentTemplate dtUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f''
            "WITH expanded_rules AS (SELECT tenant_uuid, \
            \                               workspace_uuid, \
            \                               jsonb_array_elements(allowed_packages) AS rule \
            \                        FROM ${documentTemplate} \
            \                        WHERE uuid = ? \
            \                          AND tenant_uuid = ?) \
            \SELECT DISTINCT ON (kmp.organization_id, kmp.km_id) kmp.* \
            \FROM ${package} kmp \
            \     JOIN expanded_rules er ON \
            \    kmp.tenant_uuid = er.tenant_uuid \
            \        AND (kmp.workspace_uuid IS NULL OR kmp.workspace_uuid = er.workspace_uuid) \
            \        AND (er.rule ->> 'orgId' IS NULL OR kmp.organization_id = er.rule ->> 'orgId') \
            \        AND (er.rule ->> 'kmId' IS NULL OR kmp.km_id = er.rule ->> 'kmId') \
            \        AND (er.rule ->> 'minVersion' IS NULL OR \
            \             string_to_array(kmp.version, '.')::int[] >= string_to_array(er.rule ->> 'minVersion', '.')::int[]) \
            \        AND (er.rule ->> 'maxVersion' IS NULL OR \
            \             string_to_array(kmp.version, '.')::int[] <= string_to_array(er.rule ->> 'maxVersion', '.')::int[]) \
            \ORDER BY kmp.organization_id, \
            \         kmp.km_id, \
            \         string_to_array(kmp.version, '.')::int[] DESC, \
            \         kmp.workspace_uuid IS NOT NULL DESC;"
            [("package", entityName), ("documentTemplate", "document_template")]
  let params = [U.toString dtUuid, U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findVersionsForPackage :: RequestContextC s sc m => String -> String -> Maybe U.UUID -> m [String]
findVersionsForPackage orgId kmId mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ f' "SELECT version FROM %s WHERE tenant_uuid = ? and organization_id = ? and km_id = ? AND %s" [entityName, workspacePlaneCondition]
  let params = [toField tenantUuid, toField orgId, toField kmId, toField mWorkspaceUuid]
  logQuery sql params
  let action conn = query conn sql params
  versions <- runDB action
  return . fmap fromOnly $ versions

findPackageByUuid :: RequestContextC s sc m => U.UUID -> m KnowledgeModelPackage
findPackageByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findPackageByUuid' :: RequestContextC s sc m => U.UUID -> m (Maybe KnowledgeModelPackage)
findPackageByUuid' uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findPackageByCoordinate :: RequestContextC s sc m => Coordinate -> Maybe U.UUID -> m KnowledgeModelPackage
findPackageByCoordinate coordinate mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  action <- createFindPackageByCoordinateAction coordinate mWorkspaceUuid
  runOneEntityDB entityName action [tenantQueryUuid tenantUuid, ("organization_id", coordinate.organizationId), ("km_id", coordinate.entityId), ("version", coordinate.version)]

findPackageByCoordinate' :: RequestContextC s sc m => Coordinate -> Maybe U.UUID -> m (Maybe KnowledgeModelPackage)
findPackageByCoordinate' coordinate mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  action <- createFindPackageByCoordinateAction coordinate mWorkspaceUuid
  runOneEntityDB' entityName action [tenantQueryUuid tenantUuid, ("organization_id", coordinate.organizationId), ("km_id", coordinate.entityId), ("version", coordinate.version)]

createFindPackageByCoordinateAction :: RequestContextC s sc m => Coordinate -> Maybe U.UUID -> m (Connection -> IO [KnowledgeModelPackage])
createFindPackageByCoordinateAction Coordinate {..} mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "SELECT * FROM %s WHERE tenant_uuid = ? AND organization_id = ? AND km_id = ? AND version = ? AND %s ORDER BY workspace_uuid IS NOT NULL DESC LIMIT 1"
            [entityName, workspacePlaneCondition]
  let params = [toField tenantUuid, toField organizationId, toField entityId, toField version, toField mWorkspaceUuid]
  logQuery sql params
  return (\conn -> query conn sql params)

findLatestPackageByOrganizationIdAndKmId :: RequestContextC s sc m => String -> String -> Maybe KnowledgeModelPackagePhase -> Maybe U.UUID -> m KnowledgeModelPackage
findLatestPackageByOrganizationIdAndKmId orgId kmId mPhase mWorkspaceUuid = do
  (action, phaseParams) <- createFindLatestPackageByOrganizationIdAndKmIdAction orgId kmId mPhase mWorkspaceUuid
  runOneEntityDB entityName action ([("organization_id", orgId), ("km_id", kmId)] ++ phaseParams)

findLatestPackageByOrganizationIdAndKmId' :: RequestContextC s sc m => String -> String -> Maybe KnowledgeModelPackagePhase -> Maybe U.UUID -> m (Maybe KnowledgeModelPackage)
findLatestPackageByOrganizationIdAndKmId' orgId kmId mPhase mWorkspaceUuid = do
  (action, phaseParams) <- createFindLatestPackageByOrganizationIdAndKmIdAction orgId kmId mPhase mWorkspaceUuid
  runOneEntityDB' entityName action ([("organization_id", orgId), ("km_id", kmId)] ++ phaseParams)

createFindLatestPackageByOrganizationIdAndKmIdAction :: RequestContextC s sc m => String -> String -> Maybe KnowledgeModelPackagePhase -> Maybe U.UUID -> m (Connection -> IO [KnowledgeModelPackage], [(String, String)])
createFindLatestPackageByOrganizationIdAndKmIdAction orgId kmId mPhase mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  let (phaseCondition, phaseParams) =
        case mPhase of
          Just ReleasedKnowledgeModelPackagePhase -> ("AND phase = 'ReleasedKnowledgeModelPackagePhase'", [("phase", "ReleasedKnowledgeModelPackagePhase")])
          Just DeprecatedKnowledgeModelPackagePhase -> ("AND phase = 'DeprecatedKnowledgeModelPackagePhase'", [("phase", "DeprecatedKnowledgeModelPackagePhase")])
          Nothing -> ("", [])
  let sql =
        fromString $
          f''
            "SELECT * \
            \FROM ${package} \
            \WHERE tenant_uuid = ? \
            \  AND organization_id = ? \
            \  AND km_id = ? \
            \  AND ${workspaceCondition} \
            \  ${phaseCondition} \
            \ORDER BY split_part(version, '.', 1)::int DESC, \
            \        split_part(version, '.', 2)::int DESC, \
            \        split_part(version, '.', 3)::int DESC, \
            \        workspace_uuid IS NOT NULL DESC \
            \LIMIT 1"
            [("package", entityName), ("phaseCondition", phaseCondition), ("workspaceCondition", workspacePlaneCondition)]
  let params = [toField tenantUuid, toField orgId, toField kmId, toField mWorkspaceUuid]
  logQuery sql params
  return (\conn -> query conn sql params, phaseParams)

countPackages :: RequestContextC s sc m => m Int
countPackages = do
  tenantUuid <- asks (.tenantUuid')
  createCountByFn entityName tenantCondition [tenantUuid]

countPackagesGroupedByOrganizationIdAndKmId :: RequestContextC s sc m => m Int
countPackagesGroupedByOrganizationIdAndKmId = do
  tenantUuid <- asks (.tenantUuid')
  countPackagesGroupedByOrganizationIdAndKmIdWithTenant tenantUuid

countPackagesGroupedByOrganizationIdAndKmIdWithTenant :: RequestContextC s sc m => U.UUID -> m Int
countPackagesGroupedByOrganizationIdAndKmIdWithTenant tenantUuid = do
  let sql =
        fromString $
          f'
            "SELECT COUNT(*) \
            \FROM (SELECT 1 \
            \      FROM %s \
            \      WHERE tenant_uuid = ? \
            \      GROUP BY organization_id, km_id) nested;"
            [entityName]
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

insertPackage :: RequestContextC s sc m => KnowledgeModelPackage -> m Int64
insertPackage package = do
  createInsertFn entityName package

deletePackages :: RequestContextC s sc m => m Int64
deletePackages = do
  createDeleteEntitiesFn entityName

deletePackagesFiltered :: RequestContextC s sc m => [(String, String)] -> m Int64
deletePackagesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

deletePackageByUuid :: RequestContextC s sc m => U.UUID -> m Int64
deletePackageByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

deletePackagesByOrganizationIdAndKmId :: RequestContextC s sc m => String -> String -> Maybe U.UUID -> m Int64
deletePackagesByOrganizationIdAndKmId organizationId kmId mWorkspaceUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString $ f' "DELETE FROM %s WHERE tenant_uuid = ? AND organization_id = ? AND km_id = ? AND %s" [entityName, workspaceExactCondition]
  let params = [toField tenantUuid, toField organizationId, toField kmId, toField mWorkspaceUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
