module Shared.Database.DAO.KnowledgeModel.KnowledgeModelPackageDAO where

import Control.Monad.Reader (asks)
import qualified Data.List as L
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact ()
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageList ()
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageSuggestion ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageDeletionImpact
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageList
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Util.Logger
import Shared.Util.String

entityName = "knowledge_model_package"

pageLabel = "knowledgeModelPackages"

findPackagesPage
  :: WizardRequestContextC s m
  => Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Pageable
  -> [Sort]
  -> m (Page KnowledgeModelPackageList)
findPackagesPage mOrganizationId mKmId mQuery mOutdated pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    "registry_knowledge_model_package"
    pageLabel
    pageable
    sort
    "uuid, knowledge_model_package.name, knowledge_model_package.organization_id, knowledge_model_package.km_id, version, phase, description, non_editable, public, registry_knowledge_model_package.remote_version, registry_organization.name as org_name, registry_organization.logo as org_logo, knowledge_model_package.language, knowledge_model_package.created_at"
    "km_id"
    mQuery
    Nothing
    mOrganizationId
    mKmId
    mOutdated
    ( case mOutdated of
        Just _ -> " AND is_outdated(registry_knowledge_model_package.remote_version, version) = ?"
        Nothing -> ""
    )

findPackageSuggestionByUuid :: WizardRequestContextC s m => U.UUID -> m KnowledgeModelPackageSuggestion
findPackageSuggestionByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityWithFieldsByFn "uuid, name, organization_id, km_id, version, description" False entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findPackageSuggestionsPage :: WizardRequestContextC s m => Maybe String -> Maybe [Coordinate] -> Maybe [Coordinate] -> Maybe KnowledgeModelPackagePhase -> Maybe Bool -> Pageable -> [Sort] -> m (Page KnowledgeModelPackageSuggestion)
findPackageSuggestionsPage mQuery mSelectCoordinates mExcludeCoordinates mPhase mNonEditable pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    let (selectCondition, selectParams) =
          case mSelectCoordinates of
            Nothing -> ("", [])
            Just [] -> ("", [])
            Just selectCoordinates ->
              let mapFn _ = " (organization_id = ? AND km_id = ?)"
               in (" AND (" ++ L.intercalate " OR " (fmap mapFn selectCoordinates) ++ ")", concatMap (\c -> [c.organizationId, c.entityId]) selectCoordinates)
    let (excludeCondition, excludeParams) =
          case mExcludeCoordinates of
            Nothing -> ("", [])
            Just [] -> ("", [])
            Just excludeCoordinates ->
              let mapFn _ = " NOT(organization_id = ? AND km_id = ?)"
               in (" AND (" ++ L.intercalate " AND " (fmap mapFn excludeCoordinates) ++ ")", concatMap (\c -> [c.organizationId, c.entityId]) excludeCoordinates)
    let phaseCondition =
          case mPhase of
            Just phase -> f' "AND phase = '%s'" [show phase]
            Nothing -> ""
    let nonEditableCondition =
          case mNonEditable of
            Just nonEditable -> f' "AND non_editable = '%s'" [show nonEditable]
            Nothing -> ""
    -- 2. Get total count
    count <- countPackageSuggestions mQuery selectCondition excludeCondition selectParams excludeParams phaseCondition nonEditableCondition
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "SELECT uuid, \
              \       name, \
              \       organization_id, \
              \       km_id, \
              \       version, \
              \       description \
              \FROM knowledge_model_package outer_package \
              \WHERE tenant_uuid = ? AND concat(organization_id, ':', km_id, ':', version) IN ( \
              \    SELECT CONCAT(organization_id, ':', km_id, ':', \
              \                  (max(string_to_array(version, '.')::int[]))[1] || '.' || \
              \                  (max(string_to_array(version, '.')::int[]))[2] || '.' || \
              \                  (max(string_to_array(version, '.')::int[]))[3]) \
              \    FROM knowledge_model_package \
              \    WHERE tenant_uuid = ? \
              \      AND (name ~* ? OR organization_id ~* ? OR km_id ~* ? OR version ~* ?) %s %s %s %s \
              \    GROUP BY organization_id, km_id) \
              \%s \
              \OFFSET %s \
              \LIMIT %s"
              [selectCondition, excludeCondition, phaseCondition, nonEditableCondition, mapSort sort, show skip, show sizeI]
    let params =
          [U.toString tenantUuid, U.toString tenantUuid]
            ++ [regexM mQuery]
            ++ [regexM mQuery]
            ++ [regexM mQuery]
            ++ [regexM mQuery]
            ++ selectParams
            ++ excludeParams
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

countPackageSuggestions :: WizardRequestContextC s m => Maybe String -> String -> String -> [String] -> [String] -> String -> String -> m Int
countPackageSuggestions mQuery selectCondition excludeCondition selectParams excludeParams phaseCondition nonEditableCondition = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "SELECT COUNT(*) \
            \FROM (SELECT COUNT(*) \
            \   FROM knowledge_model_package \
            \   WHERE tenant_uuid = ? AND (name ~* ? OR organization_id ~* ? OR km_id ~* ? OR version ~* ?) %s %s %s %s \
            \   GROUP BY organization_id, km_id) nested"
            [selectCondition, excludeCondition, phaseCondition, nonEditableCondition]
  let params =
        [U.toString tenantUuid, regexM mQuery, regexM mQuery, regexM mQuery, regexM mQuery]
          ++ selectParams
          ++ excludeParams
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

findDependentPackageResources :: WizardRequestContextC s m => [U.UUID] -> m [KnowledgeModelPackageDeletionImpact]
findDependentPackageResources pkgUuids = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        f''
          "WITH RECURSIVE package_tree AS ( \
          \    SELECT  \
          \        uuid, \
          \        name, \
          \        version, \
          \        previous_package_uuid, \
          \        tenant_uuid \
          \    FROM knowledge_model_package \
          \    WHERE uuid IN (${pkgUuids}) \
          \      AND tenant_uuid = '${tenantUuid}' \
          \ \
          \    UNION \
          \ \
          \    SELECT  \
          \        p.uuid, \
          \        p.name, \
          \        p.version, \
          \        p.previous_package_uuid, \
          \        p.tenant_uuid \
          \    FROM knowledge_model_package p \
          \    INNER JOIN package_tree pt ON p.previous_package_uuid = pt.uuid \
          \    WHERE p.tenant_uuid = '${tenantUuid}' \
          \) \
          \SELECT  \
          \    pt.uuid, \
          \    pt.name, \
          \    pt.version, \
          \    (SELECT COALESCE(jsonb_agg(jsonb_build_object( \
          \        'uuid', child.uuid, \
          \        'name', child.name, \
          \        'version', child.version \
          \    )), '[]'::jsonb) \
          \    FROM knowledge_model_package child \
          \    WHERE child.previous_package_uuid = pt.uuid \
          \      AND child.tenant_uuid = pt.tenant_uuid) AS packages, \
          \ \
          \    (SELECT COALESCE(jsonb_agg(jsonb_build_object( \
          \        'uuid', e.uuid, \
          \        'name', e.name \
          \    )), '[]'::jsonb) \
          \    FROM knowledge_model_editor e \
          \    WHERE e.previous_package_uuid = pt.uuid \
          \      AND e.tenant_uuid = pt.tenant_uuid) AS editors, \
          \ \
          \    (SELECT COALESCE(jsonb_agg(jsonb_build_object( \
          \        'uuid', pr.uuid, \
          \        'name', pr.name \
          \    )), '[]'::jsonb) \
          \    FROM project pr \
          \    WHERE pr.knowledge_model_package_uuid = pt.uuid \
          \      AND pr.tenant_uuid = pt.tenant_uuid) AS projects \
          \FROM package_tree pt;"
          [ ("pkgUuids", L.intercalate "," . fmap (\u -> f' "'%s'" [U.toString u]) $ pkgUuids)
          , ("tenantUuid", U.toString tenantUuid)
          ]
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  runDB action

updatePackageMetamodelVersion :: WizardRequestContextC s m => U.UUID -> Int -> m Int64
updatePackageMetamodelVersion pkgUuid metamodelVersion = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE knowledge_model_package SET metamodel_version = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField metamodelVersion, toField tenantUuid, toField pkgUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updatePackagePhaseAndPublicByUuid :: WizardRequestContextC s m => U.UUID -> KnowledgeModelPackagePhase -> Bool -> m Int64
updatePackagePhaseAndPublicByUuid pkgUuid phase public = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "UPDATE knowledge_model_package SET phase = ?, public = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField phase, toField public, toField tenantUuid, toField pkgUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
