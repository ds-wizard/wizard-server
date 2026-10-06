module Shared.Database.DAO.KnowledgeModel.KnowledgeModelPackageDAO where

import Control.Monad.Reader (asks)
import qualified Data.List as L
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageList ()
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageSuggestion ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageList
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Shared.Util.Logger

entityName = "knowledge_model_package"

pageLabel = "knowledgeModelPackages"

findPackagesPage
  :: WizardRequestContextC s m
  => Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Pageable
  -> [Sort]
  -> m (Page KnowledgeModelPackageList)
findPackagesPage mId mQuery mOutdated pageable sort = do
  workspaceCondition <- tenantOrWorkspaceCondition Nothing "knowledge_model_package.workspace_uuid"
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    "registry_knowledge_model_package"
    pageLabel
    pageable
    sort
    "uuid, knowledge_model_package.name, knowledge_model_package.id, version, phase, description, non_editable, public, registry_knowledge_model_package.remote_version, knowledge_model_package.language, knowledge_model_package.created_at, knowledge_model_package.workspace_uuid"
    mQuery
    Nothing
    mId
    mOutdated
    ( case mOutdated of
        Just _ -> " AND is_outdated(registry_knowledge_model_package.remote_version, version) = ?"
        Nothing -> ""
    )
    True
    workspaceCondition

findPackageSuggestionByUuid :: WizardRequestContextC s m => U.UUID -> m KnowledgeModelPackageSuggestion
findPackageSuggestionByUuid uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityWithFieldsByFn "uuid, name, id, version, description" False entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

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
              let mapFn _ = " id = ?"
               in (" AND (" ++ L.intercalate " OR " (fmap mapFn selectCoordinates) ++ ")", fmap (.id) selectCoordinates)
    let (excludeCondition, excludeParams) =
          case mExcludeCoordinates of
            Nothing -> ("", [])
            Just [] -> ("", [])
            Just excludeCoordinates ->
              let mapFn _ = " id != ?"
               in (" AND (" ++ L.intercalate " AND " (fmap mapFn excludeCoordinates) ++ ")", fmap (.id) excludeCoordinates)
    let phaseCondition =
          case mPhase of
            Just phase -> f' "AND phase = '%s'" [show phase]
            Nothing -> ""
    let nonEditableCondition =
          case mNonEditable of
            Just nonEditable -> f' "AND non_editable = '%s'" [show nonEditable]
            Nothing -> ""
    workspaceCondition <- tenantOrWorkspaceCondition Nothing "workspace_uuid"
    -- 2. Get total count
    count <- countPackageSuggestions mQuery selectCondition excludeCondition selectParams excludeParams phaseCondition (nonEditableCondition ++ workspaceCondition)
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "SELECT uuid, \
              \       name, \
              \       id, \
              \       version, \
              \       description \
              \FROM knowledge_model_package outer_package \
              \WHERE tenant_uuid = ? AND concat(id, ':', version, ':', workspace_uuid) IN ( \
              \    SELECT CONCAT(id, ':', \
              \                  (max(string_to_array(version, '.')::int[]))[1] || '.' || \
              \                  (max(string_to_array(version, '.')::int[]))[2] || '.' || \
              \                  (max(string_to_array(version, '.')::int[]))[3], ':', workspace_uuid) \
              \    FROM knowledge_model_package \
              \    WHERE tenant_uuid = ? \
              \      AND (name ~* ? OR id ~* ? OR version ~* ?) %s %s %s %s %s \
              \    GROUP BY id, workspace_uuid) \
              \%s \
              \OFFSET %s \
              \LIMIT %s"
              [selectCondition, excludeCondition, phaseCondition, nonEditableCondition, workspaceCondition, mapSort sort, show skip, show sizeI]
    let params =
          [U.toString tenantUuid, U.toString tenantUuid]
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
            \   WHERE tenant_uuid = ? AND (name ~* ? OR id ~* ? OR version ~* ?) %s %s %s %s \
            \   GROUP BY id, workspace_uuid) nested"
            [selectCondition, excludeCondition, phaseCondition, nonEditableCondition]
  let params =
        [U.toString tenantUuid, regexM mQuery, regexM mQuery, regexM mQuery]
          ++ selectParams
          ++ excludeParams
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

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
