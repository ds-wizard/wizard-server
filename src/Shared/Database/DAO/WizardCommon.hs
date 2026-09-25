module Shared.Database.DAO.WizardCommon (
  module Shared.Database.DAO.Common,
  runInTransaction,
  createFindEntitiesGroupByCoordinatePageableQuerySortFn,
  createCountGroupByCoordinateFn,
  workspaceOnlyCondition,
  tenantOrWorkspaceCondition,
) where

import Control.Monad.Reader (asks)
import Data.Maybe (maybeToList)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.Common hiding (runInTransaction)
import qualified Shared.Database.DAO.Common as S
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackage ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Context.Scope
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger
import Shared.Util.String

runInTransaction :: WizardRequestContextC s m => m a -> m a
runInTransaction = S.runInTransaction logInfoI logWarnI

planeGroupSql :: String -> Bool -> (String, String)
planeGroupSql entityName True = (f' ", ':', %s.workspace_uuid" [entityName], f' ", %s.workspace_uuid" [entityName])
planeGroupSql _ False = ("", "")

workspaceOnlyCondition :: WizardRequestContextC s m => String -> m String
workspaceOnlyCondition column = do
  mCurrentUser <- asks (.currentUser')
  scope <- asks (.scope')
  let floor =
        case mCurrentUser of
          Just user -> f' " AND %s" [workspaceMembershipCondition column user.uuid]
          Nothing -> ""
  return $
    case scope of
      WorkspaceScope workspaceUuid -> f' "%s AND %s = '%s'" [floor, column, U.toString workspaceUuid]
      _ -> floor

tenantOrWorkspaceCondition :: WizardRequestContextC s m => String -> m String
tenantOrWorkspaceCondition column = do
  mCurrentUser <- asks (.currentUser')
  scope <- asks (.scope')
  let floor =
        case mCurrentUser of
          Just user -> f' " AND (%s IS NULL OR %s)" [column, workspaceMembershipCondition column user.uuid]
          Nothing -> ""
  let requested workspaceUuid =
        case mCurrentUser of
          Just user -> f' " AND %s" [workspaceMembershipCondition (f' "'%s'::uuid" [U.toString workspaceUuid]) user.uuid]
          Nothing -> ""
  return $
    case scope of
      TenantScope -> f' "%s AND %s IS NULL" [floor, column]
      WorkspaceScope workspaceUuid -> f' "%s%s AND (%s IS NULL OR %s = '%s')" [floor, requested workspaceUuid, column, column, U.toString workspaceUuid]
      NoScope -> floor

createFindEntitiesGroupByCoordinatePageableQuerySortFn entityName registryEntityName pageLabel pageable sort fields entityId mQuery mEnabled mOrganizationId mEntityId mOutdated outdatedCondition planeAware scopeCondition =
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    let (planeKey, planeGroup) = planeGroupSql entityName planeAware
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    let enabledCondition =
          case mEnabled of
            Just True -> "enabled = true AND"
            Just False -> "enabled = false AND"
            _ -> ""
    -- 2. Get total count
    count <-
      createCountGroupByCoordinateFn
        entityName
        registryEntityName
        entityId
        mQuery
        enabledCondition
        mOrganizationId
        mEntityId
        mOutdated
        outdatedCondition
        planeAware
        scopeCondition
    -- 3. Get entities
    let sql =
          f''
            "SELECT ${fields} \
            \FROM ${entityName} \
            \LEFT JOIN ${registryEntityName} ON ${entityName}.organization_id = ${registryEntityName}.organization_id AND ${entityName}.${entityId} = ${registryEntityName}.${entityId} \
            \LEFT JOIN registry_organization ON ${entityName}.organization_id = registry_organization.organization_id \
            \WHERE tenant_uuid = ? AND concat(${entityName}.organization_id, ':', ${entityName}.${entityId}, ':', ${entityName}.version${planeKey}) IN ( \
            \    SELECT CONCAT(organization_id, ':', ${entityId}, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[2] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[3]${planeKey}) \
            \    FROM ${entityName} \
            \    WHERE ${enabledCondition} tenant_uuid = ? AND (name ~* ? OR organization_id ~* ? OR ${entityId} ~* ? OR version ~* ?) ${coordinateSql} ${scopeCondition} \
            \    GROUP BY organization_id, ${entityId}${planeGroup} \
            \) \
            \${outdatedCondition} \
            \${sort} \
            \OFFSET ${offset} \
            \LIMIT ${limit}"
            [ ("fields", fields)
            , ("entityName", entityName)
            , ("registryEntityName", registryEntityName)
            , ("entityId", entityId)
            , ("enabledCondition", enabledCondition)
            , ("coordinateSql", mapToDBCoordinatesSql entityName entityId mOrganizationId mEntityId)
            , ("outdatedCondition", outdatedCondition)
            , ("scopeCondition", scopeCondition)
            , ("planeKey", planeKey)
            , ("planeGroup", planeGroup)
            , ("sort", mapSort sort)
            , ("offset", show skip)
            , ("limit", show sizeI)
            ]
    logInfoI _CMP_DATABASE (trim sql)
    let action conn =
          query
            conn
            (fromString sql)
            ( toField tenantUuid
                : toField tenantUuid
                : toField (regexM mQuery)
                : toField (regexM mQuery)
                : toField (regexM mQuery)
                : toField (regexM mQuery)
                : fmap toField (mapToDBCoordinatesParams mOrganizationId mEntityId)
                ++ (maybeToList . fmap toField $ mOutdated)
            )
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

createCountGroupByCoordinateFn
  :: WizardRequestContextC s m
  => String
  -> String
  -> String
  -> Maybe String
  -> String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> String
  -> Bool
  -> String
  -> m Int
createCountGroupByCoordinateFn entityName registryEntityName entityId mQuery enabledCondition mOrganizationId mEntityId mOutdated outdatedCondition planeAware scopeCondition = do
  tenantUuid <- asks (.tenantUuid')
  let (planeKey, planeGroup) = planeGroupSql entityName planeAware
  let sql =
        f''
          "SELECT count(*) \
          \FROM ${entityName} \
          \LEFT JOIN ${registryEntityName} ON ${entityName}.organization_id = ${registryEntityName}.organization_id AND ${entityName}.${entityId} = ${registryEntityName}.${entityId} \
          \WHERE ${enabledCondition} tenant_uuid = ? AND (name ~* ? OR ${entityName}.organization_id ~* ? OR ${entityName}.${entityId} ~* ? OR ${entityName}.version ~* ?) ${coordinateSql} ${outdatedCondition} \
          \  AND concat(${entityName}.organization_id, ':', ${entityName}.${entityId}, ':', ${entityName}.version${planeKey}) IN \
          \      (SELECT CONCAT(organization_id, ':', ${entityId}, ':', \
          \                     (max(string_to_array(version, '.')::int[]))[1] || '.' || \
          \                     (max(string_to_array(version, '.')::int[]))[2] || '.' || \
          \                     (max(string_to_array(version, '.')::int[]))[3]${planeKey}) \
          \             FROM ${entityName} \
          \             WHERE tenant_uuid = ? \
          \               AND (name ~* ? OR ${entityName}.organization_id ~* ? OR ${entityName}.${entityId} ~* ? OR ${entityName}.version ~* ?) ${scopeCondition} \
          \             GROUP BY ${entityName}.organization_id, ${entityName}.${entityId}${planeGroup})"
          [ ("entityName", entityName)
          , ("registryEntityName", registryEntityName)
          , ("entityId", entityId)
          , ("enabledCondition", enabledCondition)
          , ("coordinateSql", mapToDBCoordinatesSql entityName entityId mOrganizationId mEntityId)
          , ("outdatedCondition", outdatedCondition)
          , ("scopeCondition", scopeCondition)
          , ("planeKey", planeKey)
          , ("planeGroup", planeGroup)
          ]
  logInfo _CMP_DATABASE sql
  let action conn =
        query
          conn
          (fromString sql)
          ( [toField tenantUuid, toField (regexM mQuery), toField (regexM mQuery), toField (regexM mQuery), toField (regexM mQuery)]
              ++ fmap toField (mapToDBCoordinatesParams mOrganizationId mEntityId)
              ++ maybeToList (fmap toField mOutdated)
              ++ [toField tenantUuid]
              ++ [toField $ regexM mQuery, toField $ regexM mQuery, toField (regexM mQuery), toField (regexM mQuery)]
          )
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0
