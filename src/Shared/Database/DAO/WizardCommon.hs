module Shared.Database.DAO.WizardCommon (
  module Shared.Database.DAO.Common,
  runInTransaction,
  createFindEntitiesGroupByCoordinatePageableQuerySortFn,
  createCountGroupByCoordinateFn,
  WorkspaceAccess (..),
  workspaceAccess,
  workspaceAccessSql,
  workspaceScopeCondition,
  workspaceOnlyCondition,
  tenantOrWorkspaceCondition,
) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import Shared.Database.DAO.Common hiding (runInTransaction)
import qualified Shared.Database.DAO.Common as S
import Shared.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackage ()
import Shared.Localization.Messages.Workspace.Public
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Context.Scope
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Error.Error
import Shared.Model.User.RolePermission
import Shared.Model.User.RoleSimple
import Shared.Service.Acl.AclService (permissionsInWorkspace)
import Shared.Util.Logger
import Shared.Util.String

runInTransaction :: WizardRequestContextC s m => m a -> m a
runInTransaction = S.runInTransaction logInfoI logWarnI

workspaceGroupSql :: String -> Bool -> (String, String)
workspaceGroupSql entityName True = (f' ", ':', %s.workspace_uuid" [entityName], f' ", %s.workspace_uuid" [entityName])
workspaceGroupSql _ False = ("", "")

data WorkspaceAccess
  = UnrestrictedWorkspaceAccess
  | RestrictedWorkspaceAccess [U.UUID]

workspaceAccess :: WizardRequestContextC s m => Maybe String -> m WorkspaceAccess
workspaceAccess mPermission = do
  mCurrentUser <- asks (.currentUser')
  workspaceRoles <- asks (.workspaceRoles')
  multiWorkspace <- asks (.tenantMultiWorkspace')
  return $
    case mCurrentUser of
      Nothing -> UnrestrictedWorkspaceAccess
      Just user
        | any (`elem` permissionsInWorkspace user workspaceRoles multiWorkspace Nothing) (maybe workspaceReachRolePermissions (: []) mPermission) -> UnrestrictedWorkspaceAccess
        | otherwise -> RestrictedWorkspaceAccess [workspaceUuid | (workspaceUuid, role) <- M.toList workspaceRoles, maybe True (\permission -> multiWorkspace && permission `elem` role.permissions) mPermission]

isWorkspaceAccessible :: WorkspaceAccess -> U.UUID -> Bool
isWorkspaceAccessible UnrestrictedWorkspaceAccess _ = True
isWorkspaceAccessible (RestrictedWorkspaceAccess workspaceUuids) workspaceUuid = workspaceUuid `elem` workspaceUuids

workspaceAccessSql :: String -> WorkspaceAccess -> String
workspaceAccessSql _ UnrestrictedWorkspaceAccess = "TRUE"
workspaceAccessSql _ (RestrictedWorkspaceAccess []) = "FALSE"
workspaceAccessSql column (RestrictedWorkspaceAccess workspaceUuids) = f' "%s IN (%s)" [column, L.intercalate ", " (fmap (\u -> f' "'%s'" [U.toString u]) workspaceUuids)]

workspaceScopeCondition :: WizardRequestContextC s m => String -> m String
workspaceScopeCondition column = do
  scope <- asks (.scope')
  when (scope == TenantScope) (throwError $ UserError _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED)
  return $
    case scope of
      WorkspaceScope workspaceUuid -> f' " AND %s = '%s'" [column, U.toString workspaceUuid]
      _ -> ""

workspaceOnlyCondition :: WizardRequestContextC s m => Maybe String -> String -> m String
workspaceOnlyCondition mPermission column = do
  access <- workspaceAccess mPermission
  scope <- asks (.scope')
  when (scope == TenantScope) (throwError $ UserError _ERROR_SERVICE_WORKSPACE__TENANT_NOT_ACCEPTED)
  return $
    case (scope, access) of
      (WorkspaceScope workspaceUuid, _)
        | isWorkspaceAccessible access workspaceUuid -> f' " AND %s = '%s'" [column, U.toString workspaceUuid]
        | otherwise -> " AND FALSE"
      (_, UnrestrictedWorkspaceAccess) -> ""
      (_, _) -> f' " AND %s" [workspaceAccessSql column access]

tenantOrWorkspaceCondition :: WizardRequestContextC s m => Maybe String -> String -> m String
tenantOrWorkspaceCondition mPermission column = do
  access <- workspaceAccess mPermission
  scope <- asks (.scope')
  let tenantRows =
        case (mPermission, access) of
          (Just _, RestrictedWorkspaceAccess _) -> "FALSE"
          _ -> f' "%s IS NULL" [column]
  return $
    case (scope, access) of
      (TenantScope, _) -> f' " AND %s" [tenantRows]
      (WorkspaceScope workspaceUuid, _)
        | isWorkspaceAccessible access workspaceUuid -> f' " AND (%s OR %s = '%s')" [tenantRows, column, U.toString workspaceUuid]
        | otherwise -> " AND FALSE"
      (NoScope, UnrestrictedWorkspaceAccess) -> ""
      (NoScope, _) -> f' " AND (%s OR %s)" [tenantRows, workspaceAccessSql column access]

createFindEntitiesGroupByCoordinatePageableQuerySortFn entityName registryEntityName pageLabel pageable sort fields mQuery mEnabled mId mOutdated outdatedCondition workspaceAware scopeCondition =
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    let (workspaceKey, workspaceGroup) = workspaceGroupSql entityName workspaceAware
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
        mQuery
        enabledCondition
        mId
        mOutdated
        outdatedCondition
        workspaceAware
        scopeCondition
    -- 3. Get entities
    let sql =
          f''
            "SELECT ${fields} \
            \FROM ${entityName} \
            \LEFT JOIN ${registryEntityName} ON ${entityName}.id = ${registryEntityName}.id \
            \WHERE tenant_uuid = ? AND concat(${entityName}.id, ':', ${entityName}.version${workspaceKey}) IN ( \
            \    SELECT CONCAT(id, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[2] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[3]${workspaceKey}) \
            \    FROM ${entityName} \
            \    WHERE ${enabledCondition} tenant_uuid = ? AND (name ~* ? OR id ~* ? OR version ~* ?) ${coordinateSql} ${scopeCondition} \
            \    GROUP BY id${workspaceGroup} \
            \) \
            \${outdatedCondition} \
            \${sort} \
            \OFFSET ${offset} \
            \LIMIT ${limit}"
            [ ("fields", fields)
            , ("entityName", entityName)
            , ("registryEntityName", registryEntityName)
            , ("enabledCondition", enabledCondition)
            , ("coordinateSql", mapToDBIdSql entityName mId)
            , ("outdatedCondition", outdatedCondition)
            , ("scopeCondition", scopeCondition)
            , ("workspaceKey", workspaceKey)
            , ("workspaceGroup", workspaceGroup)
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
                : fmap toField (mapToDBIdParams mId)
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
  -> Maybe String
  -> String
  -> Maybe String
  -> Maybe Bool
  -> String
  -> Bool
  -> String
  -> m Int
createCountGroupByCoordinateFn entityName registryEntityName mQuery enabledCondition mId mOutdated outdatedCondition workspaceAware scopeCondition = do
  tenantUuid <- asks (.tenantUuid')
  let (workspaceKey, workspaceGroup) = workspaceGroupSql entityName workspaceAware
  let sql =
        f''
          "SELECT count(*) \
          \FROM ${entityName} \
          \LEFT JOIN ${registryEntityName} ON ${entityName}.id = ${registryEntityName}.id \
          \WHERE ${enabledCondition} tenant_uuid = ? AND (name ~* ? OR ${entityName}.id ~* ? OR ${entityName}.version ~* ?) ${coordinateSql} ${outdatedCondition} \
          \  AND concat(${entityName}.id, ':', ${entityName}.version${workspaceKey}) IN \
          \      (SELECT CONCAT(id, ':', \
          \                     (max(string_to_array(version, '.')::int[]))[1] || '.' || \
          \                     (max(string_to_array(version, '.')::int[]))[2] || '.' || \
          \                     (max(string_to_array(version, '.')::int[]))[3]${workspaceKey}) \
          \             FROM ${entityName} \
          \             WHERE tenant_uuid = ? \
          \               AND (name ~* ? OR ${entityName}.id ~* ? OR ${entityName}.version ~* ?) ${scopeCondition} \
          \             GROUP BY ${entityName}.id${workspaceGroup})"
          [ ("entityName", entityName)
          , ("registryEntityName", registryEntityName)
          , ("enabledCondition", enabledCondition)
          , ("coordinateSql", mapToDBIdSql entityName mId)
          , ("outdatedCondition", outdatedCondition)
          , ("scopeCondition", scopeCondition)
          , ("workspaceKey", workspaceKey)
          , ("workspaceGroup", workspaceGroup)
          ]
  logInfo _CMP_DATABASE sql
  let action conn =
        query
          conn
          (fromString sql)
          ( [toField tenantUuid, toField (regexM mQuery), toField (regexM mQuery), toField (regexM mQuery)]
              ++ fmap toField (mapToDBIdParams mId)
              ++ maybeToList (fmap toField mOutdated)
              ++ [toField tenantUuid]
              ++ [toField $ regexM mQuery, toField $ regexM mQuery, toField (regexM mQuery)]
          )
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0
