module Shared.Database.DAO.Project.ProjectPermDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.Project.ProjectPerm ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Project.Acl.ProjectPerm
import Shared.Util.Logger

entityName_user = "project_perm_user"

entityName_group = "project_perm_group"

findProjectPermsFiltered :: WizardRequestContextC s m => [(String, String)] -> m [ProjectPerm]
findProjectPermsFiltered queryParams = do
  let sql =
        fromString $
          f'
            "SELECT project_uuid, 'UserProjectPermType' AS member_type, user_uuid as member_uuid, perms, tenant_uuid \
            \ FROM %s \
            \ WHERE %s \
            \ UNION \
            \ SELECT project_uuid, 'UserGroupProjectPermType' AS member_type, user_group_uuid as member_uuid, perms, tenant_uuid \
            \ FROM %s \
            \ WHERE %s"
            [entityName_user, mapToDBQuerySql queryParams, entityName_group, mapToDBQuerySql queryParams]
  let params = fmap snd queryParams ++ fmap snd queryParams
  logQuery sql params
  let action conn = query conn sql params
  runDB action

insertProjectPerm :: WizardRequestContextC s m => ProjectPerm -> m Int64
insertProjectPerm perm =
  case perm.memberType of
    UserProjectPermType -> createInsertFn entityName_user perm
    UserGroupProjectPermType -> createInsertFn entityName_group perm

deleteProjectPerms :: WizardRequestContextC s m => m Int64
deleteProjectPerms = do
  createDeleteEntitiesFn entityName_user
  createDeleteEntitiesFn entityName_group

deleteProjectPermsFiltered :: WizardRequestContextC s m => [(String, String)] -> m Int64
deleteProjectPermsFiltered queryParams = do
  createDeleteEntitiesByFn entityName_user queryParams
  createDeleteEntitiesByFn entityName_group queryParams

deleteProjectPermGroupByUserGroupUuid :: WizardRequestContextC s m => U.UUID -> m Int64
deleteProjectPermGroupByUserGroupUuid userGroupUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntitiesByFn entityName_group [tenantQueryUuid tenantUuid, ("user_group_uuid", U.toString userGroupUuid)]
