module Shared.Database.DAO.Project.ProjectUserDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Shared.Database.DAO.WizardCommon
import Shared.Database.Mapping.User.UserSimple ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.UserSimple
import Shared.Util.String

findProjectUserSuggestionsPage :: WizardRequestContextC s m => U.UUID -> String -> Maybe String -> Pageable -> [Sort] -> m (Page UserSimple)
findProjectUserSuggestionsPage projectUuid perm mQuery pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks (.tenantUuid')
    let (qCondition, qRegex) =
          ( "WHERE (concat(first_name, ' ', last_name) ~* ? OR email ~* ?)"
          , [regexM mQuery, regexM mQuery]
          )
    let params = [U.toString projectUuid, U.toString tenantUuid, U.toString projectUuid, U.toString tenantUuid] ++ qRegex
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    let countSql =
          fromString $
            f''
              "SELECT DISTINCT COUNT(uuid) \
              \FROM (SELECT u.uuid, \
              \             u.first_name, \
              \             u.last_name, \
              \             u.email \
              \      FROM project_perm_user \
              \      JOIN user_entity u ON project_perm_user.user_uuid = u.uuid AND project_perm_user.tenant_uuid = u.tenant_uuid \
              \      WHERE project_perm_user.project_uuid = ? \
              \        AND project_perm_user.tenant_uuid = ? \
              \        AND project_perm_user.perms @> ARRAY ['${perm}'] \
              \        AND u.active = true \
              \        AND u.machine = false \
              \      UNION ALL \
              \      SELECT u.uuid, \
              \             u.first_name, \
              \             u.last_name, \
              \             u.email \
              \      FROM project_perm_group \
              \      LEFT JOIN user_group_membership ug_membership ON ug_membership.user_group_uuid = project_perm_group.user_group_uuid AND ug_membership.tenant_uuid = project_perm_group.tenant_uuid \
              \      LEFT JOIN user_entity u ON u.uuid = ug_membership.user_uuid AND u.tenant_uuid = project_perm_group.tenant_uuid \
              \      WHERE project_perm_group.project_uuid = ? \
              \        AND project_perm_group.tenant_uuid = ? \
              \        AND project_perm_group.perms @> ARRAY ['${perm}'] \
              \        AND u.active = true \
              \        AND u.machine = false) u \
              \${qCondition}"
              [ ("qCondition", qCondition)
              , ("perm", perm)
              ]
    logQuery countSql params
    let action conn = query conn countSql params
    result <- runDB action
    let count =
          case result of
            [count] -> fromOnly count
            _ -> 0
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT DISTINCT * \
              \FROM (SELECT u.uuid, \
              \             u.first_name, \
              \             u.last_name, \
              \             u.email, \
              \             u.image_url, \
              \             u.affiliation \
              \      FROM project_perm_user \
              \      JOIN user_entity u ON project_perm_user.user_uuid = u.uuid AND project_perm_user.tenant_uuid = u.tenant_uuid \
              \      WHERE project_perm_user.project_uuid = ? \
              \        AND project_perm_user.tenant_uuid = ? \
              \        AND project_perm_user.perms @> ARRAY ['${perm}'] \
              \        AND u.active = true \
              \        AND u.machine = false \
              \      UNION ALL \
              \      SELECT u.uuid, \
              \             u.first_name, \
              \             u.last_name, \
              \             u.email, \
              \             u.image_url, \
              \             u.affiliation \
              \      FROM project_perm_group \
              \      LEFT JOIN user_group_membership ug_membership ON ug_membership.user_group_uuid = project_perm_group.user_group_uuid AND ug_membership.tenant_uuid = project_perm_group.tenant_uuid \
              \      LEFT JOIN user_entity u ON u.uuid = ug_membership.user_uuid AND u.tenant_uuid = project_perm_group.tenant_uuid \
              \      WHERE project_perm_group.project_uuid = ? \
              \        AND project_perm_group.tenant_uuid = ? \
              \        AND project_perm_group.perms @> ARRAY ['${perm}'] \
              \        AND u.active = true \
              \        AND u.machine = false) u \
              \${qCondition} \
              \${sort} \
              \OFFSET ${offset} \
              \LIMIT ${limit}"
              [ ("qCondition", qCondition)
              , ("perm", perm)
              , ("sort", mapSort sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
              ]
    logQuery sql params
    let action conn = query conn sql params
    entities <- runDB action
    -- 5. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page "users" metadata entities
