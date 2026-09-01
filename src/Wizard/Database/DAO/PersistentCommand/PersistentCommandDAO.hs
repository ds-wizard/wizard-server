module Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO where

import Data.String (fromString)
import Database.PostgreSQL.Simple

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Logger
import Shared.Common.Util.String (f'')
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.Public.Database.Mapping.PersistentCommand.PersistentCommandList ()
import WizardLib.Public.Model.PersistentCommand.PersistentCommandList

entityName = "w_persistent_command"

pageLabel = "persistentCommands"

findPersistentCommandsPage :: [String] -> Pageable -> [Sort] -> AppContextM (Page PersistentCommandList)
findPersistentCommandsPage states pageable sort = do
  -- 1. Prepare variables
  do
    let (statesCondition, statesParam) =
          case states of
            [] -> ("", [])
            _ -> (f' "WHERE w_persistent_command.state in (%s)" [generateQuestionMarks states], states)
    let conditionParams = statesParam
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    count <- createCountByFn entityName statesCondition statesParam
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT w_persistent_command.uuid, \
              \       w_persistent_command.state, \
              \       w_persistent_command.component, \
              \       w_persistent_command.function, \
              \       w_persistent_command.attempts, \
              \       w_persistent_command.max_attempts, \
              \       w_persistent_command.created_at, \
              \       w_persistent_command.updated_at, \
              \       concat(w_tenant.uuid, '::', \
              \              w_tenant.name, '::', \
              \              w_config_look_and_feel.logo_url, '::', \
              \              w_config_look_and_feel.primary_color, '::', \
              \              w_tenant.client_url) AS tenant, \
              \       CASE \
              \              WHEN w_user_entity.uuid IS NOT NULL THEN concat(w_user_entity.uuid, '::', \
              \                                                            w_user_entity.first_name, '::', \
              \                                                            w_user_entity.last_name, '::', \
              \                                                            w_user_entity.email, '::', \
              \                                                            w_user_entity.image_url, '::', \
              \                                                            w_user_entity.affiliation) \
              \       END AS created_by \
              \FROM w_persistent_command \
              \         LEFT JOIN w_tenant ON w_tenant.uuid = w_persistent_command.tenant_uuid \
              \         LEFT JOIN w_config_look_and_feel ON w_config_look_and_feel.tenant_uuid = w_persistent_command.tenant_uuid \
              \         LEFT JOIN w_user_entity ON w_user_entity.uuid = w_persistent_command.created_by AND w_user_entity.tenant_uuid = w_persistent_command.tenant_uuid \
              \${statesCondition} \
              \${sort} \
              \OFFSET ${offset} \
              \LIMIT ${limit}"
              [ ("statesCondition", statesCondition)
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
