module Shared.Database.DAO.PersistentCommand.WizardPersistentCommandDAO where

import Data.String (fromString)
import Database.PostgreSQL.Simple

import Shared.Database.DAO.Common
import Shared.Database.Mapping.PersistentCommand.PersistentCommandList ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.WizardRequestContext
import Shared.Model.PersistentCommand.PersistentCommandList
import Shared.Util.Logger
import Shared.Util.String (f'')

entityName = "persistent_command"

pageLabel = "persistentCommands"

findPersistentCommandsPage :: WizardRequestContextC s m => [String] -> Pageable -> [Sort] -> m (Page PersistentCommandList)
findPersistentCommandsPage states pageable sort = do
  -- 1. Prepare variables
  do
    let condition =
          case states of
            [] -> ""
            _ -> f' "WHERE persistent_command.state in (%s)" [generateQuestionMarks states]
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    count <- createCountByFn entityName condition states
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT persistent_command.uuid, \
              \       persistent_command.state, \
              \       persistent_command.component, \
              \       persistent_command.function, \
              \       persistent_command.attempts, \
              \       persistent_command.max_attempts, \
              \       persistent_command.created_at, \
              \       persistent_command.updated_at, \
              \       concat(tenant.uuid, '::', \
              \              tenant.name, '::', \
              \              config_look_and_feel.logo_url, '::', \
              \              config_look_and_feel.primary_color, '::', \
              \              tenant.client_url) AS tenant, \
              \       CASE \
              \              WHEN user_entity.uuid IS NOT NULL THEN concat(user_entity.uuid, '::', \
              \                                                            user_entity.first_name, '::', \
              \                                                            user_entity.last_name, '::', \
              \                                                            user_entity.email, '::', \
              \                                                            user_entity.image_url, '::', \
              \                                                            user_entity.affiliation) \
              \       END AS created_by \
              \FROM persistent_command \
              \         LEFT JOIN tenant ON tenant.uuid = persistent_command.tenant_uuid \
              \         LEFT JOIN config_look_and_feel ON config_look_and_feel.tenant_uuid = persistent_command.tenant_uuid \
              \         LEFT JOIN user_entity ON user_entity.uuid = persistent_command.created_by AND user_entity.tenant_uuid = persistent_command.tenant_uuid \
              \${condition} \
              \${sort} \
              \OFFSET ${offset} \
              \LIMIT ${limit}"
              [ ("condition", condition)
              , ("sort", mapSort sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
              ]
    logQuery sql states
    let action conn = query conn sql states
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
