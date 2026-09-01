module WizardLib.Public.Database.DAO.User.UserTourDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import WizardLib.Public.Database.Mapping.User.UserTour ()
import WizardLib.Public.Model.User.UserTour

entityName = "user_tour"

findUserToursByUserUuid :: AppContextC s sc m => U.UUID -> m [String]
findUserToursByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  let sql =
        fromString $
          f'
            "SELECT tour_id \
            \FROM %s \
            \WHERE tenant_uuid = ? AND user_uuid = ?"
            [table]
  let params = [U.toString tenantUuid, U.toString userUuid]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  return . concat $ entities

insertUserTour :: AppContextC s sc m => UserTour -> m Int64
insertUserTour userTour = do
  table <- tableName entityName
  createInsertFn table userTour

deleteTours :: AppContextC s sc m => m Int64
deleteTours = do
  table <- tableName entityName
  createDeleteEntitiesFn table

deleteToursByUserUuid :: AppContextC s sc m => U.UUID -> m Int64
deleteToursByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  table <- tableName entityName
  createDeleteEntityByFn table [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]
