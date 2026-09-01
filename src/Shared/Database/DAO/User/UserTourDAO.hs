module Shared.Database.DAO.User.UserTourDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Database.DAO.Common
import Shared.Database.Mapping.User.UserTour ()
import Shared.Model.Context.RequestContext
import Shared.Model.User.UserTour
import Shared.Util.String

entityName = "user_tour"

findUserToursByUserUuid :: RequestContextC s sc m => U.UUID -> m [String]
findUserToursByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "SELECT tour_id \
            \FROM %s \
            \WHERE tenant_uuid = ? AND user_uuid = ?"
            [entityName]
  let params = [U.toString tenantUuid, U.toString userUuid]
  logQuery sql params
  let action conn = query conn sql params
  entities <- runDB action
  return . concat $ entities

insertUserTour :: RequestContextC s sc m => UserTour -> m Int64
insertUserTour userTour = do
  createInsertFn entityName userTour

deleteTours :: RequestContextC s sc m => m Int64
deleteTours = do
  createDeleteEntitiesFn entityName

deleteToursByUserUuid :: RequestContextC s sc m => U.UUID -> m Int64
deleteToursByUserUuid userUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("user_uuid", U.toString userUuid)]
