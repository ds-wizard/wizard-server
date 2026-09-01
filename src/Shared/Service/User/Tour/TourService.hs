module Shared.Service.User.Tour.TourService where

import Control.Monad (void)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.User.UserTourDAO
import Shared.Model.Context.RequestContext
import Shared.Service.User.Tour.TourMapper

createOrUpdateTour :: RequestContextC s sc m => U.UUID -> String -> m ()
createOrUpdateTour userUuid tourId = do
  tenantUuid <- asks (.tenantUuid')
  now <- liftIO getCurrentTime
  let tour = toUserTour userUuid tourId tenantUuid now
  void $ insertUserTour tour
