module Shared.Service.User.Tour.TourMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Model.User.UserTour

toUserTour :: U.UUID -> String -> U.UUID -> UTCTime -> UserTour
toUserTour userUuid tourId tenantUuid createdAt = UserTour {..}
