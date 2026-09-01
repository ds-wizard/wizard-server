module WizardServer.Service.User.Tour.TourService where

import Control.Monad (void)
import qualified Data.UUID as U

import Shared.Database.DAO.User.UserTourDAO
import Shared.Model.Context.WizardRequestContext

deleteTours :: WizardRequestContextC s m => U.UUID -> m ()
deleteTours userUuid = void $ deleteToursByUserUuid userUuid
