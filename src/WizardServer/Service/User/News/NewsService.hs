module WizardServer.Service.User.News.NewsService where

import Control.Monad (void)
import qualified Data.UUID as U

import Shared.Database.DAO.User.UserDAO
import Shared.Model.Context.WizardRequestContext

updateNews :: WizardRequestContextC s m => U.UUID -> String -> m ()
updateNews userUuid lastSeenNewsId = void $ updateUserLastSeenNewsIdUuid userUuid lastSeenNewsId
