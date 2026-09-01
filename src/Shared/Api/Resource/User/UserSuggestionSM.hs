module Shared.Api.Resource.User.UserSuggestionSM where

import Data.Swagger

import Shared.Api.Resource.User.UserSuggestionJM ()
import Shared.Database.Migration.Development.User.Data.Users
import Shared.Model.User.UserSuggestion
import Shared.Util.Swagger

instance ToSchema UserSuggestion where
  declareNamedSchema = toSwagger userAlbertSuggestion
