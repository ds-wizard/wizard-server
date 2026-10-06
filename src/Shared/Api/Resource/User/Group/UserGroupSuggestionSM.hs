module Shared.Api.Resource.User.Group.UserGroupSuggestionSM where

import Data.Swagger

import Shared.Api.Resource.User.Group.UserGroupSuggestionJM ()
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Model.User.UserGroupSuggestion
import Shared.Service.User.Group.UserGroupMapper
import Shared.Util.Swagger

instance ToSchema UserGroupSuggestion where
  declareNamedSchema = toSwagger (toSuggestion bioGroup)
