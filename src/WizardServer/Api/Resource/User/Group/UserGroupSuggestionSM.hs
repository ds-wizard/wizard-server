module WizardServer.Api.Resource.User.Group.UserGroupSuggestionSM where

import Data.Swagger

import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Util.Swagger
import WizardServer.Api.Resource.User.Group.UserGroupSuggestionJM ()
import WizardServer.Model.User.UserGroupSuggestion
import WizardServer.Service.User.Group.UserGroupMapper

instance ToSchema UserGroupSuggestion where
  declareNamedSchema = toSwagger (toSuggestion bioGroup)
