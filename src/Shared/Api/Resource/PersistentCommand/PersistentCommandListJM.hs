module Shared.Api.Resource.PersistentCommand.PersistentCommandListJM where

import Data.Aeson

import Shared.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Shared.Api.Resource.Tenant.TenantSuggestionJM ()
import Shared.Api.Resource.User.UserSuggestionJM ()
import Shared.Model.PersistentCommand.PersistentCommandList
import Shared.Util.Aeson

instance FromJSON PersistentCommandList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PersistentCommandList where
  toJSON = genericToJSON jsonOptions
