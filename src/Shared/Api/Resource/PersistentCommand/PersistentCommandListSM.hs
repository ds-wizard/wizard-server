module Shared.Api.Resource.PersistentCommand.PersistentCommandListSM where

import Data.Swagger

import Shared.Api.Resource.PersistentCommand.PersistentCommandListJM ()
import Shared.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Shared.Api.Resource.Tenant.TenantSuggestionSM ()
import Shared.Api.Resource.User.UserSuggestionSM ()
import Shared.Database.Migration.Development.PersistentCommand.Data.PersistentCommands
import Shared.Model.PersistentCommand.PersistentCommandList
import Shared.Util.Swagger

instance ToSchema PersistentCommandList where
  declareNamedSchema = toSwagger command1List
