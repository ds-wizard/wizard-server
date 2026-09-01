module Shared.Database.Migration.Development.PersistentCommand.Data.PersistentCommands where

import Shared.Database.Migration.Development.Tenant.Data.Tenants
import Shared.Database.Migration.Development.User.Data.Users
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.PersistentCommand.PersistentCommandList
import Shared.Util.Date
import Shared.Util.Uuid

command1List :: PersistentCommandList
command1List =
  PersistentCommandList
    { uuid = u' "34493424-ad08-4752-acf4-ac92223dc2f6"
    , state = DonePersistentCommandState
    , component = "component1"
    , function = "function1"
    , attempts = 1
    , maxAttempts = 10
    , tenant = tenantSuggestion
    , createdBy = Just userAlbertSuggestion
    , createdAt = dt' 2018 1 25
    , updatedAt = dt' 2018 1 25
    }
