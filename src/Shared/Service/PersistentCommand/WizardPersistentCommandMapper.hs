module Shared.Service.PersistentCommand.WizardPersistentCommandMapper where

import qualified Data.UUID as U

import Shared.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Shared.Api.Resource.Tenant.TenantDTO
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Model.User.User
import qualified Shared.Service.User.WizardUserMapper as U_Mapper

toDetailDTO :: PersistentCommand U.UUID -> Maybe User -> TenantDTO -> PersistentCommandDetailDTO
toDetailDTO command user tenant =
  PersistentCommandDetailDTO
    { uuid = command.uuid
    , state = command.state
    , component = command.component
    , function = command.function
    , body = command.body
    , lastTraceUuid = command.lastTraceUuid
    , lastErrorMessage = command.lastErrorMessage
    , attempts = command.attempts
    , maxAttempts = command.maxAttempts
    , tenant = tenant
    , createdBy = fmap (U_Mapper.toSuggestion . U_Mapper.toSimple) user
    , createdAt = command.createdAt
    , updatedAt = command.updatedAt
    }
