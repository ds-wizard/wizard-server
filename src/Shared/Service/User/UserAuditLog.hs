module Shared.Service.User.UserAuditLog where

import Shared.Api.Resource.User.UserDTO
import Shared.Model.AuditLog.AuditLogCatalog
import Shared.Model.AuditLog.AuditLogEvent
import Shared.Model.AuditLog.Data.AuditLogRef
import Shared.Model.AuditLog.Data.UserAuditLogData
import Shared.Model.Context.WizardRequestContext
import Shared.Model.User.RoleSimple
import Shared.Service.AuditLog.AuditLogService

al_createUserByAdmin :: WizardRequestContextC s m => UserDTO -> m ()
al_createUserByAdmin user =
  logAuditLog
    usersCreateByAdmin
    (entityOf user.uuid user.email)
    AuditLogUserData
      { firstName = user.firstName
      , lastName = user.lastName
      , affiliation = user.affiliation
      , role = AuditLogRef {uuid = user.role.uuid, name = user.role.name}
      }
