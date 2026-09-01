module Shared.Service.User.UserAudit where

import qualified Data.Map.Strict as M
import Data.UUID as U

import Shared.Api.Resource.User.UserDTO
import Shared.Model.Context.WizardRequestContext
import Shared.Service.Audit.AuditService

auditUserCreateByAdmin :: WizardRequestContextC s m => UserDTO -> m ()
auditUserCreateByAdmin userDto =
  logAuditWithBody
    "user"
    "createByAdmin"
    (U.toString userDto.uuid)
    (M.fromList [("firstName", userDto.firstName), ("lastName", userDto.lastName), ("email", userDto.email)])

auditUserCreateByCommand :: WizardRequestContextC s m => UserDTO -> m ()
auditUserCreateByCommand userDto =
  logAuditWithBody
    "user"
    "createByCommand"
    (U.toString userDto.uuid)
    (M.fromList [("firstName", userDto.firstName), ("lastName", userDto.lastName), ("email", userDto.email)])
