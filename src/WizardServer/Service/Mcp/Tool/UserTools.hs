module WizardServer.Service.Mcp.Tool.UserTools where

import Shared.Api.Resource.User.UserChangeDTO
import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserJM ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Service.Mcp.Tool.McpExecutableServices
import Shared.Service.Mcp.Tool.RoleTools
import Shared.Service.Mcp.Tool.UserTools
import Shared.Service.User.UserService (getUserDetailById, getUsersPage, modifyUser)
import Shared.Service.User.WizardUserMapper (toUserChangeDTO)
import WizardServer.Api.Resource.User.UserChangeJM ()
import WizardServer.Service.User.Role.RoleService (createRoleWithUuid, deleteRole, getRolesPage, modifyRole)
import WizardServer.Service.User.UserInvitationService

userTools :: WizardRequestContextC s m => McpExecutableServices m -> [McpTool m]
userTools services =
  [ searchUsersTool getUsersPage
  , updateUserTool getUserDetailById toUserChangeDTO modifyUser services.findUserLoginProviders
  , inviteUsersTool inviteUsers
  , deleteUserTool getUserDetailById services
  , listRolesTool getRolesPage
  ]
    ++ roleTools createRoleWithUuid modifyRole deleteRole
