module WizardServer.Database.DAO.User.UserGroupDAO where

import Shared.Api.Resource.User.UserDTO
import Shared.Database.DAO.User.UserGroupDAO
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Context.AclContext
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.WizardRequestContext
import WizardServer.Database.Mapping.User.UserGroupSuggestion ()
import WizardServer.Model.User.UserGroupSuggestion

entityName = "user_group"

pageLabel = "userGroups"

findUserGroupSuggestionsPage :: WizardRequestContextC s m => Maybe String -> Pageable -> [Sort] -> m (Page UserGroupSuggestion)
findUserGroupSuggestionsPage mQuery pageable sort = do
  currentUser <- getCurrentUser
  hasPermission <- hasPermission _USERS_MANAGE_ROLE_PERMISSION
  createFindUserGroupPage
    "ug.uuid, ug.name, ug.description, ug.private"
    currentUser.uuid
    hasPermission
    mQuery
    ""
    pageable
    sort
