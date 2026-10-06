module Shared.Database.Migration.Development.User.UserMigration where

import Shared.Constant.Component
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserGroupDAO
import Shared.Database.DAO.User.UserGroupMembershipDAO
import Shared.Database.DAO.User.UserTokenDAO
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.UserGroups
import Shared.Database.Migration.Development.User.Data.WizardUserTokens
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Cache.ServerCache
import Shared.Model.Context.WizardRequestContext
import Shared.Util.Logger

runMigration :: WizardRequestContextC s m => m ()
runMigration = do
  logInfo _CMP_MIGRATION "(User/User) started"
  deleteUserTokens
  deleteUserGroupMemberships
  deleteUsers
  deleteUserGroups
  insertUserGroup bioGroup
  insertUserGroup plantGroup
  insertUser userSystem
  insertWorkspaceMembership (defaultWorkspaceMembership userSystem)
  insertWorkspaceMembership (differentWorkspaceMembership userSystem)
  insertUser userAlbert
  insertWorkspaceMembership (defaultWorkspaceMembership userAlbert)
  insertUserGroupMembership userAlbertBioGroupMembership
  insertUserToken albertToken
  insertUser userNikola
  insertWorkspaceMembership (defaultWorkspaceMembership userNikola)
  insertUserToken nikolaToken
  insertUserGroupMembership userNikolaBioGroupMembership
  insertUser userIsaac
  insertWorkspaceMembership (defaultWorkspaceMembership userIsaac)
  insertUserToken isaacToken
  insertUser userCharles
  insertWorkspaceMembership (differentWorkspaceMembership userCharles)
  logInfo _CMP_MIGRATION "(User/User) ended"
