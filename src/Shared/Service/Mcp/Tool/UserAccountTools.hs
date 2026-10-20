module Shared.Service.Mcp.Tool.UserAccountTools where

import Data.Aeson
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as U

import Shared.Constant.User
import Shared.Database.DAO.User.UserDAO
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Shared.Service.Mcp.Tool.McpConfirmation
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Util.JsonSchema

userAccountTools :: WizardRequestContextC s m => (U.UUID -> String -> m ()) -> (String -> m ()) -> [McpTool m]
userAccountTools setPassword sendPasswordLink = [setUserPasswordTool setPassword, sendUserEmailLinkTool sendPasswordLink]

setUserPasswordTool :: WizardRequestContextC s m => (U.UUID -> String -> m ()) -> McpTool m
setUserPasswordTool setPassword =
  McpTool
    { name = "set_user_password"
    , title = "Set user password"
    , description =
        "Set a new password of a user, who logs in with it from then on. "
          ++ confirmationDescription
    , inputSchema = argumentsSchema (confirmationArguments arguments)
    , outputSchema = Just $ confirmationOutputSchema (objectSchema [] [])
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = accountPermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withConfirmation "set_user_password" arguments (uncurry (preparePassword setPassword))
    }
  where
    arguments =
      (,)
        <$> userUuidArgument
        <*> requiredArgument "password" "The new password"
        :: Arguments (U.UUID, String)

preparePassword :: WizardRequestContextC s m => (U.UUID -> String -> m ()) -> U.UUID -> String -> m (McpConfirmation m)
preparePassword setPassword userUuid password = do
  user <- findUserByUuid userUuid
  return
    McpConfirmation
      { summary = "Sets a new password of " ++ userLabel user ++ "; their current password stops working"
      , revision = iso8601Show user.updatedAt
      , perform = object [] <$ setPassword userUuid password
      }

sendUserEmailLinkTool :: WizardRequestContextC s m => (String -> m ()) -> McpTool m
sendUserEmailLinkTool sendPasswordLink =
  McpTool
    { name = "send_user_email_link"
    , title = "Send user e-mail link"
    , description =
        "Send a user an e-mail with a link to set up or reset their password, when the organization lets them log in with a password. "
          ++ confirmationDescription
    , inputSchema = argumentsSchema (confirmationArguments userUuidArgument)
    , outputSchema = Just $ confirmationOutputSchema (objectSchema [] [])
    , annotations = confirmationAnnotations
    , continuable = False
    , permissions = accountPermissions
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withConfirmation "send_user_email_link" userUuidArgument (preparePasswordLink sendPasswordLink)
    }

preparePasswordLink :: WizardRequestContextC s m => (String -> m ()) -> U.UUID -> m (McpConfirmation m)
preparePasswordLink sendPasswordLink userUuid = do
  user <- findUserByUuid userUuid
  return
    McpConfirmation
      { summary = passwordLinkSummary user
      , revision = user.email
      , perform = object [] <$ sendPasswordLink user.email
      }

passwordLinkSummary :: User -> String
passwordLinkSummary user
  | user.passwordHash == noPasswordHash = "Sends " ++ userLabel user ++ ", who has no password yet, an e-mail with a link to set one up"
  | otherwise = "Sends " ++ userLabel user ++ " an e-mail with a link to set a new password; their current password keeps working until they use it"

userLabel :: User -> String
userLabel user = user.firstName ++ " " ++ user.lastName ++ " (" ++ user.email ++ ")"

accountPermissions :: [String]
accountPermissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION, _USERS_MANAGE_ROLE_PERMISSION]

userUuidArgument :: Arguments U.UUID
userUuidArgument = requiredArgument "user_uuid" "The user"
