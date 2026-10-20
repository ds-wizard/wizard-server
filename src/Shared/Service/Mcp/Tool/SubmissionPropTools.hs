module Shared.Service.Mcp.Tool.SubmissionPropTools where

import Data.Aeson
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Proxy

import Shared.Api.Resource.User.UserDTO
import Shared.Api.Resource.User.UserSubmissionPropListJM ()
import Shared.Database.DAO.WizardCommon (runInTransaction)
import Shared.Model.Context.RequestContextHelpers
import Shared.Model.Context.TransactionState
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Mcp.McpTool
import Shared.Model.Mcp.McpToolset
import Shared.Model.User.RolePermission
import Shared.Model.User.UserSubmissionPropList
import Shared.Service.Mcp.Tool.McpToolCommon
import Shared.Service.Mcp.Tool.McpWriteToolCommon
import Shared.Service.User.Profile.UserProfileService
import Shared.Util.JsonSchema
import Shared.Util.Secret

submissionPropTools :: WizardRequestContextC s m => [McpTool m]
submissionPropTools = [setSubmissionPropsTool]

setSubmissionPropsTool :: WizardRequestContextC s m => McpTool m
setSubmissionPropsTool =
  McpTool
    { name = "set_submission_props"
    , title = "Set submission props"
    , description =
        "Replace your values of the props (credentials such as API tokens) of the submission services (get_settings with section submission lists the services and their props), "
          ++ "given per service id; the values of the services you leave out are removed, and "
          ++ secretDescription
    , inputSchema = argumentsSchema arguments
    , outputSchema = Just $ objectSchema ["services"] [("services", arraySchema (jsonSchema (Proxy :: Proxy UserSubmissionPropList)))]
    , annotations = destructiveAnnotations
    , continuable = False
    , permissions = [_MCP_READ_ROLE_PERMISSION, _MCP_WRITE_ROLE_PERMISSION, _MCP_SENSITIVE_ROLE_PERMISSION]
    , toolset = Just AdminMcpToolset
    , transactionState = Transactional
    , run = withArguments arguments setSubmissionProps
    }
  where
    arguments = requiredArgument "props" "The values of the props by service id and prop name" :: Arguments (M.Map String (M.Map String String))

setSubmissionProps :: WizardRequestContextC s m => M.Map String (M.Map String String) -> m Value
setSubmissionProps props =
  runInTransaction $ do
    user <- getCurrentUser
    current <- getUserProfileSubmissionProps user.uuid
    _ <- modifyUserProfileSubmissionProps (fmap (unmaskSubmissionProps current) (M.toList props))
    services <- getUserProfileSubmissionProps user.uuid
    return $ object ["services" .= fmap maskSubmissionProps services]

unmaskSubmissionProps :: [UserSubmissionPropList] -> (String, M.Map String String) -> UserSubmissionPropList
unmaskSubmissionProps current (serviceId, values) =
  let currentValues = maybe M.empty (.values) (L.find ((== serviceId) . (.sId)) current)
   in UserSubmissionPropList serviceId "" (unmaskSecrets currentValues values)

maskSubmissionProps :: UserSubmissionPropList -> UserSubmissionPropList
maskSubmissionProps props = props {values = maskSecrets props.values}
