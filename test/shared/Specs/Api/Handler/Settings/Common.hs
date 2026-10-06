module Specs.Api.Handler.Settings.Common where

import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Only (..), query_)
import Network.HTTP.Types
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Api.Resource.Settings.SettingsDTO
import Shared.Api.Resource.Settings.SettingsJM ()
import Shared.Constant.Workspace
import Shared.Database.DAO.User.RoleDAO
import Shared.Database.DAO.WizardCommon
import Shared.Database.DAO.Workspace.WorkspaceMembershipDAO
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.User.Role
import Shared.Model.User.RolePermission
import Shared.Model.User.User
import Test.Hspec

import SharedTest.Specs.Api.Common
import Specs.Api.Handler.Common
import Specs.Api.Handler.Workspace.Common
import Specs.Common

tenantUrl :: String -> BS.ByteString
tenantUrl section = BS.pack $ "/api/settings/" ++ section ++ "?tenant=true"

workspaceUrl :: String -> BS.ByteString
workspaceUrl section = BS.pack $ "/api/settings/" ++ section ++ "?w=" ++ U.toString defaultWorkspaceUuid

noScopeUrl :: String -> BS.ByteString
noScopeUrl section = BS.pack $ "/api/settings/" ++ section

settingsBody :: ToJSON a => a -> Maybe Bool -> BSL.ByteString
settingsBody value overrideAllowed = encode (SettingsDTO value overrideAllowed Nothing)

expectJson expStatus expDto response =
  response `shouldRespondWith` ResponseMatcher {matchHeaders = resCtHeader : resCorsHeaders, matchStatus = expStatus, matchBody = bodyEquals (encode expDto)}

expectStatus expStatus response = response `shouldRespondWith` ResponseMatcher {matchHeaders = [], matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}

useWorkspaceUserRole requestContext = do
  enableMultiWorkspace requestContext
  demoteToResearcher requestContext userAlbert
  runInContextIO (updateWorkspaceMembershipRole defaultWorkspaceUuid userAlbert.uuid defaultWorkspaceUserRole.uuid) requestContext

useWorkspaceSettingsManager requestContext = do
  useWorkspaceUserRole requestContext
  runInContextIO (updateRoleByUuid (defaultWorkspaceUserRole {permissions = [_SETTINGS_MANAGE_ROLE_PERMISSION]} :: Role)) requestContext

countWorkspaceRows table requestContext = do
  counts <- getOneFromDB (runDB (\conn -> query_ conn (fromString ("SELECT count(*) FROM " ++ table ++ " WHERE workspace_uuid IS NOT NULL")))) requestContext
  return (sum (fmap fromOnly counts) :: Int)

reqHeaders :: [Header]
reqHeaders = [reqAuthHeader, reqCtHeader]

compareDtos resDto expDto = liftIO $ resDto `shouldBe` expDto
