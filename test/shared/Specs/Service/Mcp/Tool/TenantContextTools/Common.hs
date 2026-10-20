module Specs.Service.Mcp.Tool.TenantContextTools.Common where

import Data.Aeson
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import Network.HTTP.Types
import Network.Wai.Test (SResponse)
import Test.Hspec.Wai hiding (shouldRespondWith)
import Text.Printf (printf)

import Shared.Api.Resource.Mcp.TenantContext.TenantContextDTO
import Shared.Database.Migration.Development.Settings.Data.Settings
import Shared.Database.Migration.Development.User.Data.Roles
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Database.Migration.Development.Workspace.Data.Workspaces
import Shared.Model.Config.SimpleFeature
import Shared.Model.Settings.Settings
import Shared.Model.User.Role
import Shared.Model.User.User
import Shared.Model.Workspace.Workspace
import Shared.Util.Uuid

import SharedTest.Specs.Api.Common
import SharedTest.Specs.Api.Mcp
import Specs.Api.Handler.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
albertContext :: Bool -> [TenantContextWorkspaceDTO] -> TenantContextDTO
albertContext multiWorkspace workspaces =
  TenantContextDTO
    { user =
        TenantContextUserDTO
          { uuid = userAlbert.uuid
          , firstName = userAlbert.firstName
          , lastName = userAlbert.lastName
          , email = userAlbert.email
          , role = userAlbert.role.name
          }
    , permissions = L.sort . L.nub $ userAlbert.role.permissions
    , multiWorkspace = multiWorkspace
    , workspaceUuid = Nothing
    , workspaces = workspaces
    , usage = Nothing
    , settings =
        TenantContextSettingsDTO
          { dashboardAndMenu = settingsDashboardAndMenu
          , projects = settingsProjects
          , support = settingsSupport
          , submission = SimpleFeature settingsSubmission.enabled
          }
    }

defaultWorkspaceContext :: TenantContextWorkspaceDTO
defaultWorkspaceContext =
  TenantContextWorkspaceDTO
    { uuid = defaultWorkspace.uuid
    , name = defaultWorkspace.name
    , description = defaultWorkspace.description
    , role = Just defaultWorkspaceUserRole.name
    }

manyWorkspaces :: [Workspace]
manyWorkspaces =
  [ ( defaultWorkspace
        { uuid = u' (printf "00000000-0000-4000-8000-%012d" index)
        , name = printf "Workspace %02d" index
        , description = Just (replicate 1000 'x')
        , defaultRoleUuid = Nothing
        }
        :: Workspace
    )
  | index <- [1 .. 60 :: Int]
  ]

workspaceNames :: Value -> [Value]
workspaceNames content =
  case valueField "workspaces" content of
    Just (Array workspaces) -> mapMaybe (valueField "name") (V.toList workspaces)
    _ -> []

tenantContextOutputSchemaOf :: SResponse -> Maybe Value
tenantContextOutputSchemaOf response = valueField "outputSchema" =<< L.find ((== Just (String "get_tenant_context")) . valueField "name") (toolsOf response)

requestTenantContextPages :: Value -> Int -> WaiSession st [SResponse]
requestTenantContextPages arguments remaining = do
  response <- request methodPost "/api/mcp" [reqAuthHeader, reqCtHeader] (toolCallBody "get_tenant_context" arguments)
  case valueField "cursor" =<< structuredContentOf response of
    Just cursor | remaining > 1 -> (response :) <$> requestTenantContextPages (object ["cursor" .= cursor]) (remaining - 1)
    _ -> return [response]
