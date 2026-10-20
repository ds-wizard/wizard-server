module Specs.Service.Mcp.Tool.OpenIdClientTools.Common where

import Control.Monad (void)
import qualified Data.UUID as U

import Shared.Constant.Tenant
import Shared.Database.DAO.User.UserDAO
import Shared.Database.DAO.User.UserOpenIdIdentityDAO
import Shared.Database.Migration.Development.OpenId.Data.OpenIdClients
import Shared.Database.Migration.Development.User.Data.WizardUsers
import Shared.Model.OpenId.OpenIdClient
import Shared.Model.OpenId.OpenIdClientStyle
import Shared.Model.User.User
import Shared.Model.User.UserOpenIdIdentity
import Shared.Util.Date
import Shared.Util.Uuid

import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
newClientUuid :: U.UUID
newClientUuid = u' "6c5d4e3f-2a1b-4c0d-9e8f-7a6b5c4d3e2f"

newClient :: OpenIdClient
newClient =
  defaultOpenIdClient
    { uuid = newClientUuid
    , name = "Keycloak"
    , url = "https://keycloak.example.com"
    , clientId = "wizard"
    , clientSecret = "k3ycl0ak"
    , parameters = []
    , style = OpenIdClientStyle Nothing Nothing Nothing
    , registrationEnabled = True
    , scopeProfile = True
    , scopeEmail = True
    }

albertIdentity :: UserOpenIdIdentity
albertIdentity =
  UserOpenIdIdentity
    { uuid = u' "7d6e5f4a-3b2c-4d1e-8f0a-9b8c7d6e5f4a"
    , externalId = userAlbert.email
    , externalLabel = Nothing
    , userUuid = userAlbert.uuid
    , providerUuid = defaultOpenIdClient.uuid
    , tenantUuid = defaultTenantUuid
    , createdAt = dt' 2024 1 1
    }

runAlbertOpenIdLoginOnly requestContext = do
  runInContextIO (void $ updateUserPasswordByUuid userAlbert.uuid "no-hash" (dt' 2024 1 1)) requestContext
  runInContextIO (void $ insertUserOpenIdIdentity albertIdentity) requestContext
