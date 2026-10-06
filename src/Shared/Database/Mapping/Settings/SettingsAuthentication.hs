module Shared.Database.Mapping.Settings.SettingsAuthentication where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import Shared.Model.Settings.Settings

instance ToRow SettingsAuthentication where
  toRow SettingsAuthentication {..} =
    [ toField registrationEnabled
    , toField nonAdminLoginEnabled
    , toField sessionExpiration
    , toField userEmailLinkExpiration
    , toField twoFactorAuth.enabled
    , toField twoFactorAuth.codeLength
    , toField twoFactorAuth.expiration
    ]

instance FromRow SettingsAuthentication where
  fromRow = do
    registrationEnabled <- field
    nonAdminLoginEnabled <- field
    sessionExpiration <- field
    userEmailLinkExpiration <- field
    twoFactorAuth <- SettingsAuthenticationTwoFactorAuth <$> field <*> field <*> field
    return SettingsAuthentication {..}
