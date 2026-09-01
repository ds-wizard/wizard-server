module Shared.Service.User.UserUtil where

import qualified Crypto.PasswordStore as PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe (isJust, isNothing)

import Shared.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Tenant.Config.WizardTenantConfig
import Shared.Model.User.User
import Shared.Model.User.UserSubmissionPropEM ()
import Shared.Util.String (splitOn)

verifyPassword :: String -> String -> Bool
verifyPassword incomingPassword passwordHashFromDB =
  case splitOn ":" passwordHashFromDB of
    ["pbkdf1", hashFromDB] -> PasswordStore.verifyPassword (BS.pack incomingPassword) (BS.pack hashFromDB)
    ["pbkdf2", hashFromDB] ->
      PasswordStore.verifyPasswordWith PasswordStore.pbkdf2 (2 ^) (BS.pack incomingPassword) (BS.pack hashFromDB)
    _ -> False

isConsentRequired :: WizardRequestContextC s m => Maybe User -> m Bool
isConsentRequired mUserFromDb = do
  tcPrivacyAndSupport <- findTenantConfigPrivacyAndSupport
  return $ isNothing mUserFromDb && (isJust tcPrivacyAndSupport.privacyUrl || isJust tcPrivacyAndSupport.termsOfServiceUrl)
