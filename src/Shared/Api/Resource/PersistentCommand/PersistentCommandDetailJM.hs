module Shared.Api.Resource.PersistentCommand.PersistentCommandDetailJM where

import Data.Aeson

import Shared.Api.Resource.PersistentCommand.PersistentCommandDetailDTO
import Shared.Api.Resource.PersistentCommand.PersistentCommandJM ()
import Shared.Api.Resource.Tenant.WizardTenantJM ()
import Shared.Api.Resource.User.UserSuggestionJM ()
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON PersistentCommandDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PersistentCommandDetailDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema PersistentCommandDetailDTO where
  jsonSchema = genericJsonSchema jsonOptions
