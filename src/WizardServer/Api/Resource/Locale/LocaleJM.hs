module WizardServer.Api.Resource.Locale.LocaleJM where

import Data.Aeson

import RegistryPublic.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.Aeson
import WizardServer.Api.Resource.Locale.LocaleDTO

instance FromJSON LocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleDTO where
  toJSON = genericToJSON jsonOptions
