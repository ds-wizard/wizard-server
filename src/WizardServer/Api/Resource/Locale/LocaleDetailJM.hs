module WizardServer.Api.Resource.Locale.LocaleDetailJM where

import Data.Aeson

import Shared.Api.Resource.Registry.RegistryOrganizationJM ()
import Shared.Api.Resource.Version.VersionJM ()
import Shared.Util.Aeson
import WizardServer.Api.Resource.Locale.LocaleDetailDTO

instance FromJSON LocaleDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleDetailDTO where
  toJSON = genericToJSON jsonOptions
