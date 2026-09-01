module WizardServer.Api.Resource.Locale.LocaleDetailSM where

import Data.Swagger

import Shared.Api.Resource.Registry.RegistryOrganizationSM ()
import Shared.Api.Resource.Version.VersionSM ()
import Shared.Util.Swagger
import WizardServer.Api.Resource.Locale.LocaleDetailDTO
import WizardServer.Api.Resource.Locale.LocaleDetailJM ()
import WizardServer.Database.Migration.Development.Locale.Data.Locales

instance ToSchema LocaleDetailDTO where
  declareNamedSchema = toSwagger localeNlDetailDto
