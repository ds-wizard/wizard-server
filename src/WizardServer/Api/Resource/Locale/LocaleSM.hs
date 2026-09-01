module WizardServer.Api.Resource.Locale.LocaleSM where

import Data.Swagger

import RegistryPublic.Api.Resource.Organization.OrganizationSimpleSM ()
import Shared.Util.Swagger
import WizardServer.Api.Resource.Locale.LocaleDTO
import WizardServer.Api.Resource.Locale.LocaleJM ()
import WizardServer.Database.Migration.Development.Locale.Data.Locales

instance ToSchema LocaleDTO where
  declareNamedSchema = toSwagger localeNlDto
