module WizardServer.Api.Resource.Locale.LocaleChangeSM where

import Data.Swagger

import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Util.Swagger
import WizardServer.Api.Resource.Locale.LocaleChangeJM ()
import WizardServer.Database.Migration.Development.Locale.Data.Locales

instance ToSchema LocaleChangeDTO where
  declareNamedSchema = toSwagger localeNlChangeDto
