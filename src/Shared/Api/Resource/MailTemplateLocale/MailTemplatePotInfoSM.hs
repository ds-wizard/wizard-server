module Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoSM where

import Data.Swagger

import Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoJM
import Shared.Database.Migration.Development.MailTemplateLocale.Data.MailTemplateLocales
import Shared.Util.Swagger

instance ToSchema MailTemplatePotInfoDTO where
  declareNamedSchema = toSwagger mailTemplatePotInfoDto

instance ToSchema MailTemplatePotSource where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions mailTemplatePotSourceOptions)
