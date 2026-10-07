module Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleSM where

import Data.Swagger

import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.Locale.Common.LocaleUploadResultJM
import Shared.Api.Resource.Locale.Common.LocaleUploadResultSM ()
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO
import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleJM ()
import Shared.Database.Migration.Development.MailTemplateLocale.Data.MailTemplateLocales
import Shared.Util.Swagger

instance ToSchema MailTemplateLocaleDTO where
  declareNamedSchema = toSwagger mailTemplateLocaleDeDto

instance ToSchema (LocaleUploadResultDTO MailTemplateLocaleDTO) where
  declareNamedSchema proxy =
    withSchemaName "MailTemplateLocaleUploadResult" . withExample mailTemplateLocaleDeUploadPreviewDto $
      genericDeclareNamedSchema (fromAesonOptions localeUploadResultOptions) proxy
