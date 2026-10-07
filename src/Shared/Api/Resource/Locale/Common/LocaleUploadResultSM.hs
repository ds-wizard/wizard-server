module Shared.Api.Resource.Locale.Common.LocaleUploadResultSM where

import Data.Swagger

import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Api.Resource.Locale.Common.LocaleUploadResultJM
import Shared.Api.Resource.Locale.LocaleDTO
import Shared.Api.Resource.Locale.LocaleSM ()
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Util.Swagger

instance ToSchema (LocaleUploadResultDTO LocaleDTO) where
  declareNamedSchema proxy =
    withSchemaName "LocaleUploadResult" . withExample (localeDeUploadPreviewDto :: LocaleUploadResultDTO LocaleDTO) $
      genericDeclareNamedSchema (fromAesonOptions localeUploadResultOptions) proxy

instance ToSchema LocaleUploadWarningDTO where
  declareNamedSchema = withExample localeDeReplaceWarningDto . genericDeclareNamedSchema (fromAesonOptions localeUploadWarningOptions)
