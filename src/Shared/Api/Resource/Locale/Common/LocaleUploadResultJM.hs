module Shared.Api.Resource.Locale.Common.LocaleUploadResultJM where

import Data.Aeson

import Shared.Api.Resource.Locale.Common.LocaleUploadResultDTO
import Shared.Util.Aeson
import Shared.Util.String (stripSuffixIfExists)

instance FromJSON a => FromJSON (LocaleUploadResultDTO a) where
  parseJSON = genericParseJSON localeUploadResultOptions

instance ToJSON a => ToJSON (LocaleUploadResultDTO a) where
  toJSON = genericToJSON localeUploadResultOptions

instance FromJSON LocaleUploadWarningDTO where
  parseJSON = genericParseJSON localeUploadWarningOptions

instance ToJSON LocaleUploadWarningDTO where
  toJSON = genericToJSON localeUploadWarningOptions

localeUploadResultOptions :: Options
localeUploadResultOptions = (jsonOptionsWithTypeField "type") {constructorTagModifier = stripSuffixIfExists "LocaleUploadResultDTO"}

localeUploadWarningOptions :: Options
localeUploadWarningOptions = (jsonOptionsWithTypeField "type") {constructorTagModifier = stripSuffixIfExists "LocaleUploadWarningDTO"}
