module Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleJM where

import Data.Aeson

import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO
import Shared.Util.Aeson

instance FromJSON MailTemplateLocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MailTemplateLocaleDTO where
  toJSON = genericToJSON jsonOptions
