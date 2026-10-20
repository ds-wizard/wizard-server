module Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleJM where

import Data.Aeson

import Shared.Api.Resource.MailTemplateLocale.MailTemplateLocaleDTO
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON MailTemplateLocaleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MailTemplateLocaleDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema MailTemplateLocaleDTO where
  jsonSchema = genericJsonSchema jsonOptions
