module Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoJM where

import Data.Aeson

import Shared.Api.Resource.MailTemplateLocale.MailTemplatePotInfoDTO
import Shared.Util.Aeson
import Shared.Util.String (lowerFirst, stripSuffixIfExists)

instance FromJSON MailTemplatePotInfoDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON MailTemplatePotInfoDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON MailTemplatePotSource where
  parseJSON = genericParseJSON mailTemplatePotSourceOptions

instance ToJSON MailTemplatePotSource where
  toJSON = genericToJSON mailTemplatePotSourceOptions

mailTemplatePotSourceOptions :: Options
mailTemplatePotSourceOptions = jsonOptions {constructorTagModifier = lowerFirst . stripSuffixIfExists "MailTemplatePotSource"}
