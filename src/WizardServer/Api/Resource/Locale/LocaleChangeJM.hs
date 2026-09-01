module WizardServer.Api.Resource.Locale.LocaleChangeJM where

import Data.Aeson

import Shared.Api.Resource.Locale.LocaleChangeDTO
import Shared.Util.Aeson

instance FromJSON LocaleChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleChangeDTO where
  toJSON = genericToJSON jsonOptions
