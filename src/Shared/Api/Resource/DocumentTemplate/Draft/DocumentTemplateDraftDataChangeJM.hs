module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeDTO
import Shared.Util.Aeson

instance FromJSON DocumentTemplateDraftDataChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftDataChangeDTO where
  toJSON = genericToJSON jsonOptions
