module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftListJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateDraftList
import Shared.Util.Aeson

instance FromJSON DocumentTemplateDraftList where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftList where
  toJSON = genericToJSON jsonOptions
