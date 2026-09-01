module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson

instance FromJSON DocumentTemplateDraftCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftCreateDTO where
  toJSON = genericToJSON jsonOptions
