module Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Shared.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.Util.Aeson

instance FromJSON DocumentTemplateDraftChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateDraftChangeDTO where
  toJSON = genericToJSON jsonOptions
