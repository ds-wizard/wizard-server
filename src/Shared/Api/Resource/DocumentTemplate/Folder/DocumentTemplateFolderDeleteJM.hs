module Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderDeleteDTO
import Shared.Util.Aeson

instance FromJSON DocumentTemplateFolderDeleteDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFolderDeleteDTO where
  toJSON = genericToJSON jsonOptions
