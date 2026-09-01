module Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveJM where

import Data.Aeson

import Shared.Api.Resource.DocumentTemplate.Folder.DocumentTemplateFolderMoveDTO
import Shared.Util.Aeson

instance FromJSON DocumentTemplateFolderMoveDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateFolderMoveDTO where
  toJSON = genericToJSON jsonOptions
