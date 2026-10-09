module Shared.Api.Resource.Document.DocumentPreviewAcceptedJM where

import Data.Aeson

import Shared.Api.Resource.Document.DocumentPreviewAcceptedDTO
import Shared.Util.Aeson

instance FromJSON DocumentPreviewAcceptedDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentPreviewAcceptedDTO where
  toJSON = genericToJSON jsonOptions
