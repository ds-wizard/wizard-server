module Shared.Api.Resource.DocumentTemplate.DocumentTemplateWithCoordinateJM where

import Data.Aeson

import Shared.Model.DocumentTemplate.DocumentTemplateWithCoordinate
import Shared.Util.Aeson

instance FromJSON DocumentTemplateWithCoordinate where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentTemplateWithCoordinate where
  toJSON = genericToJSON jsonOptions
