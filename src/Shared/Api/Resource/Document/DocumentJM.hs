module Shared.Api.Resource.Document.DocumentJM where

import Data.Aeson

import Shared.Api.Resource.Document.DocumentDTO
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateFormatSimpleJM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateWithCoordinateJM ()
import Shared.Api.Resource.Project.ProjectSimpleJM ()
import Shared.Api.Resource.Submission.SubmissionJM ()
import Shared.Model.Document.Document
import Shared.Util.Aeson
import Shared.Util.JsonSchema

instance FromJSON DocumentState

instance ToJSON DocumentState

instance JsonSchema DocumentState where
  jsonSchema = genericJsonSchema defaultOptions

instance FromJSON DocumentDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON DocumentDTO where
  toJSON = genericToJSON jsonOptions

instance JsonSchema DocumentDTO where
  jsonSchema = genericJsonSchema jsonOptions
